{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run a program in a pty: read its output, send it keys, check its CPU and
-- memory. Linux only (reads @/proc@).
module Pty
  ( Session
  , withSession
  , drain
  , press
  , waitForOutput
  , cpuSeconds
  , Memory (..)
  , memory
  , bytesWritten
  , seconds
  ) where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Foreign (Ptr, alloca, nullPtr, peek, withArray0, withMany)
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import System.IO
import System.Posix.IO (fdToHandle)
import System.Posix.Process (getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (Fd (..), ProcessID)

foreign import ccall unsafe "layoutz_bench_spawn"
  c_spawn :: Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "layoutz_bench_clock_ticks"
  c_clockTicks :: IO CLong

data Session = Session
  { sPid     :: ProcessID
  , sTerm    :: Handle      -- master side
  , sWritten :: IORef Int   -- total bytes read
  }

-- | Run @program args@ on a @rows@ x @cols@ pty. Sends Ctrl-C at the end,
-- then SIGKILL in case it didn't quit.
withSession :: (Int, Int) -> FilePath -> [String] -> (Session -> IO a) -> IO a
withSession (rows, cols) program args = bracket start stop
  where
    start = do
      (pid, fd) <- spawn
      term <- fdToHandle fd
      hSetBinaryMode term True
      hSetBuffering term NoBuffering
      Session pid term <$> newIORef 0

    spawn =
      withMany withCString (program : args) $ \cargs ->
        withArray0 nullPtr cargs $ \argv ->
          alloca $ \masterPtr -> do
            pid <- c_spawn argv (fromIntegral rows) (fromIntegral cols) masterPtr
            when (pid < 0) $ ioError (userError ("could not start " <> program))
            master <- peek masterPtr
            pure (fromIntegral pid, Fd master)

    stop s = do
      press s (BC.pack "\ETX")
      drain s 0.3
      signalProcess sigKILL (sPid s) `catch` ignore
      void (getProcessStatus True False (sPid s)) `catch` ignore
      hClose (sTerm s) `catch` ignore

    ignore :: IOException -> IO ()
    ignore _ = pure ()

-- | Keep reading for @secs@ seconds.
drain :: Session -> Double -> IO ()
drain s secs = do
  deadline <- (+ secs) <$> seconds
  let loop = do
        now <- seconds
        let left = deadline - now
        when (left > 0) $ do
          more <- readSome s left
          when more loop
  loop

-- | True if anything shows up within @secs@.
waitForOutput :: Session -> Double -> IO Bool
waitForOutput = readSome

press :: Session -> B.ByteString -> IO ()
press s keys = B.hPut (sTerm s) keys `catch` \(_ :: IOException) -> pure ()

-- | Read one chunk if one comes within @secs@. False on timeout or EOF.
readSome :: Session -> Double -> IO Bool
readSome s secs = do
  ready <- hWaitForInput (sTerm s) (max 1 (round (secs * 1000)))
  if not ready
    then pure False
    else do
      chunk <- B.hGetSome (sTerm s) 65536
      modifyIORef' (sWritten s) (+ B.length chunk)
      -- layoutz asks for the cursor position to get the screen size
      when (BC.pack "\ESC[6n" `B.isInfixOf` chunk) $
        press s (BC.pack "\ESC[60;200R")
      pure (not (B.null chunk))
  `catch` \(_ :: IOException) -> pure False

-- | utime + stime so far.
cpuSeconds :: Session -> IO Double
cpuSeconds s = do
  stat <- readFile ("/proc/" <> show (sPid s) <> "/stat")
  ticks <- c_clockTicks
  -- comm can have spaces in it, so split after the closing ")"
  let fields = words (reverse (takeWhile (/= ')') (reverse stat)))
      user   = read (fields !! 11)
      system = read (fields !! 12)
  length stat `seq` pure ((user + system) / fromIntegral ticks)

-- | RSS now and peak RSS, in MiB.
data Memory = Memory { residentMiB :: Double, peakMiB :: Double }

memory :: Session -> IO Memory
memory s = do
  status <- lines <$> readFile ("/proc/" <> show (sPid s) <> "/status")
  let kib key = sum [ read n | l <- status, (k : n : _) <- [words l], k == key ] :: Double
      now     = kib "VmRSS:"
      peak    = kib "VmHWM:"
  length status `seq` pure (Memory (now / 1024) (peak / 1024))

bytesWritten :: Session -> IO Int
bytesWritten = readIORef . sWritten

seconds :: IO Double
seconds = (/ 1e9) . fromIntegral <$> getMonotonicTimeNSec
