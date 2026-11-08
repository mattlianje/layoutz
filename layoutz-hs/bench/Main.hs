{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-|
layoutz vs Bubble Tea vs Brick, all drawing the same dashboard (see Dashboard.hs).

Two kinds of numbers:
- render: ms to turn the view into text, timed inside each program
- running: each program in a 200x60 pty (see Pty.hs). Idle CPU, keypress
  latency, and CPU/output while the model changes every 10ms

Run with `make bench`, or by hand:

> cabal bench --benchmark-options="--bubbletea path/to/bin --brick path/to/bin --out bench"

Any flag can be left out. Each run writes results.svg and the results section
of BENCHES.md into the --out directory (default: bench). Expect a few % noise between runs, so only
compare rows from the same run.
-}
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM)
import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import Data.Maybe (catMaybes)
import Layoutz
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.IO
import System.Process (readProcess)
import Text.Printf (printf)

import Dashboard
import Pty
import Report

main :: IO ()
main = getArgs >>= \case
  -- the bench re-runs itself with these to get the layoutz app
  ["--app", "idle"] -> runApp (dashboardApp Idle)
  ["--app", "busy"] -> runApp (dashboardApp Busy)
  args -> maybe usage (uncurry benchmark) (parseArgs args)
  where
    parseArgs = \case
      []                          -> Just ([], "bench")
      "--bubbletea" : path : rest -> first (Other "bubbletea" path :) <$> parseArgs rest
      "--brick" : path : rest     -> first (Other "brick" path :) <$> parseArgs rest
      "--out" : dir : rest        -> (\(os, _) -> (os, dir)) <$> parseArgs rest
      _                           -> Nothing
    first f (a, b) = (f a, b)
    usage = do
      hPutStrLn stderr "usage: layoutz-bench [--bubbletea PATH] [--brick PATH] [--out DIR]"
      exitFailure

-- | Name and path of another library's dashboard binary.
data Other = Other String FilePath

data Subject = Subject
  { subjectName :: String
  , renderTime  :: IO Double   -- ms per frame
  , program     :: FilePath
  , idleArgs    :: [String]
  , busyArgs    :: [String]
  }

data Results = Results
  { renderMs  :: Double
  , idleCpu   :: Double     -- fraction of one core
  , latencyMs :: [Double]
  , busyCpu   :: Double
  , outputKiB :: Double     -- per second
  , idleMem   :: Memory
  , busyMem   :: Memory
  }

screen :: (Int, Int)
screen = (60, 200)

layoutz :: IO Subject
layoutz = do
  self <- getExecutablePath
  pure Subject
    { subjectName = "layoutz"
    , renderTime  = layoutzRenderTime
    , program     = self
    , idleArgs    = ["--app", "idle"]
    , busyArgs    = ["--app", "busy"]
    }

-- | The other binaries all take render|idle|busy|version.
other :: Other -> IO Subject
other (Other name path) = do
  version <- unwords . words <$> readProcess path ["version"] ""
  pure Subject
    { subjectName = name <> " " <> version
    , renderTime  = read <$> readProcess path ["render"] ""
    , program     = path
    , idleArgs    = ["idle"]
    , busyArgs    = ["busy"]
    }

-- | Same as the others: 100 warmup frames, then time 2000.
layoutzRenderTime :: IO Double
layoutzRenderTime = do
  forM_ [1 .. 100] frame
  start <- seconds
  forM_ [1 .. frames] frame
  end <- seconds
  pure ((end - start) * 1000 / fromIntegral frames)
  where
    frames = 2000 :: Int
    (rows, cols) = screen
    frame k = evaluate (length (renderIn cols rows (dashboard k)))

measure :: Subject -> IO Results
measure subject = do
  step "render"
  rendered <- renderTime subject

  step "idle"
  (idle, latencies, idleM) <- withSession screen (program subject) (idleArgs subject) $ \s -> do
    drain s 1   -- startup + first frame
    idle <- cpuDuring s (drain s 5)
    latencies <- replicateM 40 (keypressLatency s)
    m <- memory s
    pure (idle, catMaybes latencies, m)

  step "busy"
  (busy, output, busyM) <- withSession screen (program subject) (busyArgs subject) $ \s -> do
    drain s 1
    before <- bytesWritten s
    start  <- seconds
    busy   <- cpuDuring s (drain s 5)
    end    <- seconds
    after  <- bytesWritten s
    m <- memory s
    pure (busy, fromIntegral (after - before) / 1024 / (end - start), m)

  putStrLn "done"
  pure (Results rendered idle latencies busy output idleM busyM)
  where
    step what = putStr (what <> "... ") >> hFlush stdout

cpuDuring :: Session -> IO () -> IO Double
cpuDuring s action = do
  cpu0 <- cpuSeconds s
  t0   <- seconds
  action
  cpu1 <- cpuSeconds s
  t1   <- seconds
  pure ((cpu1 - cpu0) / (t1 - t0))

-- | ms from pressing @+@ to the first byte back.
keypressLatency :: Session -> IO (Maybe Double)
keypressLatency s = do
  drain s 0.1   -- let the last frame finish
  start <- seconds
  press s (BC.pack "+")
  answered <- waitForOutput s 1
  end <- seconds
  drain s 0.05
  pure (if answered then Just ((end - start) * 1000) else Nothing)

benchmark :: [Other] -> FilePath -> IO ()
benchmark others out = do
  hSetBuffering stdout LineBuffering
  subjects <- (:) <$> layoutz <*> traverse other others
  results <- forM subjects $ \subject -> do
    putStr (subjectName subject <> ": ")
    r <- measure subject
    pure (subjectName subject, r)
  putStrLn ""
  putStrLn (render (report results))
  save out Run
    { runSetup   = setup
    , runCharts  = charts results
    , runHeaders = headers
    , runRows    = tableRows results
    }
  forM_ (filter (`notElem` measured) ["bubbletea", "brick"]) $ \name ->
    putStrLn ("\n" <> name <> " was not measured: " <> howToBuild name)
  where
    measured = [ name | Other name _ <- others ]
    howToBuild = \case
      "bubbletea" -> "`make bench` builds it, needs Go 1.21+ (GO=... to pick one)"
      _           -> "`make bench` builds it with cabal, see the error above"

report :: [(String, Results)] -> View m
report results = vstack
  [ text setup
  , br
  , table headers [ map text r | r <- tableRows results ]
  , br
  , ul (map text notes)
  ]

setup :: String
setup = printf "Dashboard on a %dx%d terminal: 40-row table, 15 bars, 20-line log" cols rows
  where (rows, cols) = screen

headers :: [String]
headers = ["", "render", "idle CPU", "key -> screen (p50 / p90)", "busy CPU", "busy output", "mem idle / busy peak"]

tableRows :: [(String, Results)] -> [[String]]
tableRows results =
  [ [ name
    , printf "%.2f ms" renderMs
    , percent idleCpu
    , printf "%.1f / %.1f ms" (quantile 0.5 latencyMs) (quantile 0.9 latencyMs)
    , percent busyCpu
    , printf "%.0f KiB/s" outputKiB
    , printf "%.1f / %.1f MiB" (residentMiB idleMem) (peakMiB busyMem)
    ]
  | (name, Results{..}) <- results
  ]

-- | The same numbers as the table, one chart per column.
charts :: [(String, Results)] -> [Chart]
charts results =
  [ panel "render, ms per frame" $ \Results{..} ->
      (renderMs, printf "%.2f ms" renderMs)
  , panel "idle CPU, % of one core" $ \Results{..} ->
      (idleCpu * 100, percent idleCpu)
  , panel "keypress -> screen, ms (p50)" $ \Results{..} ->
      let p50 = quantile 0.5 latencyMs
      in (p50, printf "%.1f ms (p90 %.1f)" p50 (quantile 0.9 latencyMs))
  , panel "busy CPU, % of one core" $ \Results{..} ->
      (busyCpu * 100, percent busyCpu)
  , panel "busy output, KiB/s" $ \Results{..} ->
      (outputKiB, printf "%.0f KiB/s" outputKiB)
  , panel "memory, MiB (busy peak)" $ \Results{..} ->
      (peakMiB busyMem, printf "%.1f MiB" (peakMiB busyMem))
  ]
  where
    panel title f = Chart title [ uncurry (Bar name) (f r) | (name, r) <- results ]

notes :: [String]
notes =
  [ "render: one frame of the view to text (Brick: to vty spans, it has no plain-text render)"
  , "idle CPU: nothing changing"
  , "key -> screen: keypress to first byte of the next frame"
  , "busy: model changes every 10ms, output is bytes the terminal has to parse"
  , "mem: RSS incl. runtime and binary, peak is the high-water mark while busy"
  , "CPU is % of one core, multi-threaded programs (Go, -threaded GHC) can go over 100%"
  ]

percent :: Double -> String
percent x = printf "%.1f%%" (x * 100)

quantile :: Double -> [Double] -> Double
quantile _ [] = 0 / 0
quantile q xs = sort xs !! min (length xs - 1) (floor (q * fromIntegral (length xs)))
