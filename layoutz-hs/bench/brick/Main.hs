{-# LANGUAGE CPP #-}

-- | Brick version of ../Dashboard.hs, driven by ../Main.hs.
--
-- > brick-dashboard render    ms per frame
-- > brick-dashboard idle      run the app, redraw on "+"
-- > brick-dashboard busy      same, plus a tick every 10ms
-- > brick-dashboard version   brick version
module Main (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderAttr, borderWithLabel)
import Brick.Widgets.Table (columnBorders, renderTable, rowBorders, table)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (evaluate)
import Control.Monad (forM_, forever, void, when)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp (..))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

screen :: (Int, Int)
screen = (200, 60)   -- cols, rows (vty order)

dashboard :: Int -> Widget ()
dashboard k = hBox [box "Table" tbl, str " ", vBox [allGreen (box "Bars" bars), box "Log" logs]]
  where
    tbl = renderTable . rowBorders False . columnBorders True . table $
      map str ["id", "name", "value"]
        : [ map str [show i, "item" <> show (i + k), show (i * k)] | i <- [1 .. 40 :: Int] ]
    bars = vBox [ str (bar ("b" <> show i) (fromIntegral ((i * k) `mod` 100) / 100)) | i <- [1 .. 15 :: Int] ]
    logs = vBox [ withAttr cyan (str ("line " <> show (j + k) <> " lorem ipsum dolor sit amet")) | j <- [1 .. 20 :: Int] ]

-- | Border too, to match @fg@ on a layoutz box.
allGreen :: Widget () -> Widget ()
allGreen = overrideAttr borderAttr green . withAttr green

box :: String -> Widget () -> Widget ()
box title = borderWithLabel (str title) . padLeftRight 1

bar :: String -> Double -> String
bar label p = label <> " [" <> replicate n '█' <> replicate (20 - n) '─' <> "] " <> show (floor (p * 100) :: Int) <> "%"
  where n = floor (p * 20)

green, cyan :: AttrName
green = attrName "green"
cyan  = attrName "cyan"

colours :: AttrMap
colours = attrMap V.defAttr [(green, fg V.green), (cyan, fg V.cyan)]

data Tick = Tick

app :: App Int Tick ()
app = App
  { appDraw         = \k -> [dashboard k]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = \ev -> case ev of
      VtyEvent (V.EvKey (V.KChar '+') [])        -> modify (+ 1)
      VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
      AppEvent Tick                              -> modify (+ 1)
      _                                          -> pure ()
  , appStartEvent   = pure ()
  , appAttrMap      = const colours
  }

runDashboard :: Bool -> IO ()
runDashboard busy = do
  chan <- newBChan 10
  when busy $
    void (forkIO (forever (writeBChan chan Tick >> threadDelay 10000)))
  vty <- mkVty V.defaultConfig
  void (customMain vty (mkVty V.defaultConfig) (Just chan) app 1)

-- | ms per frame, widgets -> vty spans ('renderWidget' then 'displayOpsForPic').
-- Closest thing to layoutz's 'renderIn' or lipgloss's View, all that's left
-- after is writing escape codes. 100 warmup frames, then time 2000.
renderTime :: IO Double
renderTime = do
  forM_ [1 .. 100] frame
  start <- getMonotonicTimeNSec
  forM_ [1 .. frames] frame
  end <- getMonotonicTimeNSec
  pure (fromIntegral (end - start) / 1e6 / fromIntegral frames)
  where
    frames = 2000 :: Int
    frame k = do
      let ops = displayOpsForPic (renderWidget (Just colours) [dashboard k] screen) screen
      evaluate (Vector.sum (Vector.map (Vector.sum . Vector.map spanLength) ops))
    spanLength (TextSpan _ _ _ t) = fromIntegral (TL.length t)
    spanLength _                  = 1 :: Int

main :: IO ()
main = getArgs >>= \args -> case args of
  ["render"]  -> renderTime >>= printf "%.4f\n"
  ["version"] -> putStrLn VERSION_brick
  ["idle"]    -> runDashboard False
  ["busy"]    -> runDashboard True
  _ -> do
    hPutStrLn stderr "usage: brick-dashboard render|idle|busy|version"
    exitWith (ExitFailure 2)
