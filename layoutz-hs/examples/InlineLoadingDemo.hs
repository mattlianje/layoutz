{-# LANGUAGE OverloadedStrings #-}

{-|
Inline loading demo - progress bars that render in-place without
clearing the screen. The user prompt and prior output stay visible.

Run with: cabal run inline-loading-demo
-}

module Main where

import Layoutz

data S = S { prog :: Double, done :: Int }
data M = Tick

progressBar :: String -> Double -> LayoutzApp S M
progressBar label speed = LayoutzApp
  { appInit = (S 0.0 0, CmdNone)
  , appUpdate = \Tick st ->
      if done st > 20     then (st, CmdExit)
      else if prog st >= 1.0 then (st { done = done st + 1 }, CmdNone)
      else (st { prog = min 1.0 (prog st + speed) }, CmdNone)
  , appSubscriptions = \_ -> subEveryMs 16 Tick
  , appView = \st ->
      let w = 40
          filled = floor (prog st * fromIntegral w) :: Int
          bar = map (\i ->
            let r' = fromIntegral i / fromIntegral w :: Double
                r = floor (r' * 180) + 50
                g = floor ((1 - r') * 200) + 55
            in withColor (ColorTrue r g 255) $ text "█"
            ) [0 .. filled - 1]
          empty = replicate (w - filled) (withColor ColorBrightBlack $ text "░")
          pct = show (floor (prog st * 100) :: Int) ++ "%"
      in layout [ tightRow (bar ++ empty)
                , withColor ColorBrightCyan $ text $ label <> " " <> pct
                ]
  }

main :: IO ()
main = do
  putStrLn "Just a normal process"
  runInline (progressBar "Fetching deps..." 0.018)
  runInline (progressBar "Building..."      0.010)
  runInline (progressBar "Linking..."       0.025)
