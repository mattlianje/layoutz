{-# LANGUAGE OverloadedStrings #-}

{-|
Inline loading bar demo — renders a gradient progress bar in-place.

Run with: cabal run inline-bar
-}

module Main where

import Layoutz

data LoadState = LoadState
  { progress :: Double
  , doneTicks :: Int
  } deriving (Show)

data Msg = Tick deriving (Show, Eq)

initialState :: LoadState
initialState = LoadState 0.0 0

update :: Msg -> LoadState -> (LoadState, Cmd Msg)
update Tick state
  | doneTicks state > 30 = (state, cmdNone) -- finished
  | progress state >= 1.0 = (state { doneTicks = doneTicks state + 1 }, cmdNone)
  | otherwise =
      let next = min 1.0 (progress state + 0.008)
      in (state { progress = next }, cmdNone)

view :: LoadState -> View m
view state =
  let w = 40
      filled = floor (progress state * fromIntegral w) :: Int
      barBlocks = map (\i ->
        let ratio = fromIntegral i / fromIntegral w :: Double
            r = floor (ratio * 180) + 50
            g = floor ((1 - ratio) * 200) + 55
            b = 255 :: Int
        in fg (ColorTrue r g b) $ text "█"
        ) [0 .. filled - 1]
      emptyBlocks = replicate (w - filled)
        (fg ColorBrightBlack $ text "░")
      pct = show (floor (progress state * 100) :: Int) ++ "%"
  in vstack
       [ row (barBlocks ++ emptyBlocks) & gap 0
       , fg ColorBrightCyan $ text $ "Linking... " <> pct
       ]

app :: App LoadState Msg
app = App
  { appInit = (initialState, cmdNone)
  , appUpdate = update
  , appSubscriptions = \_ -> subEveryMs 16 Tick
  , appView = view
  }

main :: IO ()
main = runApp app
