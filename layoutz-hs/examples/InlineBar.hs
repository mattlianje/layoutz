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
  | doneTicks state > 30 = (state, CmdNone) -- finished
  | progress state >= 1.0 = (state { doneTicks = doneTicks state + 1 }, CmdNone)
  | otherwise =
      let next = min 1.0 (progress state + 0.008)
      in (state { progress = next }, CmdNone)

view :: LoadState -> L
view state =
  let w = 40
      filled = floor (progress state * fromIntegral w) :: Int
      barBlocks = map (\i ->
        let ratio = fromIntegral i / fromIntegral w :: Double
            r = floor (ratio * 180) + 50
            g = floor ((1 - ratio) * 200) + 55
            b = 255 :: Int
        in withColor (ColorTrue r g b) $ text "█"
        ) [0 .. filled - 1]
      emptyBlocks = replicate (w - filled)
        (withColor ColorBrightBlack $ text "░")
      pct = show (floor (progress state * 100) :: Int) ++ "%"
  in layout
       [ tightRow (barBlocks ++ emptyBlocks)
       , withColor ColorBrightCyan $ text $ "Linking... " <> pct
       ]

app :: LayoutzApp LoadState Msg
app = LayoutzApp
  { appInit = (initialState, CmdNone)
  , appUpdate = update
  , appSubscriptions = \_ -> subEveryMs 16 Tick
  , appView = view
  }

main :: IO ()
main = runApp app
