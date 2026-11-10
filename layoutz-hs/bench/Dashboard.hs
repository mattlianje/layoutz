{-# LANGUAGE OverloadedStrings #-}

-- | What every program in the bench draws. Frame @k@ changes most numbers.
--
-- Copied in bubbletea/main.go, brick/Main.hs and ratatui/src/main.rs, keep
-- them in sync.
module Dashboard
  ( dashboard
  , dashboardApp
  , Mode (..)
  ) where

import Layoutz

dashboard :: Int -> View m
dashboard k = row
  [ box "Table"
      [ table ["id", "name", "value"]
          [ [text (show i), text ("item" <> show (i + k)), text (show (i * k))] | i <- [1 .. 40 :: Int] ]
      ]
  , vstack
      [ box "Bars"
          [ inlineBar ("b" <> show i) (fromIntegral ((i * k) `mod` 100) / 100) | i <- [1 .. 15 :: Int] ]
          & fg ColorGreen
      , box "Log"
          [ text ("line " <> show (j + k) <> " lorem ipsum dolor sit amet") & fg ColorCyan | j <- [1 .. 20 :: Int] ]
      ]
  ]

-- | 'Idle' only redraws on @+@, 'Busy' also ticks every 10ms.
data Mode = Idle | Busy
  deriving (Eq, Show)

data Msg = Next

dashboardApp :: Mode -> LayoutzApp Int Msg
dashboardApp mode = LayoutzApp
  { appInit          = (1, cmdNone)
  , appUpdate        = \Next k -> (k + 1, cmdNone)
  , appSubscriptions = \_ -> mconcat
      [ subKeyPress $ \key -> case key of
          KeyChar '+' -> Just Next
          _           -> Nothing
      , if mode == Busy then subEveryMs 10 Next else subNone
      ]
  , appView          = dashboard
  }
