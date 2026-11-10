{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Maybe (fromMaybe)
import Layoutz

data Model = Model
  { fighters :: SelectList String
  , logView  :: Viewport
  , focused  :: String
  }

data Msg = Moved (SelectList String) | Picked String | Scrolled Viewport | Focused String

-- | Strikes a minute at the opening bell, how that changes each minute, and
-- the share that land.
data Pace = Pace Double Double Double

roster :: [(String, Pace)]
roster =
  [ ("Gegard Mousasi", Pace 48 (-0.6) 0.50)
  , ("Rory MacDonald", Pace 62 (-0.9) 0.42)
  , ("Jet Li",         Pace 70 (-2.2) 0.55)
  , ("Lyoto Machida",  Pace 34 0.2    0.60)
  , ("Anderson Silva", Pace 38 1.3    0.50)
  ]

fighterNames :: [String]
fighterNames = map fst roster

-- | Three five-minute rounds, an exchange every 11 seconds.
fightLog :: String -> [String]
fightLog name =
  [ "R" <> show (1 + secs `div` 300) <> "  " <> clock (secs `mod` 300) <> "  " <> surname <> "  " <> moves !! ((i * 7 + length name) `mod` length moves)
  | i <- [0 .. 79 :: Int], let secs = i * 11 ]
  where
    surname = last (words name)
    clock s = show (s `div` 60) <> ":" <> (if s `mod` 60 < 10 then "0" else "") <> show (s `mod` 60)
    moves =
      [ "jab lands", "low kick, checked", "double leg, stuffed", "right hand lands clean"
      , "clinch, knee to the body", "circles out", "head kick, blocked", "slips the cross, counters"
      , "takes the back", "sprawls, back to the feet" ]

app :: LayoutzApp Model Msg
app = LayoutzApp
  { appInit = (Model (selectListOf fighterNames) viewportBottom "", cmdNone)
  , appUpdate = \msg m -> case msg of
      Moved sl   -> (m { fighters = sl, logView = viewportBottom }, cmdNone)
      Picked _   -> (m, cmdFocus "log")
      Scrolled v -> (m { logView = v }, cmdNone)
      Focused n  -> (m { focused = n }, cmdNone)
  , appSubscriptions = const (subFocus Focused)
  , appView = view
  }

view :: Model -> View Msg
view m = vstack
  [ text "layoutz · focus demo" & styleBold
  , row
      [ box "fighters" [selectList Moved Picked item (fighters m) & fill]
          & fixedWidth 22 & focusable "fighters" & focusStyle highlight
      , vstack
          [ box "strikes/min · 3 rounds" [paceView] & fixedHeight 13
          , box ("fight log · " <> name) [viewport Scrolled (logView m) (map text (fightLog name)) & fill]
              & fill & focusable "log" & focusStyle highlight
          ] & fill
      ] & fill
  , text ("focus: " <> focused m <> "   Tab switch · ↑↓ PgUp/PgDn · Enter → log · Esc quit") & styleDim
  ]
  where
    name = fromMaybe "" (selectedItem (fighters m))
    paceView = maybe (text "") (\p -> sized (paceChart p) & fill) (lookup name roster)
    -- padded so the selected one shows as an even bar
    item s = text (" " <> s <> replicate (longest - length s + 1) ' ')
      & if s == name then styleReverse else id
    longest = maximum (map length fighterNames)
    highlight :: View n -> View n
    highlight = fg ColorCyan . border BorderDouble

-- | Strikes thrown and landed over fifteen minutes, drawn to whatever size it
-- is given.
paceChart :: Pace -> Int -> Int -> View m
paceChart (Pace start fade accuracy) w h = plotLine pw (max 2 (h - 4))   -- the axis, its labels and the legend take 4 rows
  [ Series (curve thrown) "thrown" ColorCyan
  , Series (curve landed) "landed" ColorBrightBlack
  ]
  where
    pw = max 4 (w - 6)   -- the y labels ("52.3 │") take 6 columns
    n  = 6 * pw          -- a few samples per column keeps the steep parts joined up
    curve f = [ (x, f x) | i <- [0 .. n - 1], let x = 15 * fromIntegral i / fromIntegral (n - 1) ]
    -- output drifts over the fight and dips between rounds
    thrown x = start + fade * x + 5 * cos (2 * pi * (x + start) / 5) - 6 * exp (negate ((x - 5) ** 2)) - 6 * exp (negate ((x - 10) ** 2))
    landed x = accuracy * thrown x + 3 * sin (2 * pi * x / 3)

main :: IO ()
main = runApp app
