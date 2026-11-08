{-# LANGUAGE OverloadedStrings #-}

{-|
Focus demo: a service list and its logs. Tab switches panels, arrows and
PgUp/PgDn move or scroll, Enter jumps to the logs, the wheel scrolls whatever
is under it. Esc quits.
-}

module Main where

import Data.Maybe (fromMaybe)
import Layoutz

data Model = Model
  { services :: SelectList String
  , logView  :: Viewport
  , focused  :: String
  }

data Msg = Moved (SelectList String) | Picked String | Scrolled Viewport | Focused String

logsFor :: String -> [String]
logsFor svc =
  [ "12:" <> pad2 (i `div` 60) <> ":" <> pad2 (i `mod` 60) <> "  " <> svc <> "  " <> event i
  | i <- [0 .. 79 :: Int] ]
  where
    pad2 n = (if n < 10 then "0" else "") <> show n
    event i = ["GET /health 200", "POST /jobs 201", "GET /jobs/" <> show i <> " 200", "GET /jobs/x 404"] !! (i `mod` 4)

app :: App Model Msg
app = App
  { appInit = (Model (selectListOf ["api", "worker", "scheduler", "db", "cache"]) viewportBottom "", cmdNone)
  , appUpdate = \msg m -> case msg of
      Moved sl   -> (m { services = sl, logView = viewportBottom }, cmdNone)
      Picked _   -> (m, cmdFocus "logs")
      Scrolled v -> (m { logView = v }, cmdNone)
      Focused n  -> (m { focused = n }, cmdNone)
  , appSubscriptions = const (subFocus Focused)
  , appView = view
  }

view :: Model -> View Msg
view m = vstack
  [ text "layoutz · focus demo" & styleBold
  , row
      [ box "services" [selectList Moved Picked text (services m) & fill]
          & fixedWidth 20 & focusable "services" & focusStyle highlight
      , box ("logs · " <> svc) [viewport Scrolled (logView m) (map text (logsFor svc)) & fill]
          & fill & focusable "logs" & focusStyle highlight
      ] & fill
  , text ("focus: " <> focused m <> "   Tab switch · ↑↓ PgUp/PgDn · Enter → logs · Esc quit") & styleDim
  ]
  where
    svc = fromMaybe "" (selectedItem (services m))
    highlight :: View n -> View n
    highlight = fg ColorCyan . border BorderDouble

main :: IO ()
main = runApp app
