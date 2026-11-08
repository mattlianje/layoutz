{-# LANGUAGE OverloadedStrings #-}

{-|
Sizing demo: a fixed-width sidebar and a main panel that fill the terminal.
Click a section or scroll over the sidebar; Esc quits.
-}

module Main where

import Layoutz

data Msg = Pick Int | Nudge Int

sections :: [(String, [String])]
sections =
  [ ("Overview", ["3 services up", "1 deploy running", "0 alerts"])
  , ("Services", ["api      up   23%", "worker   up   61%", "cache    down  0%"])
  , ("Deploys",  ["#412  api     ok", "#413  worker  running"])
  , ("Alerts",   ["all quiet"])
  ]

update :: Msg -> Int -> Int
update (Pick i)  _ = i
update (Nudge d) i = max 0 (min (length sections - 1) (i + d))

view :: Int -> View Msg
view current = vstack
  [ text "layoutz · fill demo" & styleBold
  , row
      [ box "sections" (zipWith item [0 ..] sections) & fixedWidth 18 & onScroll Nudge
      , box title (map text body) & fill
      ] & fill
  , text "click a section · scroll the sidebar · Esc to quit" & styleDim
  ]
  where
    (title, body) = sections !! current
    item i (name, _)
      | i == current = text ("› " <> name) & fg ColorCyan & onClick (Pick i)
      | otherwise    = text ("  " <> name) & onClick (Pick i)

main :: IO ()
main = runApp (sandbox 0 update view)
