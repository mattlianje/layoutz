{-# LANGUAGE OverloadedStrings #-}
import Layoutz

-- Define layouts
people = table ["Name", "Role", "Status"]
  [ ["Alice", "Engineer", "Online"]
  , ["Eve",   "QA",       "Away"]
  , [ul ["Gegard", ul ["Mousasi", ul ["was a BAD man"]]], "Fighter", "Nasty"]
  ] & border BorderRound

xs = [0, 0.02 .. 8]

-- Nest, compose, combine them
demo = vstack
  [ center $ row
      [ text "Layoutz" & underline & glyph "^" & accent ColorBrightMagenta & styleBold
      , "... A Small Demo (ちいさい)"
      ]
  , row
      [ statusCard "Users" "1.2K" & fg ColorBrightBlue
      , statusCard "API" "UP" & border BorderDouble & fg ColorBrightGreen
      , statusCard "CPU" "23%" & border BorderThick & fg ColorBrightYellow
      , people
      , section "Pugilists"
          [ kv [("Kazushi", "Sakuraba"), ("Jet 李連杰", "Li"), ("Rory", "MacDonald")]
          , row [ fg (ColorTrue r g b) "█"
                   | i <- [0, 12 .. 255]
                   , let r = if i < 128 then i * 2 else 255
                         g = if i < 128 then 255 else (255 - i) * 2
                         b = if i > 128 then (i - 128) * 2 else 0
                   ] & gap 0
          ]
      ]
  , row
      [ vstack
          [ box "Wrapped" [wrap 20 "Where there is a will ... Water x Necessaries"]
              & styleReverse & styleBold & fg ColorBrightMagenta
          , ol ["Arcole", "Rivoli", "Austerlitz", ol ["Iéna", "Wagram", ol ["Бородино"]]]
          ]
      , vstack
          [ box "Deploy Status"
              [ inlineBar "Build" 1.0
              , inlineBar "Test" 0.8
              , inlineBar "Deploy" 0.3
              ] & fg ColorGreen
          , tree "📁 Project"
              [ branch "src"
                  [ branch "Main" [leaf "App.hs"]
                  , branch "test" [leaf "Spec.hs"]
                  ]
              ] & fg ColorCyan
          ] & margin "[Haskell!]"
      , box "Waves"
          [ plotLine 24 6
              [ Series [(x, sin x) | x <- xs] "sin" ColorBrightCyan
              , Series [(x, cos x) | x <- xs] "cos" ColorBrightMagenta
              ]
          ] & border BorderRound
      ]
  ]

-- Get pretty strings w/ render
main = putStrLn $ render demo
