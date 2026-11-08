{-# LANGUAGE OverloadedStrings #-}
import Layoutz

-- Define layouts
people = withBorder BorderRound $ table ["Name", "Role", "Status"]
  [ ["Alice", "Engineer", "Online"]
  , ["Eve",   "QA",       "Away"]
  , [ul ["Gegard", ul ["Mousasi", ul ["was a BAD man"]]], "Fighter", "Nasty"]
  ]

xs = [0, 0.02 .. 8]

-- Nest, compose, combine them
demo = vstack
  [ center $ row
      [ withStyle StyleBold $ underlineColored "^" ColorBrightMagenta $ text "Layoutz"
      , "... A Small Demo (ちいさい)"
      ]
  , row
      [ withColor ColorBrightBlue $ statusCard "Users" "1.2K"
      , withColor ColorBrightGreen $ withBorder BorderDouble $ statusCard "API" "UP"
      , withColor ColorBrightYellow $ withBorder BorderThick $ statusCard "CPU" "23%"
      , people
      , section "Pugilists"
          [ kv [("Kazushi", "Sakuraba"), ("Jet 李連杰", "Li"), ("Rory", "MacDonald")]
          , tightRow [ withColor (ColorTrue r g b) "█"
                     | i <- [0, 12 .. 255]
                     , let r = if i < 128 then i * 2 else 255
                           g = if i < 128 then 255 else (255 - i) * 2
                           b = if i > 128 then (i - 128) * 2 else 0
                     ]
          ]
      ]
  , row
      [ vstack
          [ withColor ColorBrightMagenta $ withStyle (StyleReverse <> StyleBold) $
              box "Wrapped" [wrap 20 "Where there is a will ... Water x Necessaries"]
          , ol ["Arcole", "Rivoli", "Austerlitz", ol ["Iéna", "Wagram", ol ["Бородино"]]]
          ]
      , margin "[Haskell!]"
          [ withColor ColorGreen $ box "Deploy Status"
              [ inlineBar "Build" 1.0
              , inlineBar "Test" 0.8
              , inlineBar "Deploy" 0.3
              ]
          , withColor ColorCyan $ tree "📁 Project"
              [ branch "src"
                  [ branch "Main" [leaf "App.hs"]
                  , branch "test" [leaf "Spec.hs"]
                  ]
              ]
          ]
      , withBorder BorderRound $ box "Waves"
          [ plotLine 24 6
              [ Series [(x, sin x) | x <- xs] "sin" ColorBrightCyan
              , Series [(x, cos x) | x <- xs] "cos" ColorBrightMagenta
              ]
          ]
      ]
  ]

-- Get pretty strings w/ render
main = putStrLn $ render demo
