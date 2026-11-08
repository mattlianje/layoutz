{-# LANGUAGE OverloadedStrings #-}
import Layoutz

-- Define layouts
t :: View m
t = table ["Name", "Role", "Status"]
    [ ["Alice", "Engineer", "Online"]
    , ["Eve", "QA", "Away"]
    , [ul ["Gegard", ul ["Mousasi", ul ["was a BAD man"]]], "Fighter", "Nasty"]
    ] & border BorderRound

gradient :: View m
gradient = row (map cell [0, 12 .. 255]) & gap 0
  where
    cell i =
      let r = if i < 128 then i * 2 else 255
          g = if i < 128 then 255 else (255 - i) * 2
          b = if i > 128 then (i - 128) * 2 else 0
      in text "█" & fg (ColorTrue r g b)

-- Nest, compose, combine them
d :: View m
d = vstack
    [ center $ row
        [ text "Layoutz" & underline & glyph "^" & accent ColorBrightMagenta & styleBold
        , "... A Small Demo (ちいさい)" ]
    , row
        [ statusCard "Users" "1.2K" & fg ColorBrightBlue
        , statusCard "API" "UP" & border BorderDouble & fg ColorBrightGreen
        , statusCard "CPU" "23%" & border BorderThick & fg ColorBrightYellow
        , t
        , section "Pugilists"
            [ kv [("Kazushi", "Sakuraba"), ("Jet 李連杰", "Li"), ("Rory", "MacDonald")]
            , gradient
            ]
        ]
    , row
        [ vstack
            [ box "Wrapped" [ wrap 20 "Where there is a will ... Water x Necessaries" ]
                & styleReverse & styleBold & fg ColorBrightMagenta
            , ol [ "Arcole" , "Austerlitz" , ol [ "Iéna" , ol ["Бородино"] ] ] ]
        , vstack
            [ box "Deploy Status"
                [ inlineBar "Build" 1.0
                , inlineBar "Test" 0.8
                , inlineBar "Deploy" 0.3
                ] & fg ColorGreen
            , tree "📁 Project"
                [ branch "src"
                    [ leaf "main.hs"
                    , leaf "test.hs"
                    ]
                ] & fg ColorCyan
            ] & margin "[Haskell!]"
        ]
    ]

-- Get pretty strings w/ render
main :: IO ()
main = putStrLn $ render d
