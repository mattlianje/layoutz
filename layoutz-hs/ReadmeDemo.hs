{-# LANGUAGE OverloadedStrings #-}
import Layoutz

-- Define layouts
t = withBorder BorderRound $ table ["Name", "Role", "Status"]
    [ ["Alice", "Engineer", "Online"]
    , ["Eve", "QA", "Away"] 
    , [ul ["Gegard", ul ["Mousasi", ul ["was a BAD man"]]], "Fighter", "Nasty"]
    ]

-- Nest, compose, combine them
d = layout
    [ center $ row 
        [ withStyle StyleBold $ underlineColored "^" ColorBrightMagenta $ text "Layoutz"
        , "... A Small Demo (ちいさい)" ]
    , row
        [ withColor ColorBrightBlue $ statusCard "Users" "1.2K"
        , withColor ColorBrightGreen $ withBorder BorderDouble $ statusCard "API" "UP"
        , withColor ColorBrightYellow $ withBorder BorderThick $ statusCard "CPU" "23%"
        , t
        , section "Pugilists" 
            [kv [("Kazushi", "Sakuraba"), ("Jet 李連杰", "Li"), ("Rory", "MacDonald")]]
        ]
    , br
    , row
        [ layout
            [ box "Wrapped"
                [ wrap 20 "Where there is a will ... Water x Necessaries" ]
            , ol [ "Arcole" , "Austerlitz" , ol [ "Iéna" , ol ["Бородино"] ] ] ]
        , margin "[Haskell!]"
            [ withColor ColorGreen $ box "Deploy Status"
                [ inlineBar "Build" 1.0
                , inlineBar "Test" 0.8
                , inlineBar "Deploy" 0.3
                ]
            , withColor ColorCyan $ tree "📁 Project" 
                [ branch "src" 
                    [ leaf "main.hs"
                    , leaf "test.hs"
                    ]
                ]
            ]
        ]
    ]

-- Get pretty strings w/ render
main :: IO ()
main = putStrLn $ render d
