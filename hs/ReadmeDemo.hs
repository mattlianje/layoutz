{-# LANGUAGE OverloadedStrings #-}

import Layoutz

-- Define layouts (matching the Scala demo structure)
t = table' RoundBorder ["Name", "Role", "Status"]
    [ [text "Alice", text "Engineer", text "Online"]
    , [text "Eve", text "QA", text "Away"] 
    , [ul [text "Gegard", ul [text "Mousasi", ul [text "was a BAD man"]]], text "Fighter", text "Nasty"]
    ]

-- Nest, compose, combine them
d = layout
    [ center $ row [text "Layoutz", underline' "ˆ" $ text "DEMO"]
    , br
    , row
        [ statusCard "Users" "1.2K"
        , statusCard' DoubleBorder "API" "UP"
        , statusCard' ThickBorder "CPU" "23%"
        , t
        , section "Pugilists" 
            [kv [("Kazushi", "Sakuraba"), ("Jet", "Li"), ("Rory", "MacDonald")]]
        ]
    , br
    , margin "[Haskell!]"
        [ row
            [ box "Deploy Status"
                [ inlineBar "Build" 1.0
                , inlineBar "Test" 0.8
                , inlineBar "Deploy" 0.3
                ]
            , tree "📁 Project" 
                [ branch "src" 
                    [ leaf "main.hs"
                    , leaf "api.hs"
                    ]
                ]
            ]
        ]
    ]

-- Get pretty strings w/ render
main :: IO ()
main = putStrLn $ render d
