{-# LANGUAGE OverloadedStrings #-}
import Layoutz

-- Define layouts (matching the Scala demo structure)
t = withBorder RoundBorder $ table ["Name", "Role", "Status"]
    [ ["Alice", "Engineer", "Online"]
    , ["Eve", "QA", "Away"] 
    , [ul ["Gegard", ul ["Mousasi", ul ["was a BAD man"]]], "Fighter", "Nasty"]
    ]

-- Nest, compose, combine them
d = layout
    [ center $ row ["Layoutz", underline' "ˆ" $ text "DEMO"]
    , br
    , row
        [ statusCard "Users" "1.2K"
        , withBorder DoubleBorder $ statusCard "API" "UP"
        , withBorder ThickBorder $ statusCard "CPU" "23%"
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
