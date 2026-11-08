{-# LANGUAGE OverloadedStrings #-}
import Layoutz

sinPts = [(x, sin (x * 0.15) * 5) | x <- [0 .. 60]]
cosPts = [(x, cos (x * 0.15) * 5) | x <- [0 .. 60]]

charts = vstack
  [ row
      [ withBorder BorderRound $ box "plotLine"
          [ plotLine 52 10
              [ Series sinPts "sin(x)" ColorBrightCyan
              , Series cosPts "cos(x)" ColorBrightMagenta
              ]
          ]
      , withBorder BorderRound $ box "plotPie"
          [ plotPie 32 8
              [ Slice 50 "Liquor" ColorDefault, Slice 20 "Protein" ColorDefault
              , Slice 10 "Water" ColorDefault, Slice 20 "Fun" ColorDefault
              ]
          ]
      ]
  , row
      [ withBorder BorderRound $ box "plotBar"
          [ plotBar 42 9
              [ BarItem 100 "Mon" ColorBrightBlue, BarItem 120 "Tue" ColorBrightMagenta
              , BarItem 110 "Wed" ColorBrightCyan, BarItem 85 "Thu" ColorBrightYellow
              ]
          ]
      , withBorder BorderRound $ box "plotStackedBar"
          [ plotStackedBar 42 7
              [ StackedBarGroup [BarItem 30 "Q1" ColorDefault, BarItem 20 "Q2" ColorDefault, BarItem 25 "Q3" ColorDefault] "2023"
              , StackedBarGroup [BarItem 35 "Q1" ColorDefault, BarItem 25 "Q2" ColorDefault, BarItem 30 "Q3" ColorDefault] "2024"
              , StackedBarGroup [BarItem 40 "Q1" ColorDefault, BarItem 30 "Q2" ColorDefault, BarItem 35 "Q3" ColorDefault] "2025"
              ]
          ]
      ]
  , row
      [ withBorder BorderRound $ box "plotHeatmap"
          [ plotHeatmap' 5 $ HeatmapData
              [ [12, 15, 22, 28, 30, 25]
              , [14, 18, 25, 32, 35, 28]
              , [10, 13, 20, 26, 28, 22]
              , [16, 20, 27, 31, 30, 33]
              ]
              ["Mon", "Tue", "Wed", "Thu"]
              ["6am", "9am", "12pm", "3pm", "6pm", "9pm"]
          ]
      , vstack
          [ withBorder BorderRound $ box "chart" [chart [("Web", 10), ("Mobile", 20), ("API", 15)]]
          , withBorder BorderRound $ box "plotSparkline"
              [withColor ColorBrightGreen $ plotSparkline [1, 4, 2, 8, 5, 7, 3, 6, 9, 4, 6, 8, 5, 9]]
          ]
      ]
  ]

main = putStrLn $ render charts
