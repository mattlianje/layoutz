{-# LANGUAGE OverloadedStrings #-}
import Layoutz

sinPts = [(fromIntegral i, sin (fromIntegral i * 0.15) * 5) | i <- [0 .. 60 :: Int]]
cosPts = [(fromIntegral i, cos (fromIntegral i * 0.15) * 5) | i <- [0 .. 60 :: Int]]

d = vstack
  [ row
      [ panel "Signals"
          [ plotLine 44 9
              [ Series sinPts "sin(x)" ColorBrightCyan
              , Series cosPts "cos(x)" ColorBrightMagenta
              ]
          ]
      , panel "Diet"
          [ plotPie 33 7
              [ Slice 50 "Liquor" ColorDefault, Slice 20 "Protein" ColorDefault
              , Slice 10 "Water" ColorDefault, Slice 20 "Fun" ColorDefault
              ]
          ]
      ]
  , row
      [ panel "Commits"
          [ plotBar 47 9
              [ BarItem 100 "Mon" ColorBrightBlue, BarItem 120 "Tue" ColorBrightMagenta
              , BarItem 110 "Wed" ColorBrightCyan, BarItem 85 "Thu" ColorBrightYellow
              , BarItem 115 "Fri" ColorBrightGreen
              ]
          ]
      , panel "Load"
          [ plotHeatmap' 5 $ HeatmapData
              [ [12, 15, 22, 28, 30, 25, 18]
              , [14, 18, 25, 32, 35, 28, 20]
              , [10, 13, 20, 26, 28, 22, 15]
              , [11, 16, 24, 30, 33, 27, 19]
              , [13, 17, 23, 29, 34, 31, 26]
              , [16, 20, 27, 31, 30, 33, 29]
              , [15, 19, 21, 24, 22, 20, 17]
              ]
              ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
              ["6am", "9am", "12pm", "3pm", "6pm", "9pm", "12am"]
          ]
      ]
  ]

panel title body = box title body & border BorderRound

main = putStrLn $ render d
