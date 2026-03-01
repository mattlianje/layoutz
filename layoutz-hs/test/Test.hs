{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Layoutz
import Data.List (isInfixOf)
import Data.IORef (newIORef, readIORef, writeIORef)

-- Helper to strip ANSI codes for testing
stripAnsiTest :: String -> String
stripAnsiTest [] = []
stripAnsiTest ('\ESC':'[':rest) = stripAnsiTest (dropAfterM rest)
  where 
    dropAfterM [] = []
    dropAfterM ('m':xs) = xs
    dropAfterM (_:xs) = dropAfterM xs
stripAnsiTest (c:rest) = c : stripAnsiTest rest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Layoutz Tests"
  [ basicElementTests
  , visualFeatureTests
  , dataVisualizationTests
  , containerTests
  , layoutTests
  , dimensionTests
  , colorTests
  , extendedColorTests
  , styleTests
  , commandTests
  , visualizationTests
  ]

-- Basic element tests
basicElementTests :: TestTree
basicElementTests = testGroup "Basic Elements"
  [ testCase "text rendering" $
      render (text "Hello World") @?= "Hello World"
      
  , testCase "line break rendering" $
      render br @?= ""
      
  , testCase "underline default" $
      render (underline $ text "Test") @?= "Test\n────"
      
  , testCase "underline custom" $
      render (underline' "=" $ text "Test") @?= "Test\n===="
      
  , testCase "center default width" $
      -- center now uses smart auto-centering, test within a layout context
      render (layout [text "wide content here to establish context width", center $ text "Test"]) @?= "wide content here to establish context width\n                    Test                    "
      
  , testCase "center custom width" $
      render (center' 10 $ text "Test") @?= "   Test   "
  ]

-- Visual feature tests
visualFeatureTests :: TestTree
visualFeatureTests = testGroup "Visual Features"
  [ testCase "horizontal rule default" $
      render hr @?= "──────────────────────────────────────────────────"
      
  , testCase "horizontal rule custom char" $
      render (hr' "=") @?= "=================================================="
      
  , testCase "horizontal rule custom width" $
      render (hr'' "-" 10) @?= "----------"
      
  , testCase "vertical rule default" $
      length (lines $ render vr) @?= 10
      
  , testCase "vertical rule custom char" $
      head (lines $ render $ vr' "║") @?= "║"
      
  , testCase "vertical rule custom height" $
      length (lines $ render $ vr'' "|" 5) @?= 5
      
  , testCase "padding" $
      lines (render $ pad 2 $ text "Test") @?= ["        ", "        ", "  Test  ", "        ", "        "]
      
  , testCase "margin custom" $
      render (margin ">>>" [text "Line 1", text "Line 2"]) @?= ">>> Line 1\n>>> Line 2"
      
  , testCase "margin error" $
      render (margin "[error]" [text "Error message"]) @?= "[error] Error message"
  ]

-- Data visualization tests
dataVisualizationTests :: TestTree
dataVisualizationTests = testGroup "Data Visualization"
  [ testCase "inline bar 0%" $
      "Test [────────────────────] 0%" `elem` lines (render $ inlineBar "Test" 0.0) @?= True
      
  , testCase "inline bar 50%" $
      "Test [██████████──────────] 50%" `elem` lines (render $ inlineBar "Test" 0.5) @?= True
      
  , testCase "inline bar 100%" $
      "Test [████████████████████] 100%" `elem` lines (render $ inlineBar "Test" 1.0) @?= True
      
  , testCase "chart" $
      "Task │████████████████████████████████████████│ 50" `elem` lines (render $ chart [("Task", 50.0)]) @?= True
      
  , testCase "key-value pairs" $
      render (kv [("Key", "Value")]) @?= "Key: Value"
  ]

-- Container tests
containerTests :: TestTree
containerTests = testGroup "Containers"
  [ testCase "status card default border" $
      length (lines $ render $ statusCard "API" "UP") @?= 4
      
  , testCase "status card double border" $
      "╔═══════╗" `elem` lines (render $ withBorder BorderDouble $ statusCard "API" "UP") @?= True
      
  , testCase "box default border" $
      "┌──Title──┐" `elem` lines (render $ box "Title" [text "Content"]) @?= True
      
  , testCase "box double border" $
      "╔══Title══╗" `elem` lines (render $ withBorder BorderDouble $ box "Title" [text "Content"]) @?= True
  ]

-- Layout tests
layoutTests :: TestTree
layoutTests = testGroup "Layout"
  [ testCase "unordered list single level" $
      render (ul [text "Item 1", text "Item 2"]) @?= "• Item 1\n• Item 2"
      
  , testCase "unordered list nested" $
      "  ◦ Sub 1" `elem` lines (render $ ul [text "Item 1", ul [text "Sub 1", text "Sub 2"]]) @?= True
      
  , testCase "ordered list single level" $
      render (ol [text "First", text "Second"]) @?= "1. First\n2. Second"
      
  , testCase "ordered list nested with letters" $
      "  a. Nested" `elem` lines (render $ ol [text "Item 1", ol [text "Nested", text "Also nested"]]) @?= True
      
  , testCase "ordered list triple nested with roman numerals" $
      "    i. Deep" `elem` lines (render $ ol [text "L1", ol [text "L2", ol [text "Deep"]]]) @?= True
      
  , testCase "table basic" $
      "│ A │ B │" `elem` lines (render $ table ["A", "B"] [[text "1", text "2"]]) @?= True
      
  , testCase "tree structure" $
      "Root" `elem` lines (render $ tree "Root" [leaf "Child"]) @?= True
      
  , testCase "section" $
      "=== Title ===" `elem` lines (render $ section "Title" [text "Content"]) @?= True
      
  , testCase "row layout" $
      render (row [text "A", text "B"]) @?= "A B"
      
  , testCase "layout composition" $
      render (layout [text "Line 1", text "Line 2"]) @?= "Line 1\nLine 2"
      
  , testCase "align left" $
      render (alignLeft 10 "Hi") @?= "Hi        "
      
  , testCase "align right" $
      render (alignRight 10 "Hi") @?= "        Hi"
      
  , testCase "align center" $
      render (alignCenter 10 "Hi") @?= "    Hi    "
      
  , testCase "justify" $
      render (justify 20 "Hello world test") @?= "Hello   world   test"
  ]

-- Dimension tests
dimensionTests :: TestTree
dimensionTests = testGroup "Dimensions"
  [ testCase "width calculation" $
      width (text "Hello") @?= 5
      
  , testCase "height calculation" $
      height (layout [text "Line 1", text "Line 2"]) @?= 2
  ]

-- Color tests
colorTests :: TestTree
colorTests = testGroup "Colors"
  [ testCase "render includes ANSI codes" $
      "\ESC[31m" `isInfixOf` render (withColor ColorRed $ text "Hello") @?= True
      
  , testCase "render includes reset code" $
      "\ESC[0m" `isInfixOf` render (withColor ColorRed $ text "Hello") @?= True
      
  , testCase "colored element width ignores ANSI codes" $
      width (withColor ColorRed $ text "Hello") @?= 5
      
  , testCase "colored status card maintains structure" $
      let colored = render $ withColor ColorGreen $ withBorder BorderDouble $ statusCard "API" "UP"
          strippedLines = map stripAnsiTest (lines colored)
      in "╔═══════╗" `elem` strippedLines @?= True
      
  , testCase "colored box maintains structure" $
      let colored = render $ withColor ColorBlue $ box "Title" [text "Content"]
          strippedLines = map stripAnsiTest (lines colored)
      in "┌──Title──┐" `elem` strippedLines @?= True
      
  , testCase "nested colors work" $
      "\ESC[32m" `isInfixOf` render (withColor ColorRed $ layout [withColor ColorGreen $ text "Hi"]) @?= True
      
  , testCase "bright colors use correct codes" $
      "\ESC[91m" `isInfixOf` render (withColor ColorBrightRed $ text "Hi") @?= True
      
  , testCase "color with borders both work" $
      let colored = render $ withColor ColorYellow $ withBorder BorderThick $ statusCard "Test" "OK"
          strippedLines = map stripAnsiTest (lines colored)
      in "┏━━━━━━━━┓" `elem` strippedLines @?= True
      
  , testCase "colored underline includes ANSI codes" $
      "\ESC[31m" `isInfixOf` render (underlineColored "=" ColorRed $ text "Title") @?= True
      
  , testCase "colored underline maintains structure" $
      let underlined = render $ underlineColored "=" ColorRed $ text "Test"
          strippedLines = map stripAnsiTest (lines underlined)
      in "====" `elem` strippedLines @?= True
  ]

-- Extended color tests (256-color and RGB)
extendedColorTests :: TestTree
extendedColorTests = testGroup "Extended Colors"
  [ testCase "256-color palette (ColorFull)" $
      "\ESC[38;5;" `isInfixOf` render (withColor (ColorFull 196) $ text "Red") @?= True
      
  , testCase "256-color clamping max" $
      "\ESC[38;5;255" `isInfixOf` render (withColor (ColorFull 300) $ text "Clamped") @?= True
      
  , testCase "256-color clamping min" $
      "\ESC[38;5;0" `isInfixOf` render (withColor (ColorFull (-10)) $ text "Clamped") @?= True
      
  , testCase "RGB true color (ColorTrue)" $
      "\ESC[38;2;" `isInfixOf` render (withColor (ColorTrue 255 100 50) $ text "RGB") @?= True
      
  , testCase "RGB true color full spec" $
      "\ESC[38;2;255;100;50" `isInfixOf` render (withColor (ColorTrue 255 100 50) $ text "RGB") @?= True
      
  , testCase "RGB color clamping" $
      "\ESC[38;2;255;255;0" `isInfixOf` render (withColor (ColorTrue 300 300 (-10)) $ text "Clamped") @?= True
      
  , testCase "tightRow renders without spaces" $
      stripAnsiTest (render $ tightRow [withColor ColorRed $ text "A", withColor ColorGreen $ text "B"]) @?= "AB"
      
  , testCase "tightRow vs row spacing" $
      let tightResult = stripAnsiTest (render $ tightRow [text "A", text "B"])
          normalResult = render $ row [text "A", text "B"]
      in (tightResult == "AB" && normalResult == "A B") @?= True
  ]

-- Style tests (including combining)
styleTests :: TestTree
styleTests = testGroup "Styles"
  [ testCase "bold style" $
      "\ESC[1m" `isInfixOf` render (withStyle StyleBold $ text "Bold") @?= True
      
  , testCase "italic style" $
      "\ESC[3m" `isInfixOf` render (withStyle StyleItalic $ text "Italic") @?= True
      
  , testCase "combined styles with <>" $
      let combined = render $ withStyle (StyleBold <> StyleItalic) $ text "Fancy"
      in ("\ESC[1;3m" `isInfixOf` combined || "\ESC[3;1m" `isInfixOf` combined) @?= True
      
  , testCase "triple combined styles" $
      let combined = render $ withStyle (StyleBold <> StyleItalic <> StyleUnderline) $ text "Very Fancy"
          hasAll = "\ESC[" `isInfixOf` combined && "1" `isInfixOf` combined && "3" `isInfixOf` combined && "4" `isInfixOf` combined
      in hasAll @?= True
      
  , testCase "style with color both work" $
      let styled = render $ withColor ColorRed $ withStyle StyleBold $ text "Important"
      in ("\ESC[31m" `isInfixOf` styled && "\ESC[1m" `isInfixOf` styled) @?= True
      
  , testCase "style maintains element width" $
      width (withStyle StyleBold $ text "Hello") @?= 5
      
  , testCase "combined style maintains structure" $
      let styled = render $ withStyle (StyleBold <> StyleReverse) $ box "Title" [text "Content"]
          strippedLines = map stripAnsiTest (lines styled)
      in "┌──Title──┐" `elem` strippedLines @?= True
      
  , testCase "wrap text at word boundaries" $
      let wrapped = render $ wrap 10 "This is a very long text"
          wrappedLines = lines wrapped
      in length wrappedLines > 1 @?= True
      
  , testCase "wrap preserves words" $
      "This is a" `isInfixOf` render (wrap 10 "This is a test") @?= True
  ]

-- Command execution tests
commandTests :: TestTree
commandTests = testGroup "Commands"
  [ testCase "CmdNone produces Nothing" $ do
      result <- executeCmd (CmdNone :: Cmd String)
      result @?= Nothing

  , testCase "cmdFire executes IO without message" $ do
      ref <- newIORef (0 :: Int)
      result <- executeCmd (cmdFire $ writeIORef ref 42 :: Cmd String)
      val <- readIORef ref
      result @?= Nothing
      val @?= 42

  , testCase "cmdTask executes IO and returns message" $ do
      result <- executeCmd (cmdTask $ pure "hello" :: Cmd String)
      result @?= Just "hello"

  , testCase "CmdBatch executes all commands" $ do
      ref <- newIORef (0 :: Int)
      _ <- executeCmd (CmdBatch [cmdFire $ writeIORef ref 1, cmdFire $ writeIORef ref 2] :: Cmd String)
      val <- readIORef ref
      val @?= 2  -- Last write wins

  , testCase "CmdBatch returns first Just message" $ do
      result <- executeCmd (CmdBatch [CmdRun (pure Nothing), cmdTask (pure "first"), cmdTask (pure "second")] :: Cmd String)
      result @?= Just "first"
  ]

-- Visualization primitive tests
visualizationTests :: TestTree
visualizationTests = testGroup "Visualizations"
  [ testGroup "Renames"
    [ testCase "ColorDefault produces no ANSI wrapping" $
        render (withColor ColorDefault $ text "Hello") @?= "Hello"

    , testCase "StyleDefault produces no style wrapping" $
        render (withStyle StyleDefault $ text "Hello") @?= "Hello"

    , testCase "StyleDefault is Monoid mempty" $
        render (withStyle mempty $ text "Hello") @?= "Hello"
    ]

  , testGroup "Sparkline"
    [ testCase "sparkline output length matches input" $
        length (render $ plotSparkline [1,2,3,4,5]) @?= 5

    , testCase "sparkline empty input" $
        render (plotSparkline []) @?= ""

    , testCase "sparkline constant values use middle block" $
        all (== '▄') (render $ plotSparkline [5,5,5]) @?= True

    , testCase "sparkline max value is full block" $
        last (render $ plotSparkline [0,0,10]) @?= '█'

    , testCase "sparkline min value is lowest block" $
        head (render $ plotSparkline [0,5,10]) @?= '▁'
    ]

  , testGroup "Line Plot"
    [ testCase "plotLine no data" $
        render (plotLine 20 5 []) @?= "No data"

    , testCase "plotLine empty series" $
        render (plotLine 20 5 [Series [] "empty" ColorBrightCyan]) @?= "No data"

    , testCase "plotLine contains y-axis" $
        "│" `isInfixOf` render (plotLine 20 5 [Series [(0,0),(1,1)] "test" ColorBrightCyan]) @?= True

    , testCase "plotLine contains x-axis" $
        "─" `isInfixOf` render (plotLine 20 5 [Series [(0,0),(1,1)] "test" ColorBrightCyan]) @?= True

    , testCase "plotLine multi-series has legend" $
        let r = render $ plotLine 20 5
              [ Series [(0,0),(1,1)] "A" ColorBrightCyan
              , Series [(0,1),(1,0)] "B" ColorBrightRed ]
        in ("A" `isInfixOf` r && "B" `isInfixOf` r) @?= True

    , testCase "plotLine single series no legend" $
        let r = render $ plotLine 20 5 [Series [(0,0),(1,1)] "only" ColorBrightCyan]
        in not ("●" `isInfixOf` stripAnsiTest r) @?= True

    , testCase "plotLine contains braille characters" $
        let r = stripAnsiTest $ render $ plotLine 30 8 [Series [(x, sin x) | x <- [0,0.1..6.28]] "sin" ColorBrightCyan]
        in any (\c -> c >= '⠀' && c <= '⣿') r @?= True
    ]

  , testGroup "Pie Chart"
    [ testCase "plotPie no data" $
        render (plotPie 10 5 []) @?= "No data"

    , testCase "plotPie legend has percentages" $
        let r = render $ plotPie 15 6 [Slice 75 "Big" ColorBrightCyan, Slice 25 "Small" ColorBrightMagenta]
        in ("75%" `isInfixOf` stripAnsiTest r && "25%" `isInfixOf` stripAnsiTest r) @?= True

    , testCase "plotPie legend has labels" $
        let r = stripAnsiTest $ render $ plotPie 15 6
              [Slice 50 "Alpha" ColorBrightCyan, Slice 50 "Beta" ColorBrightMagenta]
        in ("Alpha" `isInfixOf` r && "Beta" `isInfixOf` r) @?= True

    , testCase "plotPie contains braille characters" $
        let r = stripAnsiTest $ render $ plotPie 20 8
              [Slice 60 "A" ColorBrightCyan, Slice 40 "B" ColorBrightMagenta]
        in any (\c -> c >= '⠀' && c <= '⣿') r @?= True
    ]

  , testGroup "Bar Chart"
    [ testCase "plotBar no data" $
        render (plotBar 20 5 []) @?= "No data"

    , testCase "plotBar contains y-axis" $
        "│" `isInfixOf` render (plotBar 20 5 [BarItem 10 "A" ColorBrightCyan]) @?= True

    , testCase "plotBar contains bar labels" $
        let r = stripAnsiTest $ render $ plotBar 30 5
              [BarItem 10 "Mon" ColorBrightCyan, BarItem 20 "Tue" ColorBrightGreen]
        in ("Mon" `isInfixOf` r && "Tue" `isInfixOf` r) @?= True

    , testCase "plotBar contains block characters" $
        let r = stripAnsiTest $ render $ plotBar 20 5 [BarItem 100 "X" ColorBrightCyan]
        in any (\c -> c `elem` ("▁▂▃▄▅▆▇█" :: String)) r @?= True

    , testCase "plotBar y-axis shows max value" $
        let r = stripAnsiTest $ render $ plotBar 30 5 [BarItem 50 "A" ColorBrightCyan]
        in "50" `isInfixOf` r @?= True
    ]

  , testGroup "Stacked Bar Chart"
    [ testCase "plotStackedBar no data" $
        render (plotStackedBar 20 5 []) @?= "No data"

    , testCase "plotStackedBar contains group labels" $
        let r = stripAnsiTest $ render $ plotStackedBar 30 5
              [ StackedBarGroup [BarItem 10 "X" ColorDefault] "G1"
              , StackedBarGroup [BarItem 20 "X" ColorDefault] "G2" ]
        in ("G1" `isInfixOf` r && "G2" `isInfixOf` r) @?= True

    , testCase "plotStackedBar multi-segment has legend" $
        let r = stripAnsiTest $ render $ plotStackedBar 30 5
              [ StackedBarGroup [BarItem 10 "Sales" ColorDefault, BarItem 5 "Tax" ColorDefault] "Q1" ]
        in ("Sales" `isInfixOf` r && "Tax" `isInfixOf` r) @?= True

    , testCase "plotStackedBar single segment no legend" $
        let r = stripAnsiTest $ render $ plotStackedBar 30 5
              [ StackedBarGroup [BarItem 10 "Only" ColorDefault] "Q1"
              , StackedBarGroup [BarItem 20 "Only" ColorDefault] "Q2" ]
        in not ("█ Only" `isInfixOf` r) @?= True
    ]

  , testGroup "Heatmap"
    [ testCase "plotHeatmap no data" $
        render (plotHeatmap (HeatmapData [] [] [])) @?= "No data"

    , testCase "plotHeatmap contains row labels" $
        let r = stripAnsiTest $ render $ plotHeatmap
              (HeatmapData [[1,2],[3,4]] ["Row1","Row2"] ["C1","C2"])
        in ("Row1" `isInfixOf` r && "Row2" `isInfixOf` r) @?= True

    , testCase "plotHeatmap contains column labels" $
        let r = stripAnsiTest $ render $ plotHeatmap
              (HeatmapData [[1,2],[3,4]] ["R1","R2"] ["ColA","ColB"])
        in ("ColA" `isInfixOf` r && "ColB" `isInfixOf` r) @?= True

    , testCase "plotHeatmap contains ANSI background codes" $
        "\ESC[48;5;" `isInfixOf` render (plotHeatmap
          (HeatmapData [[10,90]] ["R"] ["A","B"])) @?= True

    , testCase "plotHeatmap contains gradient legend" $
        let r = stripAnsiTest $ render $ plotHeatmap
              (HeatmapData [[0,100]] ["R"] ["A","B"])
        in ("0" `isInfixOf` r && "100" `isInfixOf` r) @?= True

    , testCase "plotHeatmap' custom cell width" $
        let r = render $ plotHeatmap' 10 (HeatmapData [[1]] ["R"] ["C"])
        in length r > length (render $ plotHeatmap (HeatmapData [[1]] ["R"] ["C"])) @?= True
    ]
  ]
