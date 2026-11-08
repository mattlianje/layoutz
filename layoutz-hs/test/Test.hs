{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Layoutz
import Data.List (isInfixOf, inits)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)

stripAnsiTest :: String -> String
stripAnsiTest [] = []
stripAnsiTest ('\ESC':'[':rest) = stripAnsiTest (dropAfterM rest)
  where
    dropAfterM [] = []
    dropAfterM ('m':xs) = xs
    dropAfterM (_:xs) = dropAfterM xs
stripAnsiTest (c:rest) = c : stripAnsiTest rest

renderS :: View m -> String
renderS = render

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
  , gapClipBannerTests
  , eventTests
  , sizingTests
  , focusTests
  , widgetTests
  , inputTests
  ]

-- gap / clip / banner
gapClipBannerTests :: TestTree
gapClipBannerTests = testGroup "Gap, Clip & Banner"
  [ testCase "gap 2 places stacks side by side" $
      render (row [vstack ["A", "B"], vstack ["C", "D"]] & gap 2) @?= "A  C\nB  D"

  , testCase "gap sets row spacing" $
      render (row [vstack ["A"], vstack ["B"]] & gap 1) @?= "A B"

  , testCase "clip leaves short text untouched" $
      render (clip 15 (text "short")) @?= "short"

  , testCase "clip cuts long text with the default ellipsis" $
      render (clip 15 (text "Very long text that will be cut off")) @?= "Very long te..."

  , testCase "clip with a custom ellipsis" $
      render (text "Custom ellipsis example text here" & clip 20 & ellipsis "…") @?= "Custom ellipsis exa…"

  , testCase "banner is a titleless double-bordered box" $
      render (banner ["System Dashboard"]) @?= "╔══════════════════╗\n║ System Dashboard ║\n╚══════════════════╝"
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
      render (text "Test" & underline & glyph "=") @?= "Test\n===="
      
  , testCase "center default width" $
      render (vstack [text "wide content here to establish context width", center $ text "Test"]) @?= "wide content here to establish context width\n                    Test                    "
      
  , testCase "center custom width" $
      render (centerIn 10 $ text "Test") @?= "   Test   "
  ]

-- Visual feature tests
visualFeatureTests :: TestTree
visualFeatureTests = testGroup "Visual Features"
  [ testCase "horizontal rule default" $
      render hr @?= "──────────────────────────────────────────────────"
      
  , testCase "horizontal rule custom char" $
      render (hr & glyph "=") @?= "=================================================="
      
  , testCase "horizontal rule custom width" $
      render (hr & glyph "-" & size 10) @?= "----------"
      
  , testCase "vertical rule default" $
      length (lines $ renderS vr) @?= 10
      
  , testCase "vertical rule custom char" $
      head (lines $ renderS $ vr & glyph "║") @?= "║"
      
  , testCase "vertical rule custom height" $
      length (lines $ renderS $ vr & glyph "|" & size 5) @?= 5
      
  , testCase "padding" $
      lines (renderS $ pad 2 $ text "Test") @?= ["        ", "        ", "  Test  ", "        ", "        "]
      
  , testCase "margin custom" $
      render (vstack [text "Line 1", text "Line 2"] & margin ">>>") @?= ">>> Line 1\n>>> Line 2"
      
  , testCase "margin error" $
      render (vstack [text "Error message"] & margin "[error]") @?= "[error] Error message"
  ]

-- Data visualization tests
dataVisualizationTests :: TestTree
dataVisualizationTests = testGroup "Data Visualization"
  [ testCase "inline bar 0%" $
      "Test [────────────────────] 0%" `elem` lines (renderS $ inlineBar "Test" 0.0) @?= True
      
  , testCase "inline bar 50%" $
      "Test [██████████──────────] 50%" `elem` lines (renderS $ inlineBar "Test" 0.5) @?= True
      
  , testCase "inline bar 100%" $
      "Test [████████████████████] 100%" `elem` lines (renderS $ inlineBar "Test" 1.0) @?= True
      
  , testCase "chart" $
      "Task │████████████████████████████████████████│ 50" `elem` lines (renderS $ chart [("Task", 50.0)]) @?= True
      
  , testCase "key-value pairs" $
      render (kv [("Key", "Value")]) @?= "Key: Value"
  ]

-- Container tests
containerTests :: TestTree
containerTests = testGroup "Containers"
  [ testCase "status card default border" $
      length (lines $ renderS $ statusCard "API" "UP") @?= 4
      
  , testCase "status card double border" $
      "╔═══════╗" `elem` lines (renderS $ border BorderDouble $ statusCard "API" "UP") @?= True
      
  , testCase "box default border" $
      "┌──Title──┐" `elem` lines (renderS $ box "Title" [text "Content"]) @?= True
      
  , testCase "box double border" $
      "╔══Title══╗" `elem` lines (renderS $ border BorderDouble $ box "Title" [text "Content"]) @?= True
  ]

-- Layout tests
layoutTests :: TestTree
layoutTests = testGroup "Layout"
  [ testCase "unordered list single level" $
      render (ul [text "Item 1", text "Item 2"]) @?= "• Item 1\n• Item 2"
      
  , testCase "unordered list nested" $
      "  ◦ Sub 1" `elem` lines (renderS $ ul [text "Item 1", ul [text "Sub 1", text "Sub 2"]]) @?= True
      
  , testCase "ordered list single level" $
      render (ol [text "First", text "Second"]) @?= "1. First\n2. Second"
      
  , testCase "ordered list nested with letters" $
      "  a. Nested" `elem` lines (renderS $ ol [text "Item 1", ol [text "Nested", text "Also nested"]]) @?= True
      
  , testCase "ordered list triple nested with roman numerals" $
      "    i. Deep" `elem` lines (renderS $ ol [text "L1", ol [text "L2", ol [text "Deep"]]]) @?= True
      
  , testCase "table basic" $
      "│ A │ B │" `elem` lines (renderS $ table ["A", "B"] [[text "1", text "2"]]) @?= True
      
  , testCase "tree structure" $
      "Root" `elem` lines (renderS $ tree "Root" [leaf "Child"]) @?= True
      
  , testCase "section" $
      "=== Title ===" `elem` lines (renderS $ section "Title" [text "Content"]) @?= True
      
  , testCase "row spacing" $
      render (row [text "A", text "B"]) @?= "A B"
      
  , testCase "settings reach through colour" $
      render (hr & fg ColorRed & size 5) @?= render (hr & size 5 & fg ColorRed)

  , testCase "vstack composition" $
      render (vstack [text "Line 1", text "Line 2"]) @?= "Line 1\nLine 2"
      
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
      height (vstack [text "Line 1", text "Line 2"]) @?= 2
  ]

-- Color tests
colorTests :: TestTree
colorTests = testGroup "Colors"
  [ testCase "renderS includes ANSI codes" $
      "\ESC[31m" `isInfixOf` renderS (fg ColorRed $ text "Hello") @?= True
      
  , testCase "renderS includes reset code" $
      "\ESC[0m" `isInfixOf` renderS (fg ColorRed $ text "Hello") @?= True
      
  , testCase "colored element width ignores ANSI codes" $
      width (fg ColorRed $ text "Hello") @?= 5
      
  , testCase "colored status card maintains structure" $
      let colored = renderS $ fg ColorGreen $ border BorderDouble $ statusCard "API" "UP"
          strippedLines = map stripAnsiTest (lines colored)
      in "╔═══════╗" `elem` strippedLines @?= True
      
  , testCase "colored box maintains structure" $
      let colored = renderS $ fg ColorBlue $ box "Title" [text "Content"]
          strippedLines = map stripAnsiTest (lines colored)
      in "┌──Title──┐" `elem` strippedLines @?= True
      
  , testCase "nested colors work" $
      "\ESC[32m" `isInfixOf` renderS (fg ColorRed $ vstack [fg ColorGreen $ text "Hi"]) @?= True
      
  , testCase "bright colors use correct codes" $
      "\ESC[91m" `isInfixOf` renderS (fg ColorBrightRed $ text "Hi") @?= True
      
  , testCase "color with borders both work" $
      let colored = renderS $ fg ColorYellow $ border BorderThick $ statusCard "Test" "OK"
          strippedLines = map stripAnsiTest (lines colored)
      in "┏━━━━━━━━┓" `elem` strippedLines @?= True
      
  , testCase "colored underline includes ANSI codes" $
      "\ESC[31m" `isInfixOf` renderS (text "Title" & underline & glyph "=" & accent ColorRed) @?= True
      
  , testCase "colored underline maintains structure" $
      let rendered = renderS $ accent ColorRed . glyph "=" . underline $ text "Test"
          strippedLines = map stripAnsiTest (lines rendered)
      in "====" `elem` strippedLines @?= True
  ]

-- Extended color tests (256-color and RGB)
extendedColorTests :: TestTree
extendedColorTests = testGroup "Extended Colors"
  [ testCase "256-color palette (ColorFull)" $
      "\ESC[38;5;" `isInfixOf` renderS (fg (ColorFull 196) $ text "Red") @?= True
      
  , testCase "256-color clamping max" $
      "\ESC[38;5;255" `isInfixOf` renderS (fg (ColorFull 300) $ text "Clamped") @?= True
      
  , testCase "256-color clamping min" $
      "\ESC[38;5;0" `isInfixOf` renderS (fg (ColorFull (-10)) $ text "Clamped") @?= True
      
  , testCase "RGB true color (ColorTrue)" $
      "\ESC[38;2;" `isInfixOf` renderS (fg (ColorTrue 255 100 50) $ text "RGB") @?= True
      
  , testCase "RGB true color full spec" $
      "\ESC[38;2;255;100;50" `isInfixOf` renderS (fg (ColorTrue 255 100 50) $ text "RGB") @?= True
      
  , testCase "RGB color clamping" $
      "\ESC[38;2;255;255;0" `isInfixOf` renderS (fg (ColorTrue 300 300 (-10)) $ text "Clamped") @?= True
      
  , testCase "gap 0 renders without spaces" $
      stripAnsiTest (renderS $ row [fg ColorRed $ text "A", fg ColorGreen $ text "B"] & gap 0) @?= "AB"
      
  , testCase "gap 0 vs default spacing" $
      let tightResult = stripAnsiTest (renderS $ row [text "A", text "B"] & gap 0)
          normalResult = renderS $ row [text "A", text "B"]
      in (tightResult == "AB" && normalResult == "A B") @?= True
  ]

-- Style tests (including combining)
styleTests :: TestTree
styleTests = testGroup "Styles"
  [ testCase "bold style" $
      "\ESC[1m" `isInfixOf` renderS (styleBold $ text "Bold") @?= True
      
  , testCase "italic style" $
      "\ESC[3m" `isInfixOf` renderS (styleItalic $ text "Italic") @?= True
      
  , testCase "stacked styles merge into one SGR" $
      renderS (text "Fancy" & styleBold & styleItalic) @?= "\ESC[1;3mFancy\ESC[0m"
      
  , testCase "triple combined styles" $
      let combined = renderS $ (styleBold . styleItalic . styleUnderline) $ text "Very Fancy"
          hasAll = "\ESC[" `isInfixOf` combined && "1" `isInfixOf` combined && "3" `isInfixOf` combined && "4" `isInfixOf` combined
      in hasAll @?= True
      
  , testCase "style with color both work" $
      let styled = renderS $ fg ColorRed $ styleBold $ text "Important"
      in ("\ESC[31m" `isInfixOf` styled && "\ESC[1m" `isInfixOf` styled) @?= True
      
  , testCase "style maintains element width" $
      width (styleBold $ text "Hello") @?= 5
      
  , testCase "combined style maintains structure" $
      let styled = renderS $ (styleBold . styleReverse) $ box "Title" [text "Content"]
          strippedLines = map stripAnsiTest (lines styled)
      in "┌──Title──┐" `elem` strippedLines @?= True
      
  , testCase "wrap text at word boundaries" $
      let wrapped = renderS $ wrap 10 "This is a very long text"
          wrappedLines = lines wrapped
      in length wrappedLines > 1 @?= True
      
  , testCase "wrap preserves words" $
      "This is a" `isInfixOf` renderS (wrap 10 "This is a test") @?= True
  ]

-- Command execution tests
commandTests :: TestTree
commandTests = testGroup "Commands"
  [ testCase "cmdNone produces nothing" $ do
      result <- executeCmd (cmdNone :: Cmd String)
      result @?= []

  , testCase "cmdFire executes IO without message" $ do
      ref <- newIORef (0 :: Int)
      result <- executeCmd (cmdFire $ writeIORef ref 42 :: Cmd String)
      val <- readIORef ref
      result @?= []
      val @?= 42

  , testCase "cmdTask executes IO and returns message" $ do
      result <- executeCmd (cmdTask $ pure "hello" :: Cmd String)
      result @?= ["hello"]

  , testCase "<> runs effects in order" $ do
      ref <- newIORef (0 :: Int)
      _ <- executeCmd (cmdFire (writeIORef ref 1) <> cmdFire (writeIORef ref 2) :: Cmd String)
      val <- readIORef ref
      val @?= 2

  , testCase "<> delivers every message" $ do
      result <- executeCmd (mconcat [cmdFire (pure ()), cmdTask (pure "first"), cmdTask (pure "second")] :: Cmd String)
      result @?= ["first", "second"]

  , testCase "fmap maps messages" $ do
      result <- executeCmd (length <$> cmdTask (pure ("four" :: String)))
      result @?= [4]
  ]

-- Visualization primitive tests
visualizationTests :: TestTree
visualizationTests = testGroup "Visualizations"
  [ testGroup "Renames"
    [ testCase "ColorDefault produces no ANSI wrapping" $
        render (fg ColorDefault $ text "Hello") @?= "Hello"

    ]

  , testGroup "Sparkline"
    [ testCase "sparkline output length matches input" $
        length (renderS $ plotSparkline [1,2,3,4,5]) @?= 5

    , testCase "sparkline empty input" $
        render (plotSparkline []) @?= ""

    , testCase "sparkline constant values use middle block" $
        all (== '▄') (renderS $ plotSparkline [5,5,5]) @?= True

    , testCase "sparkline max value is full block" $
        last (renderS $ plotSparkline [0,0,10]) @?= '█'

    , testCase "sparkline min value is lowest block" $
        head (renderS $ plotSparkline [0,5,10]) @?= '▁'
    ]

  , testGroup "Line Plot"
    [ testCase "plotLine no data" $
        render (plotLine 20 5 []) @?= "No data"

    , testCase "plotLine empty series" $
        render (plotLine 20 5 [Series [] "empty" ColorBrightCyan]) @?= "No data"

    , testCase "plotLine contains y-axis" $
        "│" `isInfixOf` renderS (plotLine 20 5 [Series [(0,0),(1,1)] "test" ColorBrightCyan]) @?= True

    , testCase "plotLine contains x-axis" $
        "─" `isInfixOf` renderS (plotLine 20 5 [Series [(0,0),(1,1)] "test" ColorBrightCyan]) @?= True

    , testCase "plotLine multi-series has legend" $
        let r = renderS $ plotLine 20 5
              [ Series [(0,0),(1,1)] "A" ColorBrightCyan
              , Series [(0,1),(1,0)] "B" ColorBrightRed ]
        in ("A" `isInfixOf` r && "B" `isInfixOf` r) @?= True

    , testCase "plotLine single series no legend" $
        let r = renderS $ plotLine 20 5 [Series [(0,0),(1,1)] "only" ColorBrightCyan]
        in not ("●" `isInfixOf` stripAnsiTest r) @?= True

    , testCase "plotLine contains braille characters" $
        let r = stripAnsiTest $ renderS $ plotLine 30 8 [Series [(x, sin x) | x <- [0,0.1..6.28]] "sin" ColorBrightCyan]
        in any (\c -> c >= '⠀' && c <= '⣿') r @?= True
    ]

  , testGroup "Pie Chart"
    [ testCase "plotPie no data" $
        render (plotPie 10 5 []) @?= "No data"

    , testCase "plotPie legend has percentages" $
        let r = renderS $ plotPie 15 6 [Slice 75 "Big" ColorBrightCyan, Slice 25 "Small" ColorBrightMagenta]
        in ("75%" `isInfixOf` stripAnsiTest r && "25%" `isInfixOf` stripAnsiTest r) @?= True

    , testCase "plotPie legend has labels" $
        let r = stripAnsiTest $ renderS $ plotPie 15 6
              [Slice 50 "Alpha" ColorBrightCyan, Slice 50 "Beta" ColorBrightMagenta]
        in ("Alpha" `isInfixOf` r && "Beta" `isInfixOf` r) @?= True

    , testCase "plotPie contains braille characters" $
        let r = stripAnsiTest $ renderS $ plotPie 20 8
              [Slice 60 "A" ColorBrightCyan, Slice 40 "B" ColorBrightMagenta]
        in any (\c -> c >= '⠀' && c <= '⣿') r @?= True
    ]

  , testGroup "Bar Chart"
    [ testCase "plotBar no data" $
        render (plotBar 20 5 []) @?= "No data"

    , testCase "plotBar contains y-axis" $
        "│" `isInfixOf` renderS (plotBar 20 5 [BarItem 10 "A" ColorBrightCyan]) @?= True

    , testCase "plotBar contains bar labels" $
        let r = stripAnsiTest $ renderS $ plotBar 30 5
              [BarItem 10 "Mon" ColorBrightCyan, BarItem 20 "Tue" ColorBrightGreen]
        in ("Mon" `isInfixOf` r && "Tue" `isInfixOf` r) @?= True

    , testCase "plotBar contains block characters" $
        let r = stripAnsiTest $ renderS $ plotBar 20 5 [BarItem 100 "X" ColorBrightCyan]
        in any (\c -> c `elem` ("▁▂▃▄▅▆▇█" :: String)) r @?= True

    , testCase "plotBar y-axis shows max value" $
        let r = stripAnsiTest $ renderS $ plotBar 30 5 [BarItem 50 "A" ColorBrightCyan]
        in "50" `isInfixOf` r @?= True
    ]

  , testGroup "Stacked Bar Chart"
    [ testCase "plotStackedBar no data" $
        render (plotStackedBar 20 5 []) @?= "No data"

    , testCase "plotStackedBar contains group labels" $
        let r = stripAnsiTest $ renderS $ plotStackedBar 30 5
              [ StackedBarGroup [BarItem 10 "X" ColorDefault] "G1"
              , StackedBarGroup [BarItem 20 "X" ColorDefault] "G2" ]
        in ("G1" `isInfixOf` r && "G2" `isInfixOf` r) @?= True

    , testCase "plotStackedBar multi-segment has legend" $
        let r = stripAnsiTest $ renderS $ plotStackedBar 30 5
              [ StackedBarGroup [BarItem 10 "Sales" ColorDefault, BarItem 5 "Tax" ColorDefault] "Q1" ]
        in ("Sales" `isInfixOf` r && "Tax" `isInfixOf` r) @?= True

    , testCase "plotStackedBar single segment no legend" $
        let r = stripAnsiTest $ renderS $ plotStackedBar 30 5
              [ StackedBarGroup [BarItem 10 "Only" ColorDefault] "Q1"
              , StackedBarGroup [BarItem 20 "Only" ColorDefault] "Q2" ]
        in not ("█ Only" `isInfixOf` r) @?= True

    , testCase "plotStackedBar legend uses explicit segment colors" $
        let r = renderS $ plotStackedBar 30 5
              [ StackedBarGroup [ BarItem 10 "Red" ColorRed
                                , BarItem 5 "Blue" ColorBlue ] "Q1" ]
        in (   "\ESC[31m█\ESC[0m Red" `isInfixOf` r
            && "\ESC[34m█\ESC[0m Blue" `isInfixOf` r) @?= True

    , testCase "plotStackedBar legend falls back to palette color" $
        let r = renderS $ plotStackedBar 30 5
              [ StackedBarGroup [ BarItem 10 "A" ColorDefault
                                , BarItem 5 "B" ColorDefault ] "Q1" ]
        in (   "\ESC[96m█\ESC[0m A" `isInfixOf` r
            && "\ESC[95m█\ESC[0m B" `isInfixOf` r) @?= True
    ]

  , testGroup "Heatmap"
    [ testCase "plotHeatmap no data" $
        render (plotHeatmap (HeatmapData [] [] [])) @?= "No data"

    , testCase "plotHeatmap contains row labels" $
        let r = stripAnsiTest $ renderS $ plotHeatmap
              (HeatmapData [[1,2],[3,4]] ["Row1","Row2"] ["C1","C2"])
        in ("Row1" `isInfixOf` r && "Row2" `isInfixOf` r) @?= True

    , testCase "plotHeatmap contains column labels" $
        let r = stripAnsiTest $ renderS $ plotHeatmap
              (HeatmapData [[1,2],[3,4]] ["R1","R2"] ["ColA","ColB"])
        in ("ColA" `isInfixOf` r && "ColB" `isInfixOf` r) @?= True

    , testCase "plotHeatmap contains ANSI background codes" $
        "\ESC[48;5;" `isInfixOf` renderS (plotHeatmap
          (HeatmapData [[10,90]] ["R"] ["A","B"])) @?= True

    , testCase "plotHeatmap contains gradient legend" $
        let r = stripAnsiTest $ renderS $ plotHeatmap
              (HeatmapData [[0,100]] ["R"] ["A","B"])
        in ("0" `isInfixOf` r && "100" `isInfixOf` r) @?= True

    , testCase "plotHeatmap' custom cell width" $
        let r = renderS $ plotHeatmap' 10 (HeatmapData [[1]] ["R"] ["C"])
        in length r > length (renderS $ plotHeatmap (HeatmapData [[1]] ["R"] ["C"])) @?= True
    ]
  ]

data TMsg = Inc | Dec | Outer | Scrolled Int | Wrap TMsg
  deriving (Eq, Show)

eventTests :: TestTree
eventTests = testGroup "Events"
  [ testCase "click lands on the button under the cell" $ do
      let v = row [button "-" Dec, text " 3 ", button "+" Inc]
      clickAt (0, 1) v @?= Just Dec
      clickAt (0, 9) v @?= Just Inc
      clickAt (0, 5) v @?= Nothing

  , testCase "innermost handler wins, outer catches the rest" $ do
      let v = onClick Outer (box "" [button "+" Inc])
      clickAt (1, 3) v @?= Just Inc
      clickAt (0, 0) v @?= Just Outer
      clickAt (1, 0) v @?= Just Outer

  , testCase "scroll bubbles past click-only children" $ do
      let v = onScroll Scrolled (vstack [button "a" Inc, "b"])
      scrollAt 1 (0, 1) v @?= Just (Scrolled 1)
      scrollAt (-1) (1, 0) v @?= Just (Scrolled (-1))
      clickAt (0, 1) v @?= Just Inc

  , testCase "regions follow centring, colour and table cells" $ do
      clickAt (1, 9) (vstack [text (replicate 20 '-'), center (button "ok" Inc)]) @?= Just Inc
      clickAt (1, 2) (vstack [text (replicate 20 '-'), center (button "ok" Inc)]) @?= Nothing
      clickAt (0, 1) (fg ColorRed (button "x" Inc)) @?= Just Inc
      clickAt (3, 3) (table ["A"] [[button "go" Inc]]) @?= Just Inc

  , testCase "fmap re-targets messages for embedded components" $
      clickAt (0, 1) (fmap Wrap (button "x" Inc)) @?= Just (Wrap Inc)

  , testCase "handler-free views mix into views that send messages" $
      clickAt (1, 1) (vstack [text "title", button "x" Inc]) @?= Just Inc

  , testCase "handlers don't change the rendering" $ do
      render (row [button "-" Dec, "x"]) @?= render (row [styleReverse " - ", "x"])
      width (onClick Inc (box "t" ["abc"])) @?= width (box "t" ["abc"])
  ]

sizingTests :: TestTree
sizingTests = testGroup "Sizing"
  [ testCase "fill takes the leftover width of an row" $
      lines (renderIn 20 3 (row ["ab", box "" ["x"] & fill]))
        @?= [ "ab ┌───────────────┐"
            , "   │ x             │"
            , "   └───────────────┘" ]

  , testCase "fill takes the leftover height of a vstack" $
      lines (renderIn 7 5 (vstack ["top", box "" ["x"] & fill]))
        @?= [ "top"
            , "┌─────┐"
            , "│ x   │"
            , "│     │"
            , "└─────┘" ]

  , testCase "fill shares the room between fills" $
      map length (lines (renderIn 21 3 (row [box "" ["a"] & fill, box "" ["b"] & fill])))
        @?= [21, 21, 21]

  , testCase "without room, fill changes nothing" $
      render (box "" ["x"] & fill) @?= render (box "" ["x"])

  , testCase "fixedWidth pads or cuts to the width" $ do
      map length (lines (render (box "" ["x"] & fixedWidth 9))) @?= [9, 9, 9]
      render (text "abcdef" & fixedWidth 3) @?= "abc"

  , testCase "a fixed-width sidebar spans the row's height" $
      lines (renderIn 20 4 (row [box "" ["a"] & fixedWidth 5, box "" ["b"] & fill] & fill))
        @?= [ "┌───┐ ┌────────────┐"
            , "│ a │ │ b          │"
            , "│   │ │            │"
            , "└───┘ └────────────┘" ]

  , testCase "rules stretch under fill" $
      render (row [hr & fill] & fixedWidth 6) @?= "──────"

  , testCase "clicks cover the stretched area" $
      clickAt (1, 12) (row ["ab", box "" ["x"] & fill & onClick Inc] & fixedWidth 20) @?= Just Inc

  , testCase "clip and pad keep clicks" $ do
      clickAt (0, 1) (clip 3 (button "long" Inc)) @?= Just Inc
      clickAt (1, 2) (pad 1 (button "x" Inc)) @?= Just Inc
  ]

focusTests :: TestTree
focusTests = testGroup "Focus"
  [ testCase "Tab order is screen order" $
      focusOrder (vstack [text "a" & focusable "one", row [text "b" & focusable "two", text "c" & focusable "three"]])
        @?= ["one", "two", "three"]

  , testCase "keys go to the focused view" $ do
      let v = vstack [ text "a" & onKey (const (Just Inc)) & focusable "one"
                     , text "b" & onKey (const (Just Dec)) & focusable "two" ]
      pressKey (Just "one") (KeyChar 'x') v @?= Just Inc
      pressKey (Just "two") (KeyChar 'x') v @?= Just Dec
      pressKey Nothing (KeyChar 'x') v @?= Nothing

  , testCase "onKey outside focusable still belongs to it" $ do
      let v = text "a" & focusable "one" & onKey (const (Just Inc))
      pressKey (Just "one") KeyEnter v @?= Just Inc
      pressKey Nothing KeyEnter v @?= Nothing

  , testCase "the focused view gets first pick, the rest gets what it leaves" $ do
      let quitOn k = if k == KeyChar 'q' then Just Outer else Nothing
          v = vstack [text "a" & onKey (\k -> if k == KeyEnter then Just Inc else Nothing) & focusable "one"]
                & onKey quitOn
      pressKey (Just "one") KeyEnter v @?= Just Inc
      pressKey (Just "one") (KeyChar 'q') v @?= Just Outer

  , testCase "focusStyle doesn't show without focus" $
      render (text "x" & focusable "a" & focusStyle styleBold) @?= "x"
  ]

widgetTests :: TestTree
widgetTests = testGroup "Widgets"
  [ testCase "selectList draws a cursor" $
      lines (stripAnsiTest (render (fruits (selectListOf ["a", "b", "c"]))))
        @?= ["› a", "  b", "  c"]

  , testCase "selectList moves with keys, clicks and the wheel" $ do
      let sl = selectListOf ["a", "b", "c"]
          movedTo = fmap selectedItem
      movedTo (pressKey Nothing KeyDown (fruits sl)) @?= Just (Just "b")
      movedTo (pressKey Nothing KeyEnd (fruits sl)) @?= Just (Just "c")
      movedTo (clickAt (2, 1) (fruits sl)) @?= Just (Just "c")
      movedTo (scrollAt 1 (0, 0) (fruits sl)) @?= Just (Just "b")

  , testCase "selectList keeps the cursor in sight" $ do
      let sl = iterate (press KeyDown) (selectListOf (map show [1 .. 6 :: Int])) !! 4
      selectedItem sl @?= Just "5"
      map stripAnsiTest (lines (renderIn 6 2 (fruits sl & fill))) @?= ["  4  █", "› 5  │"]

  , testCase "viewport shows a window with a scrollbar" $
      lines (render (numbers viewportTop)) @?= ["1 █", "2 │", "3 │"]

  , testCase "viewport scrolls and sticks to the bottom" $ do
      let down = scrolled KeyDown viewportTop
          end  = scrolled KeyEnd viewportTop
      lines (render (numbers down)) @?= ["2 █", "3 │", "4 │"]
      end @?= viewportBottom
      lines (render (numbers end)) @?= ["8 │", "9 │", "10█"]
      lines (render (numbers (scrolled KeyUp end))) @?= ["7 │", "8 █", "9 │"]
  ]
  where
    fruits sl = selectList id (const sl) text sl
    press k sl = fromMaybe sl (pressKey Nothing k (fruits sl))
    numbers vp = viewport id vp (map (text . show) [1 .. 10 :: Int]) & fixedHeight 3
    scrolled k vp = fromMaybe vp (pressKey Nothing k (numbers vp))

-- parseInput
inputTests :: TestTree
inputTests = testGroup "Input parsing"
  [ testCase "each known sequence parses on its own" $
      mapM_ (\(bytes, want) -> (bytes, parseInput bytes) @?= (bytes, Just (want, ""))) sequences

  , testCase "every strict prefix of a sequence needs more input" $
      mapM_ (\p -> (p, parseInput p) @?= (p, Nothing))
        [ p | (bytes, _) <- sequences, p <- init (inits bytes) ]

  , testCase "back to back sequences split apart" $
      parseAll (concatMap fst sequences) @?= map snd sequences

  , testCase "leftover input comes back untouched" $
      parseInput "\ESC[Axyz" @?= Just (InputKey KeyUp, "xyz")

  , testCase "a lone ESC waits for the caller to decide" $
      parseInput "\ESC" @?= Nothing

  , testCase "nothing to parse" $
      parseInput "" @?= Nothing

  , testCase "modified keys we don't know are not Esc" $
      parseAll "\ESC[1;5A\ESC[15~" @?= [key (KeySpecial "[1;5A"), key (KeySpecial "[15~")]

  , testCase "a row-1 position report is modified F3, not a resize" $
      parseAll "\ESC[1;2R" @?= [key (KeySpecial "[1;2R")]

  , testCase "a broken mouse report is a special key" $
      parseAll "\ESC[<0;10M" @?= [key (KeySpecial "[<0;10")]
  ]
  where
    key = InputKey
    parseAll str = case parseInput str of
      Just (i, rest) -> i : parseAll rest
      Nothing        -> []
    sequences =
      [ ("a",            key (KeyChar 'a'))
      , (" ",            key (KeyChar ' '))
      , ("é",            key (KeyChar 'é'))
      , ("\r",           key KeyEnter)
      , ("\n",           key KeyEnter)
      , ("\t",           key KeyTab)
      , ("\DEL",         key KeyBackspace)
      , ("\b",           key KeyBackspace)
      , ("\SOH",         key (KeyCtrl 'A'))
      , ("\ETX",         key (KeyCtrl 'C'))
      , ("\ESC[A",       key KeyUp)
      , ("\ESC[B",       key KeyDown)
      , ("\ESC[C",       key KeyRight)
      , ("\ESC[D",       key KeyLeft)
      , ("\ESC[H",       key KeyHome)
      , ("\ESC[F",       key KeyEnd)
      , ("\ESC[Z",       key KeyBackTab)
      , ("\ESC[1~",      key KeyHome)
      , ("\ESC[3~",      key KeyDelete)
      , ("\ESC[4~",      key KeyEnd)
      , ("\ESC[5~",      key KeyPageUp)
      , ("\ESC[6~",      key KeyPageDown)
      , ("\ESCOH",       key KeyHome)
      , ("\ESCOF",       key KeyEnd)
      , ("\ESCOP",       key (KeySpecial "OP"))
      , ("\ESCx",        key (KeySpecial "x"))
      , ("\ESC[<0;10;5M", InputMouse (MouseEvent 0 (4, 9) True))
      , ("\ESC[<0;10;5m", InputMouse (MouseEvent 0 (4, 9) False))
      , ("\ESC[<65;1;1M", InputMouse (MouseEvent 65 (0, 0) True))
      , ("\ESC[60;200R",  InputSize 60 200)
      ]
