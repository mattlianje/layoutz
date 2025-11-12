{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Layoutz
import Data.List (isInfixOf)

-- Helper to strip ANSI codes for testing (re-export from Layoutz would be better, but this works)
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