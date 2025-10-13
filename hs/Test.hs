{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Layoutz

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
      
  , testCase "padding" $
      lines (render $ pad 2 $ text "Test") @?= ["        ", "        ", "  Test  ", "        ", "        "]
      
  , testCase "margin custom" $
      render (margin ">>>" [text "Line 1", text "Line 2"]) @?= ">>> Line 1\n>>> Line 2"
      
  , testCase "margin error" $
      render (marginError [text "Error message"]) @?= "[error] Error message"
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
      "╔═══════╗" `elem` lines (render $ statusCard' DoubleBorder "API" "UP") @?= True
      
  , testCase "box default border" $
      "┌──Title──┐" `elem` lines (render $ box "Title" [text "Content"]) @?= True
      
  , testCase "box double border" $
      "╔══Title══╗" `elem` lines (render $ box' DoubleBorder "Title" [text "Content"]) @?= True
  ]

-- Layout tests
layoutTests :: TestTree
layoutTests = testGroup "Layout"
  [ testCase "unordered list single level" $
      render (ul [text "Item 1", text "Item 2"]) @?= "• Item 1\n• Item 2"
      
  , testCase "unordered list nested" $
      "  ◦ Sub 1" `elem` lines (render $ ul [text "Item 1", ul [text "Sub 1", text "Sub 2"]]) @?= True
      
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
  ]

-- Dimension tests
dimensionTests :: TestTree
dimensionTests = testGroup "Dimensions"
  [ testCase "width calculation" $
      width (text "Hello") @?= 5
      
  , testCase "height calculation" $
      height (layout [text "Line 1", text "Line 2"]) @?= 2
  ]