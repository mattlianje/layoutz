{-# LANGUAGE OverloadedStrings #-}

{-|
Showcase demo – tours every layoutz element and visualization.

Controls:
- ←/→   switch scenes (1–7)
- 1–7   jump to scene
- ESC    quit

Scene-specific keys are shown in the footer.
-}

module Main where

import Layoutz
import Text.Printf (printf)
import Data.Char (isAlphaNum)

-- State -----------------------------------------------------------------------

data ShowcaseState = ShowcaseState
  { scene         :: Int
  , tick          :: Int
  , textValue     :: String
  , stItems       :: [String]
  , addingItem    :: Bool
  , addTick       :: Int
  , selected      :: [Int]
  , cursor        :: Int
  , lineOffset    :: Int
  , tableRow      :: Int
  , tableSelected :: [Int]
  , barMode       :: Int
  , ballY         :: Double
  , ballVy        :: Double
  , gravity       :: Int
  , ballTrail     :: [Double]
  }

initialState :: ShowcaseState
initialState = ShowcaseState
  { scene = 0, tick = 0, textValue = "", stItems = []
  , addingItem = False, addTick = 0, selected = [], cursor = 0
  , lineOffset = 0, tableRow = 0, tableSelected = []
  , barMode = 0, ballY = 10.0, ballVy = 0.0, gravity = 5
  , ballTrail = replicate 80 10.0
  }

-- Messages --------------------------------------------------------------------

data Msg
  = NextScene | PrevScene | GoScene Int | Tick
  | TypeChar Char | Backspace | SubmitItem
  | ToggleSelect | CursorUp | CursorDown
  | AdjustUp | AdjustDown | ToggleBarMode | KickBall

-- Constants -------------------------------------------------------------------

totalScenes :: Int
totalScenes = 7

sceneNames :: [String]
sceneNames =
  [ "Bouncing Ball", "Text Input & Lists", "Borders & Styles"
  , "Tables", "Charts & Plots", "Bar Charts & Sparklines"
  , "Selections & Heatmap" ]

services :: [(String, String, String, String)]
services =
  [ ("API Gateway", "LIVE", "12ms", "99.99%")
  , ("Database",    "LIVE", "3ms",  "99.95%")
  , ("Cache",       "WARN", "1ms",  "98.50%")
  , ("Queue",       "LIVE", "8ms",  "99.90%")
  , ("Auth",        "LIVE", "5ms",  "99.97%")
  , ("CDN",         "LIVE", "2ms",  "99.99%")
  ]

sceneWidth :: Int
sceneWidth = 75

-- Helpers ---------------------------------------------------------------------

toggleIn :: Int -> [Int] -> [Int]
toggleIn x xs = if x `elem` xs then filter (/= x) xs else x : xs

-- Update ----------------------------------------------------------------------

update :: Msg -> ShowcaseState -> (ShowcaseState, Cmd Msg)
update msg s = case msg of
  NextScene -> (s { scene = (scene s + 1) `mod` totalScenes }, CmdNone)
  PrevScene -> (s { scene = (scene s - 1 + totalScenes) `mod` totalScenes }, CmdNone)
  GoScene n | n >= 0 && n < totalScenes -> (s { scene = n }, CmdNone)
  GoScene _ -> (s, CmdNone)

  Tick ->
    let -- Text input adding animation
        s1 = if addingItem s && addTick s >= 8
             then s { addingItem = False, addTick = 0
                    , stItems = stItems s ++ [textValue s], textValue = "" }
             else if addingItem s
             then s { addTick = addTick s + 1 }
             else s
        -- Ball physics
        g     = fromIntegral (gravity s1) * 0.08
        newVy = ballVy s1 - g
        rawY  = ballY s1 + newVy * 0.3
        (ny, vy) = bounce rawY newVy
        trail = drop (max 0 (length (ballTrail s1) - 79)) (ballTrail s1) ++ [ny]
    in (s1 { tick = tick s1 + 1, ballY = ny, ballVy = vy, ballTrail = trail }, CmdNone)

  TypeChar c | not (addingItem s) -> (s { textValue = textValue s ++ [c] }, CmdNone)
  TypeChar _ -> (s, CmdNone)

  Backspace | not (addingItem s) ->
    (s { textValue = if null (textValue s) then "" else init (textValue s) }, CmdNone)
  Backspace -> (s, CmdNone)

  SubmitItem | not (addingItem s) && not (null (textValue s)) ->
    (s { addingItem = True, addTick = 0 }, CmdNone)
  SubmitItem -> (s, CmdNone)

  ToggleSelect
    | scene s == 3 -> (s { tableSelected = toggleIn (tableRow s) (tableSelected s) }, CmdNone)
    | scene s == 6 -> (s { selected = toggleIn (cursor s) (selected s) }, CmdNone)
    | otherwise    -> (s, CmdNone)

  CursorUp
    | scene s == 3 -> (s { tableRow = (tableRow s - 1 + length services) `mod` length services }, CmdNone)
    | scene s == 6 -> (s { cursor = (cursor s - 1 + 7) `mod` 7 }, CmdNone)
    | otherwise    -> (s, CmdNone)

  CursorDown
    | scene s == 3 -> (s { tableRow = (tableRow s + 1) `mod` length services }, CmdNone)
    | scene s == 6 -> (s { cursor = (cursor s + 1) `mod` 7 }, CmdNone)
    | otherwise    -> (s, CmdNone)

  AdjustUp
    | scene s == 0 -> (s { gravity = min (gravity s + 1) 15 }, CmdNone)
    | otherwise    -> (s { lineOffset = min (lineOffset s + 1) 10 }, CmdNone)

  AdjustDown
    | scene s == 0 -> (s { gravity = max (gravity s - 1) 1 }, CmdNone)
    | otherwise    -> (s { lineOffset = max (lineOffset s - 1) (-10) }, CmdNone)

  ToggleBarMode -> (s { barMode = (barMode s + 1) `mod` 2 }, CmdNone)

  KickBall -> (s { ballVy = 5.0 }, CmdNone)

bounce :: Double -> Double -> (Double, Double)
bounce y vy
  | y <= 0                    = (0,  abs vy * 0.82)
  | y > 12                    = (12, -(abs vy * 0.5))
  | abs vy < 0.05 && y < 0.1 = (0, 0)
  | otherwise                 = (y, vy)

-- Subscriptions ---------------------------------------------------------------

subscriptions :: ShowcaseState -> Sub Msg
subscriptions s = subBatch
  [ subEveryMs 80 Tick
  , subKeyPress $ \key -> case key of
      KeyRight     -> Just NextScene
      KeyLeft      -> Just PrevScene
      KeyChar '+'  -> Just AdjustUp
      KeyChar '-'  -> Just AdjustDown
      KeyChar ' '  | scene s == 0                       -> Just KickBall
      KeyChar ' '  | scene s == 3 || scene s == 6       -> Just ToggleSelect
      KeyTab       | scene s == 5                        -> Just ToggleBarMode
      KeyEnter     | scene s == 1                        -> Just SubmitItem
      KeyUp        -> Just CursorUp
      KeyDown      -> Just CursorDown
      KeyBackspace -> Just Backspace
      KeyChar c    | scene s == 1 && (isAlphaNum c || c == ' ') -> Just (TypeChar c)
      KeyChar c    | c >= '1' && c <= '7' -> Just (GoScene (fromEnum c - fromEnum '1'))
      _            -> Nothing
  ]

-- View ------------------------------------------------------------------------

view :: ShowcaseState -> L
view s =
  let header  = renderHeader s
      content = case scene s of
        0 -> sceneBouncingBall s
        1 -> sceneTextInput s
        2 -> sceneBordersStyles s
        3 -> sceneTables s
        4 -> sceneChartsPlots s
        5 -> sceneBarChartsSparklines s
        6 -> sceneSelectionsHeatmap s
        _ -> text "Unknown scene"
      footer = renderFooter s
  in alignLeft sceneWidth $ render $ layout [header, br, content, br, footer]

renderHeader :: ShowcaseState -> L
renderHeader s =
  let sceneDots = unwords [ if i == scene s then "●" else "○" | i <- [0..totalScenes-1] ]
      prefix    = " ─── "
      title     = "layoutz"
      suffix    = show (scene s + 1) ++ " / " ++ show totalScenes
      dashCount = max 3 (sceneWidth - length prefix - length title - length suffix - 2)
      dashes    = replicate dashCount '─'
  in layout
       [ br
       , tightRow
           [ withColor ColorBrightBlack $ text prefix
           , withStyle StyleBold $ withColor ColorBrightCyan $ text title
           , withColor ColorBrightBlack $ text (" " ++ dashes ++ " ")
           , withColor ColorBrightBlack $ text suffix
           ]
       , br
       , withStyle StyleBold $ withColor ColorBrightYellow $ text (" " ++ sceneNames !! scene s)
       , text (" " ++ sceneDots)
       ]

renderFooter :: ShowcaseState -> L
renderFooter s =
  let hints = case scene s of
        0 -> "  </> scenes  Space kick  +/- gravity  ESC quit"
        1 -> "  </> scenes  type + Enter to add  ESC quit"
        3 -> "  </> scenes  ^/v navigate  Space select  ESC quit"
        4 -> "  </> scenes  +/- move threshold  ESC quit"
        5 -> "  </> scenes  Tab cycle chart mode  ESC quit"
        6 -> "  </> scenes  ^/v navigate  Space toggle  ESC quit"
        _ -> "  </> scenes  ESC quit"
  in withStyle StyleDim $ withColor ColorBrightBlack $ text hints

-- Scene 1: Bouncing Ball ------------------------------------------------------

sceneBouncingBall :: ShowcaseState -> L
sceneBouncingBall s =
  let trailPoints = zip (map fromIntegral [0 :: Int ..]) (ballTrail s)
      gLabel   = printf "g = %.2f" (fromIntegral (gravity s) * 0.08 :: Double) :: String
      velLabel = printf "vy = %.1f" (ballVy s) :: String
      yLabel   = printf "y = %.1f"  (ballY s) :: String
      bounds   = [(0.0, 0.0), (0.0, 12.0)]
      energy   = min 1.0 ((abs (ballVy s) + ballY s) / 15.0)
      barW     = 14 :: Int
      filled   = floor (energy * fromIntegral barW) :: Int
      pct      = floor (energy * 100) :: Int
      energyBar = "Energy " ++ replicate filled '█' ++ replicate (barW - filled) '░'
                  ++ " " ++ show pct ++ "%"
  in row
    [ layout
        [ withColor ColorBrightYellow $ text "Trajectory"
        , plotLine 35 12
            [ Series trailPoints "ball" ColorBrightCyan
            , Series bounds " " ColorBrightBlack
            ]
        ]
    , withColor ColorBrightMagenta $ withBorder BorderRound $ box "Physics"
        [ alignLeft 28 $ render $ layout
            [ kv [ ("gravity", gLabel), ("velocity", velLabel), ("height", yLabel) ]
            , br
            , withColor ColorBrightGreen $ text energyBar
            , br
            , withColor ColorBrightCyan $ spinner "Simulating" (tick s `div` 3) SpinnerDots
            , withStyle StyleBold $ withColor ColorBrightYellow $ text "Space to kick!"
            ]
        ]
    ]

-- Scene 2: Text Input & Lists ------------------------------------------------

sceneTextInput :: ShowcaseState -> L
sceneTextInput s =
  let inputLine =
        if addingItem s then
          row [ withColor ColorBrightYellow $ spinner "Adding" (addTick s) SpinnerDots
              , withColor ColorBrightYellow $ text ("  \"" ++ textValue s ++ "\"")
              ]
        else
          let display = if null (textValue s)
                        then withColor ColorBrightBlack $ text "Type something..."
                        else withColor ColorBrightWhite $ text (textValue s)
          in tightRow [ withColor ColorBrightCyan $ text "> ", display, withStyle StyleBlink $ text "_" ]

      itemColors = [ColorBrightGreen, ColorBrightBlue, ColorBrightMagenta, ColorBrightYellow, ColorBrightCyan]
      itemList =
        if null (stItems s) then
          withColor ColorBrightBlack $ text "  (no items yet)"
        else
          layout $ zipWith (\i item ->
            tightRow
              [ withColor ColorBrightBlack $ text ("  " ++ show (i + 1) ++ ". ")
              , withColor (itemColors !! (i `mod` length itemColors)) $ text item
              ]
            ) [0 :: Int ..] (stItems s)

      boxW     = 32
      its      = stItems s
      longest  = if null its then "-" else foldl1 (\a b -> if length a >= length b then a else b) its
      shortest = if null its then "-" else foldl1 (\a b -> if length a <= length b then a else b) its
      cnt      = length its
  in row
    [ withColor ColorBrightCyan $ withBorder BorderRound $ box "Add Items"
        [ alignLeft boxW $ render inputLine
        , br
        , alignLeft boxW $ render $ layout
            [ withStyle StyleBold $ text "Items:"
            , itemList
            ]
        ]
    , withBorder BorderRound $ box "Stats"
        [ alignLeft boxW $ render $ layout
            [ tightRow [ text "Total items: ", withStyle StyleBold $ withColor ColorBrightCyan $ text (show cnt) ]
            , tightRow [ text "Longest:     ", withColor ColorBrightMagenta $ text longest ]
            , tightRow [ text "Shortest:    ", withColor ColorBrightMagenta $ text shortest ]
            ]
        , br
        , alignLeft boxW $ render $
            if cnt >= 3
            then withStyle StyleBold $ withColor ColorBrightGreen $ text "Nice collection!"
            else withColor ColorBrightBlack $ text ("Add " ++ show (3 - cnt) ++ " more...")
        ]
    ]

-- Scene 3: Borders & Styles --------------------------------------------------

sceneBordersStyles :: ShowcaseState -> L
sceneBordersStyles _ =
  let mkBox (bdr, name, c) = withColor c $ withBorder bdr $ box name [alignLeft 8 name]
      topRow    = [ (BorderNormal, "Single", ColorBrightCyan)
                  , (BorderDouble, "Double", ColorBrightMagenta)
                  , (BorderRound,  "Round",  ColorBrightGreen) ]
      bottomRow = [ (BorderThick,  "Thick",  ColorBrightYellow)
                  , (BorderDashed, "Dashed", ColorBrightBlue)
                  , (BorderAscii,  "Ascii",  ColorBrightWhite) ]
  in layout
    [ withStyle StyleBold $ withColor ColorBrightYellow $ text "Border Styles"
    , row (map mkBox topRow)
    , row (map mkBox bottomRow)
    , br
    , withStyle StyleBold $ withColor ColorBrightYellow $ text "Text Styles"
    , row
        [ withBorder BorderRound $ box "Standard"
            [ withStyle StyleBold      $ withColor ColorBrightCyan    $ text "Bold"
            , withStyle StyleItalic    $ withColor ColorBrightMagenta $ text "Italic"
            , withStyle StyleUnderline $ withColor ColorBrightGreen   $ text "Underline"
            ]
        , withBorder BorderRound $ box "Extended"
            [ withStyle StyleDim           $ withColor ColorBrightYellow $ text "Dim"
            , withStyle StyleStrikethrough $ withColor ColorBrightRed    $ text "Strikethrough"
            , withStyle (StyleBold <> StyleItalic) $ withColor ColorBrightWhite $ text "Bold+Italic"
            ]
        ]
    ]

-- Scene 4: Tables -------------------------------------------------------------

sceneTables :: ShowcaseState -> L
sceneTables s =
  let coloredRows = zipWith (\idx (name, status, lat, up) ->
        let isActive = idx == tableRow s
            isSel    = idx `elem` tableSelected s
            mark     = if isSel then "* " else "  "
            cells    = [mark ++ name, status, lat, up]
            applyStyle
              | isActive && isSel = withStyle (StyleBold <> StyleReverse) . withColor ColorBrightGreen
              | isActive          = withStyle (StyleBold <> StyleReverse) . withColor ColorBrightCyan
              | isSel             = withColor ColorBrightGreen
              | otherwise         = id
        in map (applyStyle . text) cells
        ) [0 :: Int ..] services

      selCount = length (tableSelected s)
      selInfo  = if selCount > 0
                 then withColor ColorBrightGreen $ text (show selCount ++ " selected")
                 else withColor ColorBrightBlack $ text "none selected"
  in layout
    [ withBorder BorderRound $ table ["Service", "Status", "Latency", "Uptime"] coloredRows
    , tightRow
        [ withColor ColorBrightBlack $ text (" Row " ++ show (tableRow s + 1) ++ "/" ++ show (length services) ++ "  |  ")
        , selInfo
        ]
    ]

-- Scene 5: Charts & Plots ----------------------------------------------------

sceneChartsPlots :: ShowcaseState -> L
sceneChartsPlots s =
  let sinPoints  = [ (x, sin (x + fromIntegral (tick s) * 0.06) * 4)
                   | i <- [0..100 :: Int], let x = fromIntegral i * 0.08 ]
      intercept' = fromIntegral (lineOffset s) * 0.5
      linePoints = [ (x, 0.5 * x + intercept')
                   | i <- [0..100 :: Int], let x = fromIntegral i * 0.08 ]
      sign       = if intercept' >= 0 then "+" else "-" :: String
      lineLabel  = printf "0.5x %s %.1f" sign (abs intercept') :: String
  in row
    [ layout
        [ withColor ColorBrightYellow $ text ("sin(x) & y = " ++ lineLabel ++ "  [+/- to shift]")
        , plotLine 35 12
            [ Series sinPoints  "sin(x)" ColorBrightCyan
            , Series linePoints "linear" ColorBrightYellow
            ]
        ]
    , layout
        [ withColor ColorBrightYellow $ text "Revenue Share"
        , plotPie 30 8
            [ Slice 45 "Product"   ColorBrightCyan
            , Slice 30 "Services"  ColorBrightMagenta
            , Slice 15 "Licensing" ColorBrightYellow
            , Slice 10 "Other"     ColorBrightGreen
            ]
        ]
    ]

-- Scene 6: Bar Charts & Sparklines -------------------------------------------

sceneBarChartsSparklines :: ShowcaseState -> L
sceneBarChartsSparklines s =
  let sparkData = [ sin (fromIntegral (i + tick s) * 0.3) * 10 + 15
                  | i <- [0..29 :: Int] ]
      modeName  = if barMode s == 0 then "Vertical Bars" else "Stacked Bars"
      chartElem = case barMode s of
        0 -> plotBar 30 8
              [ BarItem 85  "Mon" ColorBrightCyan
              , BarItem 120 "Tue" ColorBrightGreen
              , BarItem 95  "Wed" ColorBrightMagenta
              , BarItem 110 "Thu" ColorBrightYellow
              , BarItem 75  "Fri" ColorBrightBlue
              ]
        _ -> plotStackedBar 30 8
              [ StackedBarGroup [ BarItem 50 "Online" ColorBrightCyan
                                , BarItem 35 "Retail" ColorBrightGreen
                                , BarItem 15 "Other"  ColorBrightMagenta ] "Q1"
              , StackedBarGroup [ BarItem 70 "Online" ColorBrightCyan
                                , BarItem 30 "Retail" ColorBrightGreen
                                , BarItem 20 "Other"  ColorBrightMagenta ] "Q2"
              , StackedBarGroup [ BarItem 45 "Online" ColorBrightCyan
                                , BarItem 55 "Retail" ColorBrightGreen
                                , BarItem 10 "Other"  ColorBrightMagenta ] "Q3"
              , StackedBarGroup [ BarItem 60 "Online" ColorBrightCyan
                                , BarItem 40 "Retail" ColorBrightGreen
                                , BarItem 25 "Other"  ColorBrightMagenta ] "Q4"
              ]
  in row
    [ layout
        [ withColor ColorBrightYellow $ text "Live Signal"
        , withColor ColorBrightCyan $ plotSparkline sparkData
        ]
    , layout
        [ withColor ColorBrightYellow $ text (modeName ++ "  [Tab to cycle]")
        , chartElem
        ]
    ]

-- Scene 7: Selections & Heatmap ----------------------------------------------

sceneSelectionsHeatmap :: ShowcaseState -> L
sceneSelectionsHeatmap s =
  let days  = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
      hours = ["6am", "9am", "12pm", "3pm", "6pm", "9pm"]

      selectorLines = zipWith (\idx day ->
        let isSel = idx `elem` selected s
            isCur = cursor s == idx
            check = if isSel then "[x]" else "[ ]"
            arrow = if isCur then "> " else "  "
            label = arrow ++ check ++ " " ++ day
            applyStyle
              | isCur && isSel = withStyle StyleBold . withColor ColorBrightGreen
              | isCur          = withStyle StyleBold . withColor ColorBrightCyan
              | isSel          = withColor ColorBrightGreen
              | otherwise      = id
        in applyStyle $ text label
        ) [0 :: Int ..] days

      selCount = length (selected s)
      baseData =
        [ [10, 45, 80, 75, 50, 15]
        , [12, 50, 85, 70, 55, 20]
        , [ 8, 40, 90, 80, 60, 25]
        , [15, 55, 75, 65, 45, 18]
        , [10, 48, 70, 60, 35, 30]
        , [ 5, 15, 25, 30, 40, 55]
        , [ 3, 10, 20, 25, 35, 45]
        ]
      heatData = if null (selected s) then baseData
                 else zipWith (\idx r ->
                   if idx `elem` selected s then r else map (* 0.15) r
                 ) [0 :: Int ..] baseData
  in row
    [ withColor ColorBrightCyan $ withBorder BorderRound $ box "Schedule"
        [ layout selectorLines
        , br
        , withColor (if selCount > 0 then ColorBrightGreen else ColorBrightBlack) $
            text (show selCount ++ " of " ++ show (length days) ++ " active")
        ]
    , withBorder BorderRound $ box "Weekly Activity"
        [ plotHeatmap' 5 (HeatmapData heatData days hours) ]
    ]

-- App -------------------------------------------------------------------------

showcaseApp :: LayoutzApp ShowcaseState Msg
showcaseApp = LayoutzApp
  { appInit          = (initialState, CmdNone)
  , appUpdate        = \msg s -> update msg s
  , appSubscriptions = subscriptions
  , appView          = view
  }

main :: IO ()
main = runAppWith defaultAppOptions { optAlignment = AppAlignCenter } showcaseApp
