{-# LANGUAGE OverloadedStrings #-}

{-|
Showcase demo – tours every layoutz element and visualization.

Controls:
- ←/→   switch scenes (1–8)
- 1–8   jump to scene
- ESC    quit

Scene-specific keys are shown in the footer.
-}

module Main where

import Layoutz
import Text.Printf (printf)
import Data.Char (isAlphaNum)
import qualified Data.Text as T

-- State

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
  , rayTheta      :: Double
  , rayPhi        :: Double
  , rayDist       :: Double
  , rayMorph      :: Double
  , rayMorphTarget:: Double
  }

initialState :: ShowcaseState
initialState = ShowcaseState
  { scene = 0, tick = 0, textValue = "", stItems = []
  , addingItem = False, addTick = 0, selected = [], cursor = 0
  , lineOffset = 0, tableRow = 0, tableSelected = []
  , barMode = 0, ballY = 10.0, ballVy = 0.0, gravity = 5
  , ballTrail = replicate 80 10.0
  , rayTheta = 0.6, rayPhi = 0.35, rayDist = 3.8
  , rayMorph = 0.0, rayMorphTarget = 0.0
  }

-- Messages

data Msg
  = NextScene | PrevScene | GoScene Int | Tick
  | TypeChar Char | Backspace | SubmitItem
  | ToggleSelect | CursorUp | CursorDown
  | AdjustUp | AdjustDown | ToggleBarMode | KickBall
  | RayRotL | RayRotR | RayRotU | RayRotD | RayNextShape

-- Constants

totalScenes :: Int
totalScenes = 8

sceneNames :: [String]
sceneNames =
  [ "Ray Marcher", "Physics Game", "Text Input & Lists", "Borders & Styles"
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

-- Helpers

toggleIn :: Int -> [Int] -> [Int]
toggleIn x xs = if x `elem` xs then filter (/= x) xs else x : xs

-- Update

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
        -- Ray marcher shape morph easing
        mDiff    = rayMorphTarget s1 - rayMorph s1
        newMorph = if abs mDiff < 0.01 then rayMorphTarget s1
                   else rayMorph s1 + mDiff * 0.06
    in (s1 { tick = tick s1 + 1, ballY = ny, ballVy = vy, ballTrail = trail
           , rayMorph = newMorph }, CmdNone)

  TypeChar c | not (addingItem s) -> (s { textValue = textValue s ++ [c] }, CmdNone)
  TypeChar _ -> (s, CmdNone)

  Backspace | not (addingItem s) ->
    (s { textValue = if null (textValue s) then "" else init (textValue s) }, CmdNone)
  Backspace -> (s, CmdNone)

  SubmitItem | not (addingItem s) && not (null (textValue s)) ->
    (s { addingItem = True, addTick = 0 }, CmdNone)
  SubmitItem -> (s, CmdNone)

  ToggleSelect
    | scene s == 4 -> (s { tableSelected = toggleIn (tableRow s) (tableSelected s) }, CmdNone)
    | scene s == 7 -> (s { selected = toggleIn (cursor s) (selected s) }, CmdNone)
    | otherwise    -> (s, CmdNone)

  CursorUp
    | scene s == 4 -> (s { tableRow = (tableRow s - 1 + length services) `mod` length services }, CmdNone)
    | scene s == 7 -> (s { cursor = (cursor s - 1 + 7) `mod` 7 }, CmdNone)
    | otherwise    -> (s, CmdNone)

  CursorDown
    | scene s == 4 -> (s { tableRow = (tableRow s + 1) `mod` length services }, CmdNone)
    | scene s == 7 -> (s { cursor = (cursor s + 1) `mod` 7 }, CmdNone)
    | otherwise    -> (s, CmdNone)

  AdjustUp
    | scene s == 1 -> (s { gravity = min (gravity s + 1) 15 }, CmdNone)
    | scene s == 0 -> (s { rayDist = max (rayDist s - 0.25) 2.0 }, CmdNone)
    | otherwise    -> (s { lineOffset = min (lineOffset s + 1) 10 }, CmdNone)

  AdjustDown
    | scene s == 1 -> (s { gravity = max (gravity s - 1) 1 }, CmdNone)
    | scene s == 0 -> (s { rayDist = min (rayDist s + 0.25) 8.0 }, CmdNone)
    | otherwise    -> (s { lineOffset = max (lineOffset s - 1) (-10) }, CmdNone)

  ToggleBarMode -> (s { barMode = (barMode s + 1) `mod` 2 }, CmdNone)

  KickBall -> (s { ballVy = 5.0 }, CmdNone)

  RayRotL -> (s { rayTheta = rayTheta s - 0.15 }, CmdNone)
  RayRotR -> (s { rayTheta = rayTheta s + 0.15 }, CmdNone)
  RayRotU -> (s { rayPhi = min (rayPhi s + 0.1) 1.3 }, CmdNone)
  RayRotD -> (s { rayPhi = max (rayPhi s - 0.1) (-1.3) }, CmdNone)
  RayNextShape ->
    let next = (round (rayMorphTarget s) + 1) `mod` 2 :: Int
    in (s { rayMorphTarget = fromIntegral next }, CmdNone)

bounce :: Double -> Double -> (Double, Double)
bounce y vy
  | y <= 0                    = (0,  abs vy * 0.82)
  | y > 12                    = (12, -(abs vy * 0.5))
  | abs vy < 0.05 && y < 0.1 = (0, 0)
  | otherwise                 = (y, vy)

-- Subscriptions

subscriptions :: ShowcaseState -> Sub Msg
subscriptions s = subBatch
  [ subEveryMs 80 Tick
  , subKeyPress $ \key -> case key of
      KeyRight     -> Just NextScene
      KeyLeft      -> Just PrevScene
      KeyChar 'a'  | scene s == 0                       -> Just RayRotL
      KeyChar 'd'  | scene s == 0                       -> Just RayRotR
      KeyChar 'w'  | scene s == 0                       -> Just RayRotU
      KeyChar 's'  | scene s == 0                       -> Just RayRotD
      KeyChar '+'  -> Just AdjustUp
      KeyChar '-'  -> Just AdjustDown
      KeyChar ' '  | scene s == 0                       -> Just RayNextShape
      KeyChar ' '  | scene s == 1                       -> Just KickBall
      KeyChar ' '  | scene s == 4 || scene s == 7       -> Just ToggleSelect
      KeyTab       | scene s == 6                        -> Just ToggleBarMode
      KeyEnter     | scene s == 2                        -> Just SubmitItem
      KeyUp        -> Just CursorUp
      KeyDown      -> Just CursorDown
      KeyBackspace -> Just Backspace
      KeyChar c    | scene s == 2 && (isAlphaNum c || c == ' ') -> Just (TypeChar c)
      KeyChar c    | c >= '1' && c <= '8' -> Just (GoScene (fromEnum c - fromEnum '1'))
      _            -> Nothing
  ]

-- View

view :: ShowcaseState -> L
view s =
  let header  = renderHeader s
      content = case scene s of
        0 -> sceneRayMarcher s
        1 -> scenePhysicsGame s
        2 -> sceneTextInput s
        3 -> sceneBordersStyles s
        4 -> sceneTables s
        5 -> sceneChartsPlots s
        6 -> sceneBarChartsSparklines s
        7 -> sceneSelectionsHeatmap s
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
           [ withColor ColorBrightBlack $ text (T.pack prefix)
           , withStyle StyleBold $ withColor ColorBrightCyan $ text (T.pack title)
           , withColor ColorBrightBlack $ text (T.pack (" " ++ dashes ++ " "))
           , withColor ColorBrightBlack $ text (T.pack suffix)
           ]
       , br
       , withStyle StyleBold $ withColor ColorBrightYellow $ text (T.pack (" " ++ sceneNames !! scene s))
       , text (T.pack (" " ++ sceneDots))
       ]

renderFooter :: ShowcaseState -> L
renderFooter s =
  let hints = case scene s of
        0 -> "  </> scenes  wasd orbit  +/- zoom  Space shape  ESC quit"
        1 -> "  </> scenes  Space kick  +/- gravity  ESC quit"
        2 -> "  </> scenes  type + Enter to add  ESC quit"
        4 -> "  </> scenes  ^/v navigate  Space select  ESC quit"
        5 -> "  </> scenes  +/- move threshold  ESC quit"
        6 -> "  </> scenes  Tab cycle chart mode  ESC quit"
        7 -> "  </> scenes  ^/v navigate  Space toggle  ESC quit"
        _ -> "  </> scenes  ESC quit"
  in withStyle StyleDim $ withColor ColorBrightBlack $ text hints

-- Scene 2: Physics Game

scenePhysicsGame :: ShowcaseState -> L
scenePhysicsGame s =
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
            [ kv [ ("gravity", T.pack gLabel), ("velocity", T.pack velLabel), ("height", T.pack yLabel) ]
            , br
            , withColor ColorBrightGreen $ text (T.pack energyBar)
            , br
            , withColor ColorBrightCyan $ spinner "Simulating" (tick s `div` 3) SpinnerDots
            , withStyle StyleBold $ withColor ColorBrightYellow $ text "Press Space to kick ball!"
            ]
        ]
    ]

-- Scene 1: Ray Marcher

sceneRayMarcher :: ShowcaseState -> L
sceneRayMarcher s =
  let fb    = renderRayFrame (rayTheta s) (rayPhi s) (rayDist s) (rayMorph s)
      twoPi = 2 * pi :: Double
      thetaWrapped = rayTheta s - twoPi * fromIntegral (floor (rayTheta s / twoPi) :: Int)
      cameraStats = withColor ColorBrightBlue $ kv
        [ ("th",   T.pack (printf "%.2f" thetaWrapped :: String))
        , ("ph",   T.pack (printf "%.2f" (rayPhi s) :: String))
        , ("zoom", T.pack (printf "%.1f" (rayDist s) :: String))
        ]
      controls = withStyle StyleDim $ layout
        [ withColor ColorBrightYellow $ text "wasd   orbit"
        , withColor ColorBrightYellow $ text "+/-    zoom"
        , withColor ColorBrightYellow $ text "Space  shape"
        ]
      camera = alignLeft 20 $ render $ layout
        [ cameraStats
        , br
        , withColor ColorBrightCyan  $ spinner "render" (tick s `div` 2) SpinnerDots
        , withColor ColorBrightYellow $ spinner "light"  (tick s `div` 2) SpinnerBounce
        , br
        , controls
        ]
      cameraBox = withColor ColorBrightMagenta $ withBorder BorderRound $ box "Camera" [camera]
  in row [text (T.pack fb), cameraBox]

-- Scene 3: Text Input & Lists

sceneTextInput :: ShowcaseState -> L
sceneTextInput s =
  let inputLine =
        if addingItem s then
          row [ withColor ColorBrightYellow $ spinner "Adding" (addTick s) SpinnerDots
              , withColor ColorBrightYellow $ text (T.pack ("  \"" ++ textValue s ++ "\""))
              ]
        else
          let display = if null (textValue s)
                        then withColor ColorBrightBlack $ text "Type something..."
                        else withColor ColorBrightWhite $ text (T.pack (textValue s))
          in tightRow [ withColor ColorBrightCyan $ text "> ", display, withStyle StyleBlink $ text "_" ]

      itemColors = [ColorBrightGreen, ColorBrightBlue, ColorBrightMagenta, ColorBrightYellow, ColorBrightCyan]
      itemList =
        if null (stItems s) then
          withColor ColorBrightBlack $ text "  (no items yet)"
        else
          layout $ zipWith (\i item ->
            tightRow
              [ withColor ColorBrightBlack $ text (T.pack ("  " ++ show (i + 1) ++ ". "))
              , withColor (itemColors !! (i `mod` length itemColors)) $ text (T.pack item)
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
            [ tightRow [ text "Total items: ", withStyle StyleBold $ withColor ColorBrightCyan $ text (T.pack (show cnt)) ]
            , tightRow [ text "Longest:     ", withColor ColorBrightMagenta $ text (T.pack longest) ]
            , tightRow [ text "Shortest:    ", withColor ColorBrightMagenta $ text (T.pack shortest) ]
            ]
        , br
        , alignLeft boxW $ render $
            if cnt >= 3
            then withStyle StyleBold $ withColor ColorBrightGreen $ text "Nice collection!"
            else withColor ColorBrightBlack $ text (T.pack ("Add " ++ show (3 - cnt) ++ " more..."))
        ]
    ]

-- Scene 4: Borders & Styles

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

-- Scene 5: Tables

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
        in map (applyStyle . text . T.pack) cells
        ) [0 :: Int ..] services

      selCount = length (tableSelected s)
      selInfo  = if selCount > 0
                 then withColor ColorBrightGreen $ text (T.pack (show selCount ++ " selected"))
                 else withColor ColorBrightBlack $ text "none selected"
  in layout
    [ withBorder BorderRound $ table ["Service", "Status", "Latency", "Uptime"] coloredRows
    , tightRow
        [ withColor ColorBrightBlack $ text (T.pack (" Row " ++ show (tableRow s + 1) ++ "/" ++ show (length services) ++ "  |  "))
        , selInfo
        ]
    ]

-- Scene 6: Charts & Plots

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
        [ withColor ColorBrightYellow $ text (T.pack ("sin(x) & y = " ++ lineLabel ++ "  [+/- to shift]"))
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

-- Scene 7: Bar Charts & Sparklines

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
        [ withColor ColorBrightYellow $ text (T.pack (modeName ++ "  [Tab to cycle]"))
        , chartElem
        ]
    ]

-- Scene 8: Selections & Heatmap

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
        in applyStyle $ text (T.pack label)
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
            text (T.pack (show selCount ++ " of " ++ show (length days) ++ " active"))
        ]
    , withBorder BorderRound $ box "Weekly Activity"
        [ plotHeatmap' 5 (HeatmapData heatData (map T.pack days) hours) ]
    ]

-- Ray marcher helpers (Scene 1)

-- | 3D vector
data V3 = V3 !Double !Double !Double

vUp :: V3
vUp = V3 0 1 0

vadd, vsub, vcross :: V3 -> V3 -> V3
vadd (V3 ax ay az) (V3 bx by bz) = V3 (ax + bx) (ay + by) (az + bz)
vsub (V3 ax ay az) (V3 bx by bz) = V3 (ax - bx) (ay - by) (az - bz)
vcross (V3 ax ay az) (V3 bx by bz) =
  V3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

vscale :: V3 -> Double -> V3
vscale (V3 x y z) k = V3 (x * k) (y * k) (z * k)

vdot :: V3 -> V3 -> Double
vdot (V3 ax ay az) (V3 bx by bz) = ax * bx + ay * by + az * bz

vlen :: V3 -> Double
vlen v = sqrt (vdot v v)

vneg :: V3 -> V3
vneg (V3 x y z) = V3 (-x) (-y) (-z)

vnorm :: V3 -> V3
vnorm v = let l = vlen v in if l < 1e-10 then V3 0 0 0 else vscale v (1.0 / l)

-- | One screen cell with character + 24-bit color
data Pixel = Pixel !Char !Int !Int !Int

-- | Render a list of pixels into an ANSI string of width @w@
renderFrameBuffer :: [Pixel] -> Int -> String
renderFrameBuffer pixels w = go pixels
  where
    go [] = ""
    go ps =
      let (rowPixels, rest) = splitAt w ps
          line = concatMap pixelToAnsi rowPixels ++ "\ESC[0m"
      in if null rest then line else line ++ "\n" ++ go rest
    pixelToAnsi (Pixel ch r g b) =
      "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m" ++ [ch]

rayW, rayH, rayMaxSteps :: Int
rayW = 46
rayH = 22
rayMaxSteps = 50

rayMaxDist, rayEps :: Double
rayMaxDist = 20.0
rayEps     = 0.005

rayRamp :: String
rayRamp = " .'`^\",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"

clampD :: Double -> Double -> Double -> Double
clampD lo hi x = max lo (min hi x)

mixD :: Double -> Double -> Double -> Double
mixD a b t = a * (1.0 - t) + b * t

rSmoothstep :: Double -> Double -> Double -> Double
rSmoothstep lo hi x =
  let t = clampD 0.0 1.0 ((x - lo) / (hi - lo))
  in t * t * (3.0 - 2.0 * t)

sdTorus :: V3 -> Double -> Double -> Double
sdTorus (V3 x y z) bigR r =
  let qx = sqrt (x * x + z * z) - bigR
  in sqrt (qx * qx + y * y) - r

sdRoundBox :: V3 -> V3 -> Double -> Double
sdRoundBox (V3 px py pz) (V3 bx by bz) r =
  let qx = abs px - bx
      qy = abs py - by
      qz = abs pz - bz
      outer = vlen (V3 (max qx 0) (max qy 0) (max qz 0))
      inner = min (max qx (max qy qz)) 0.0
  in outer + inner - r

rayScene :: V3 -> Double -> Double
rayScene p morph =
  let tor = sdTorus p 0.9 0.38
      cub = sdRoundBox p (V3 0.72 0.72 0.72) 0.12
  in mixD tor cub (rSmoothstep 0 1 (clampD 0.0 1.0 morph))

calcNormal :: V3 -> Double -> V3
calcNormal (V3 x y z) morph =
  let e = 0.001
  in vnorm $ V3
       (rayScene (V3 (x + e) y z) morph - rayScene (V3 (x - e) y z) morph)
       (rayScene (V3 x (y + e) z) morph - rayScene (V3 x (y - e) z) morph)
       (rayScene (V3 x y (z + e)) morph - rayScene (V3 x y (z - e)) morph)

march :: V3 -> V3 -> Double -> Double
march ro rd morph = go 0.0 (0 :: Int)
  where
    go t i
      | i >= rayMaxSteps || t >= rayMaxDist = -1.0
      | otherwise =
          let d = rayScene (vadd ro (vscale rd t)) morph
          in if d < rayEps then t else go (t + d) (i + 1)

rayLightDir, rayFillDir :: V3
rayLightDir = vnorm (V3 0.8 1.0 (-0.6))
rayFillDir  = vnorm (V3 (-0.6) 0.4 0.7)

rayBgPixel :: V3 -> Pixel
rayBgPixel (V3 _ y _) =
  let vy = y * 0.5 + 0.5
      bg = max 4 (floor (18.0 - (1.0 - vy) * 8.0) :: Int)
  in Pixel ' ' bg bg (bg + 6)

rayShade :: V3 -> V3 -> Double -> Pixel
rayShade ro rd morph =
  let t = march ro rd morph
  in if t < 0
       then rayBgPixel rd
       else
         let hit  = vadd ro (vscale rd t)
             n    = calcNormal hit morph
             diff = max (vdot n rayLightDir) 0.0
             fill = max (vdot n rayFillDir) 0.0 * 0.35
             refl = vsub (vscale n (2.0 * vdot n rayLightDir)) rayLightDir
             spec = (max (vdot refl (vneg rd)) 0.0) ** 32.0 * 0.6
             ao   = 1.0 - clampD 0.0 0.4 (rayScene (vadd hit (vscale n 0.1)) morph * 5.0)
             -- Floor keeps hit surfaces above the ramp's whitespace glyphs
             lum  = clampD 0.0 1.0 (max ((0.12 + diff * 0.7 + fill + spec) * ao) 0.18)
             rampLen = length rayRamp
             idx = floor (clampD 0 (fromIntegral rampLen - 1) (lum * fromIntegral (rampLen - 1))) :: Int
             ch  = rayRamp !! idx
             V3 nx ny nz = n
             nx' = nx * 0.5 + 0.5
             ny' = ny * 0.5 + 0.5
             nz' = nz * 0.5 + 0.5
             rC  = floor (clampD 0 255 ((nx' * 0.55 + lum * 0.45) * 235 + 20)) :: Int
             gC  = floor (clampD 0 255 ((ny' * 0.45 + lum * 0.55) * 215 + 15)) :: Int
             bC  = floor (clampD 0 255 ((nz' * 0.50 + lum * 0.50 + 0.05) * 200 + 30)) :: Int
         in Pixel ch rC gC bC

renderRayFrame :: Double -> Double -> Double -> Double -> String
renderRayFrame theta phi dist morph =
  let ro    = V3 (dist * sin theta * cos phi)
                 (dist * sin phi)
                 (dist * cos theta * cos phi)
      fwd   = vnorm (vneg ro)
      right = vnorm (vcross fwd vUp)
      up    = vcross right fwd
      aspect = fromIntegral rayW / fromIntegral rayH * 0.48
      pixels = [ rayShade ro rd morph
               | py <- [0 .. rayH - 1]
               , let v = 0.5 - fromIntegral py / fromIntegral rayH
               , px <- [0 .. rayW - 1]
               , let u = (fromIntegral px / fromIntegral rayW - 0.5) * aspect
               , let rd = vnorm (vadd (vadd fwd (vscale right u)) (vscale up v))
               ]
  in renderFrameBuffer pixels rayW

-- App

showcaseApp :: LayoutzApp ShowcaseState Msg
showcaseApp = LayoutzApp
  { appInit          = (initialState, CmdNone)
  , appUpdate        = \msg s -> update msg s
  , appSubscriptions = subscriptions
  , appView          = view
  }

main :: IO ()
main = runAppWith defaultAppOptions { optAlignment = AppAlignCenter } showcaseApp
