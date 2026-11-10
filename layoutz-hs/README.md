<div align="right">
  <sub><em>part of <a href="https://github.com/mattlianje/cabinet6"><img src="https://raw.githubusercontent.com/mattlianje/cabinet6/master/pix/d4.png" width="20" align="top" hspace="4">c6</a></em></sub>
</div>

# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="60"> layoutz

**Simple, beautiful CLI output**

A lightweight lib (only GHC boot packages) to build compositional ANSI strings, terminal plots,
and interactive Elm-style TUI's in pure Haskell.

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/demos/showcase.gif" width="600">
  <br>
  <sub><a href="examples/ShowcaseApp.hs">ShowcaseApp.hs</a></sub>
</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-demo-repl.png" width="600">
</p>

## Features
- Pure Haskell, no dependencies beyond GHC's boot packages (use `Layoutz.hs` like a header file)
- Elm-style TUIs, with clickable and scrollable elements
- Layout primitives, tables, trees, lists, CJK-aware
- Colors, ANSI styles, rich formatting
- Terminal charts and plots
- Widgets: text input, spinners, progress bars
- Inline raster images via the [kitty graphics protocol](https://sw.kovidgoyal.net/kitty/graphics-protocol/)
- One-shot interactive prompts (`askInput`, `askChoose`, `askFilter`, …)
- Drop-in progress `loader`s for build scripts and batch jobs
- Custom elements are just functions returning a `View`
- Easy porting to MicroHs

## Installation
[![Hackage](https://img.shields.io/hackage/v/layoutz)](https://hackage.haskell.org/package/layoutz)
```haskell
build-depends: layoutz
```
Or try it in a REPL:
```bash
cabal repl --build-depends layoutz
```
All you need is one import:
```haskell
import Layoutz
```

## Of note...
- With LLM's, boilerplate code that formats & "pretty-prints" is cheaper than ever...
- Thus, more than ever, "string formatting code" is spawning, and polluting domain logic.
- We have `printf` and [full-blown](https://hackage.haskell.org/package/brick) TUI libraries - but there's a gap in-between.
- Ultimately, layoutz is just a tiny, declarative DSL to combat this.
- On the side, layoutz also has an Elm-style runtime to bring these arbitrary Elements to life: much like a flipbook.
     - The runtime has some little niceties built-in like common cmd's for IO, timers, and a key input handler.
- But at the end of the day, you can use layoutz merely to structure Strings (without any of the TUI stuff).

## TUIs
Just fill in a [`LayoutzApp`](#interactive-apps), then call `runApp`:
```haskell
data LayoutzApp model msg = LayoutzApp
  { appInit          :: (model, Cmd msg)
  , appUpdate        :: msg -> model -> (model, Cmd msg)
  , appSubscriptions :: model -> Sub msg
  , appView          :: model -> View msg
  }
```
You have built-in `Cmd`/`Sub` for IO tasks, timers, delayed messages and key input, and
any element can be made [clickable](#mouse)...

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/demos/showcase-tour.gif" width="600">
  <br>
  <sub><a href="examples/ShowcaseApp.hs">ShowcaseApp.hs</a></sub>
</p>

## Plots
```haskell
let sinePoints = [(x, sin x * 5) | x <- [0, 0.15 .. 7.5]]
plotLine 50 12 [Series sinePoints "sin(x)" ColorBrightCyan]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-charts.png" width="600">
</p>

## Prompts
You often want one-shot CLI prompts with pickers, file pickers, fuzzy-finders, pagers, etc... without dropping
down into Elm-territory to make them. This is why layoutz has the `ask*` helpers.
```haskell
name   <- askInput "Name › " "anonymous" ""
realm  <- askChoose "Choose a realm" ["The Shire", "Rivendell", "Mirkwood"] id
member <- askFilter "Search a companion › " ["Bilbo", "Gandalf", "Thorin"] 8 id
riddle <- askWrite "Pose a riddle" "This thing all things devours…" "" "Ctrl-D to save"
quest  <- askConfirm "Venture on the quest?" True "Yes" "No"
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/demos/ask-mini.gif" width="600">
  <br>
  <sub><a href="examples/AskDemo.hs">AskDemo.hs</a></sub>
</p>

## Progress
Wrap any list with `loader` for a live bar, or use `loaderStream` for a spinner. Works in any script that writes to stdout.
```haskell
_ <- loader "Resizing" files resize
_ <- loaderStream "Tailing" logLines indexLine
n <- askSpin "Awaking Smaug…" SpinnerDots (threadDelay 1500000 >> pure 42)
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/demos/loader.gif" width="600">
</p>


## Reference

- [Core Concepts](#core-concepts)
- [Border Styles](#border-styles)
- [Elements](#elements)
  - [Layout](#layout)
  - [Content](#content)
- [Custom Elements](#custom-elements)
- [Collections](#collections)
- [Colors](#colors)
- [Styles](#styles)
- [Charts & Plots](#charts--plots)
- [Interactive Apps](#interactive-apps)
  - [Keys](#key-types)
  - [Subscriptions](#subscriptions)
  - [Commands](#commands)
  - [Mouse](#mouse)
  - [Focus](#focus)
  - [Widgets](#widgets)
- [Prompts (Ask)](#prompts-ask)
- [Progress (loader)](#progress-loader)
- [Examples](#examples)
- [Upgrading from 0.x](#upgrading-from-0x)



## Core Concepts
- Every piece of content is a `View m`
- Views are **immutable** and **composable** - build complex layouts by combining simple ones
- Constructors take **content**; everything else is an **attribute** you apply with `&`:
```haskell
box "Status" ["All systems go"] & border BorderRound & fg ColorGreen
```
- Attributes are plain `View m -> View m` functions, so prefix style works too: `fg ColorGreen $ box ...`
- A `vstack` arranges views **vertically**, a `row` **horizontally**:
```haskell
vstack [elem1, elem2, elem3]  -- Joins with "\n"
```
Call `render` on any view to get a string.

The `m` is the message type sent by [clickable](#mouse) parts. A view without handlers fits any `m`,
so write helpers as `Model -> View m` and they drop into any app.

### String Literals
With `OverloadedStrings` enabled, you can use string literals anywhere a view is expected:
```haskell
vstack ["Hello", "World"]  -- Instead of vstack [text "Hello", text "World"]
underline "Title"
```

## Border Styles

Applied with `border` to `box`, `banner`, `statusCard` and `table` (anything else is left alone):
```haskell
box "Info" ["content"] & border BorderRound
statusCard "API" "UP" & border BorderDouble
table ["Name"] [["Alice"]] & border BorderThick
```

It's a plain function, so partially apply it to reuse:
```haskell
thick :: View m -> View m
thick = border BorderThick
```

```haskell
BorderNormal                  -- ┌─┐ (default)
BorderDouble                  -- ╔═╗
BorderThick                   -- ┏━┓
BorderRound                   -- ╭─╮
BorderAscii                   -- +-+
BorderBlock                   -- ███
BorderDashed                  -- ┌╌┐
BorderDotted                  -- ┌┈┐
BorderInnerHalfBlock          -- ▗▄▖
BorderOuterHalfBlock          -- ▛▀▜
BorderMarkdown                -- |-|
BorderCustom "+" "=" "|"      -- Custom border
BorderNone                    -- No borders
```

## Elements

### Layout

#### Stacking & rows
```haskell
vstack ["First", "Second", "Third"]          -- vertical
-- First
-- Second
-- Third

row ["Left", "Middle", "Right"]              -- horizontal, one space apart
-- Left Middle Right

row [vstack ["A", "B"], vstack ["C", "D"]] & gap 2
-- A  C
-- B  D

row ["A", "B", "C"] & gap 0                  -- no spacing
-- ABC
```

#### Spacing & rules
```haskell
vstack ["Line 1", br, "Line 2"]              -- br is a blank line

hr                          -- default ──────────...
hr & glyph "~"              -- custom char
hr & glyph "=" & size 20    -- ====================

vr                          -- vertical rule, 10 high with │
vr & glyph "║"              -- custom char
vr & glyph "|" & size 5     -- custom char + height
```

#### Sizing
Views draw at their natural size. 

`fill` makes one take the room its parent offers: the leftover
width of a `row`, the leftover height of a `vstack`, or the whole terminal at an app's root.
Boxes and rules stretch to it.
```haskell
renderIn 40 6 $
  row [box "nav" ["home", "logs"] & fixedWidth 12, box "main" ["hello"] & fill] & fill
-- ┌───nav────┐ ┌──────────main───────────┐
-- │ home     │ │ hello                   │
-- │ logs     │ │                         │
-- │          │ │                         │
-- │          │ │                         │
-- └──────────┘ └─────────────────────────┘

fixedWidth 20 view     -- exactly 20 wide: padded or cut
fixedHeight 5 view
renderIn 80 24 view    -- render with 80x24 of room, like an app's root

-- built from the room it gets, so it redraws as the terminal is resized
sized (\w h -> plotLine (w - 8) (h - 2) series) & fill
```

#### Text transforms
```haskell
center "Auto-centered"                       -- width taken from siblings
centerIn 20 "TITLE"                          -- │       TITLE        │
alignLeft 20 "Left"                          -- │Left                │
alignRight 20 "Right"                        -- │               Right│

justify 30 "Spread this out"
-- │Spread         this        out│

wrap 20 "Long text here that should wrap"
-- Long text here that
-- should wrap

clip 15 "Very long text that will be cut off"               -- Very long te...
clip 20 "Custom ellipsis example text here" & ellipsis "…"  -- Custom ellipsis exa…

pad 2 "content"                              -- 2 cells of padding all around

underline "Title"                            -- Title
                                             -- ─────
underline "Custom" & glyph "="               -- Custom
                                             -- ======
underline "Fancy" & glyph "~" & accent ColorCyan

vstack
  [ "Ooops!"
  , row ["val result: Int = ", underline "getString()" & glyph "^"]
  , "Expected Int, found String"
  ] & margin "[error]"
-- [error] Ooops!
-- [error] val result: Int =  getString()
-- [error]                    ^^^^^^^^^^^
-- [error] Expected Int, found String
```

### Content

#### Basics
```haskell
text "hello"
"hello"   - with OverloadedStrings

section "Status" ["All systems operational"]
-- === Status ===
-- All systems operational
section "Status" ["ok"] & glyph "-"            -- custom glyph
section "Report" ["42"] & glyph "#" & size 5   -- custom glyph + width

kv [("Name", "Alice"), ("Age", "30"), ("City", "NYC")]
-- Name: Alice
-- Age:  30
-- City: NYC
```

#### Boxes, cards & banners
```haskell
box "Status" ["All systems go"]
-- ┌─────Status─────┐
-- │ All systems go │
-- └────────────────┘

box "Fancy" ["Double border"] & border BorderDouble
-- ╔═════Fancy═════╗
-- ║ Double border ║
-- ╚═══════════════╝

box "Smooth" ["Rounded corners"] & border BorderRound
-- ╭─────Smooth──────╮
-- │ Rounded corners │
-- ╰─────────────────╯

row [ statusCard "CPU" "45%"  & fg ColorGreen
    , statusCard "MEM" "2.1G" & fg ColorCyan
    ]
-- ┌───────┐ ┌────────┐
-- │ CPU   │ │ MEM    │
-- │ 45%   │ │ 2.1G   │
-- └───────┘ └────────┘

banner ["System Dashboard"]
-- ╔══════════════════╗
-- ║ System Dashboard ║
-- ╚══════════════════╝
```

#### Lists & trees
```haskell
ul ["Backend", ul ["API", ul ["REST", "GraphQL"], "DB"], "Frontend"]
-- • Backend
--   ◦ API
--     ▪ REST
--     ▪ GraphQL
--   ◦ DB
-- • Frontend

ol ["Setup", ol ["Install deps", ol ["npm", "pip"], "Configure"], "Deploy"]
-- 1. Setup
--   a. Install deps
--     i. npm
--     ii. pip
--   b. Configure
-- 2. Deploy

tree "Project"
  [ branch "src" [leaf "main.hs", leaf "test.hs"]
  , branch "docs" [leaf "README.md"]
  ]
-- Project
-- ├── src
-- │   ├── main.hs
-- │   └── test.hs
-- └── docs
--     └── README.md
```

#### Table
```haskell
table ["Name", "Age", "City"]
  [ ["Alice", "30", "New York"]
  , ["Bob", "25", ""]
  , ["Charlie", "35", "London"]
  ]
```
```
┌─────────┬─────┬──────────┐
│ Name    │ Age │ City     │
├─────────┼─────┼──────────┤
│ Alice   │ 30  │ New York │
│ Bob     │ 25  │          │
│ Charlie │ 35  │ London   │
└─────────┴─────┴──────────┘
```

#### Progress & spinners
```haskell
inlineBar "Download" 0.75
-- Download [███████████████─────] 75%

chart [("Web", 10), ("Mobile", 20), ("API", 15)]
-- Web    │████████████████████────────────────────│ 10
-- Mobile │████████████████████████████████████████│ 20
-- API    │██████████████████████████████──────────│ 15

spinner "Loading" frame SpinnerDots     -- ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏
spinner "Loading" frame SpinnerLine     -- | / - \
spinner "Loading" frame SpinnerClock    -- 🕐 🕑 🕒 🕓 🕔 🕕 🕖 🕗 🕘 🕙 🕚 🕛
spinner "Loading" frame SpinnerBounce   -- ⠁ ⠂ ⠄ ⠂
spinner "Loading" frame SpinnerEarth    -- 🌍 🌎 🌏
spinner "Loading" frame SpinnerMoon     -- 🌑 🌒 🌓 🌔 🌕 🌖 🌗 🌘
spinner "Loading" frame SpinnerGrow     -- ▏ ▎ ▍ ▌ ▋ ▊ ▉ █ ▉ ▊ ▋ ▌ ▍ ▎
spinner "Loading" frame SpinnerArrow    -- ← ↖ ↑ ↗ → ↘ ↓ ↙
```

#### Inline Images

Inline raster images via the [kitty graphics protocol](https://sw.kovidgoyal.net/kitty/graphics-protocol/).
A `KittyImage` measures exactly `cols`×`rows` cells, so it composes with every other
element: drop it into boxes, rows, tables, `center`, etc.

```haskell
img <- kittyImageFile "pix/sakuraba.png" 35 14   -- PNG sized to 35×14 cells

putStrLn $ render $ row
  [ box "the gracie hunter" [img] & border BorderRound
  , box "stats" [kv [("flying", "yes"), ("opponent", "grounded"), ("rules", "PRIDE")]]
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/haskell-kitty-1.png" width="600">
</p>

Build straight from bytes or raw pixels:

```haskell
kittyImage  bytes 35 14       -- PNG bytes            -> cols rows
kittyRGB    pixels w h 18 9   -- raw RGB  pxW pxH     -> cols rows
kittyRGBA   pixels w h 18 9   -- raw RGBA pxW pxH     -> cols rows
```

```haskell
let (w, h) = (96, 96)
    pixels = [ channel i | i <- [0 .. w * h * 4 - 1] ]
    channel i =
      let p = i `div` 4; x = p `mod` w; y = p `div` w
      in fromIntegral $ case i `mod` 4 of
           0 -> x * 255 `div` w
           1 -> y * 255 `div` h
           2 -> 255 - (x * 255 `div` w)
           _ -> 255

putStrLn $ render $ box "gradient" [kittyRGBA pixels w h 18 9]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/haskell-kitty-2.png" width="600">
</p>

Needs a kitty-graphics-capable terminal (kitty, WezTerm, Ghostty). Inside an
`LayoutzApp`, each distinct image is transmitted once and re-placed cheaply on
every frame.

## Custom Elements

A custom element is just a function that returns a `View`. Build it from other views, or
from lines of text you draw yourself:

```haskell
square :: Int -> View m
square size
  | size < 2  = ""
  | otherwise = vstack (map text (top : middle ++ [bottom]))
  where
    w      = size * 2 - 2
    top    = "┌" ++ replicate w '─' ++ "┐"
    middle = replicate (size - 2) ("│" ++ replicate w ' ' ++ "│")
    bottom = "└" ++ replicate w '─' ++ "┘"

-- Use it like any other element
putStrLn $ render $ row [square 3, square 5, square 7]
```
```
┌────┐ ┌────────┐ ┌────────────┐
│    │ │        │ │            │
└────┘ │        │ │            │
       │        │ │            │
       └────────┘ │            │
                  │            │
                  └────────────┘
```

Attributes work on it like on anything else: `square 4 & fg ColorCyan`.

## Collections
```haskell
import Data.List (groupBy, sortOn)
import Data.Function (on)

data User = User { userName :: String, userRole :: String }

users :: [User]
users =
  [ User "Alice" "Admin"
  , User "Bob"   "User"
  , User "Tom"   "User"
  ]

putStrLn $ render $ section "Users by Role"
  [ vstack
      [ box (userRole (head grp)) [ul [text (userName u) | u <- grp]]
      | grp <- groupBy ((==) `on` userRole) (sortOn userRole users)
      ]
  ]
```
```
=== Users by Role ===
┌──Admin──┐
│ • Alice │
└─────────┘
┌──User──┐
│ • Bob  │
│ • Tom  │
└────────┘
```

## Colors

Add ANSI colors to any element:

```haskell
vstack
  [ "The quick brown fox..." & fg ColorRed
  , "The quick brown fox..." & fg ColorBrightCyan
  , underline "The quick brown fox..." & glyph "~" & accent ColorRed
  , "The quick brown fox..." & fg ColorCyan & margin "[INFO]"
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-color-2.png" width="700">
</p>


```haskell
ColorBlack
ColorRed
ColorGreen
ColorYellow
ColorBlue
ColorMagenta
ColorCyan
ColorWhite
ColorBrightBlack      -- Bright 8
ColorBrightRed
ColorBrightGreen
ColorBrightYellow
ColorBrightBlue
ColorBrightMagenta
ColorBrightCyan
ColorBrightWhite
ColorFull 196         -- 256-color palette (0-255)
ColorTrue 255 128 0   -- 24-bit RGB
ColorDefault          -- Conditional no-op
```

### Color Gradients

Create gradients with extended colors:

```haskell
let strip f xs = row [fg (f i) "█" | i <- xs] & gap 0
    palette    = strip ColorFull [16, 19 .. 205]
    redToBlue  = strip (\i -> ColorTrue i 100 (255 - i)) [0, 4 .. 255]
    greenFade  = strip (\i -> ColorTrue 0 (255 - i) i) [0, 4 .. 255]
    rainbow    = strip colorAt [0, 4 .. 255]
    colorAt i =
      let r = if i < 128 then i * 2 else 255
          g = if i < 128 then 255 else (255 - i) * 2
          b = if i > 128 then (i - 128) * 2 else 0
      in ColorTrue r g b

putStrLn $ render $ vstack [palette, redToBlue, greenFade, rainbow]
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-color-1.png" width="700">
</p>

## Styles

Add ANSI styles to any element:

```haskell
vstack
  [ "The quick brown fox..." & styleBold
  , "The quick brown fox..." & styleBold & fg ColorRed
  , "The quick brown fox..." & styleItalic & styleReverse
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-styles-1.png" width="700">
</p>

```haskell
styleBold
styleDim
styleItalic
styleUnderline  -- the ANSI style; 'underline' draws a rule below instead
styleBlink
styleReverse
styleHidden
styleStrikethrough
```

**Combining Styles:**

Chain them. Styles applied one after another merge into a single escape code:

```haskell
vstack
  [ "The quick brown fox..." & styleBold & styleItalic & styleUnderline   -- ESC[1;3;4m
  , "The quick brown fox..." & styleBold & styleReverse
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-styles-2.png" width="700">
</p>

You can also combine colors and styles:

```haskell
"The quick brown fox..." & styleBold & styleItalic & fg ColorBrightYellow
```

## Charts & Plots

```
plotLine        Series points label color
plotPie         Slice value label color
plotBar         BarItem value label color
plotStackedBar  StackedBarGroup [BarItem] label
plotHeatmap     HeatmapData rows rowLabels colLabels
chart           [(label, value)]
plotSparkline   [Double]
```

<details>
<summary>Code</summary>

```haskell
sinPts = [(x, sin (x * 0.15) * 5) | x <- [0 .. 60]]
cosPts = [(x, cos (x * 0.15) * 5) | x <- [0 .. 60]]

charts = vstack
  [ row
      [ panel "plotLine"
          [ plotLine 52 10
              [ Series sinPts "sin(x)" ColorBrightCyan
              , Series cosPts "cos(x)" ColorBrightMagenta
              ]
          ]
      , panel "plotPie"
          [ plotPie 32 8
              [ Slice 50 "Liquor" ColorDefault, Slice 20 "Protein" ColorDefault
              , Slice 10 "Water" ColorDefault, Slice 20 "Fun" ColorDefault
              ]
          ]
      ]
  , row
      [ panel "plotBar"
          [ plotBar 42 9
              [ BarItem 100 "Mon" ColorBrightBlue, BarItem 120 "Tue" ColorBrightMagenta
              , BarItem 110 "Wed" ColorBrightCyan, BarItem 85 "Thu" ColorBrightYellow
              ]
          ]
      , panel "plotStackedBar"
          [ plotStackedBar 42 7
              [ StackedBarGroup [BarItem 30 "Q1" ColorDefault, BarItem 20 "Q2" ColorDefault, BarItem 25 "Q3" ColorDefault] "2023"
              , StackedBarGroup [BarItem 35 "Q1" ColorDefault, BarItem 25 "Q2" ColorDefault, BarItem 30 "Q3" ColorDefault] "2024"
              , StackedBarGroup [BarItem 40 "Q1" ColorDefault, BarItem 30 "Q2" ColorDefault, BarItem 35 "Q3" ColorDefault] "2025"
              ]
          ]
      ]
  , row
      [ panel "plotHeatmap"
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
          [ panel "chart" [chart [("Web", 10), ("Mobile", 20), ("API", 15)]]
          , panel "plotSparkline"
              [fg ColorBrightGreen $ plotSparkline [1, 4, 2, 8, 5, 7, 3, 6, 9, 4, 6, 8, 5, 9]]
          ]
      ]
  ]

panel title body = box title body & border BorderRound

putStrLn $ render charts
```

</details>

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-charts-all.png" width="700">
  <br>
  <sub><a href="../demos/shots/AllCharts.hs">AllCharts.hs</a></sub>
</p>

See also [Granite](https://github.com/mchav/granite) for terminal plots in Haskell - which was the
inspiration here.

## Interactive Apps

`LayoutzApp` uses the [Elm Architecture](https://guide.elm-lang.org/architecture/) where your
view is a layoutz `View`.

```haskell
data LayoutzApp model msg = LayoutzApp
  { appInit          :: (model, Cmd msg)                 -- Initial model + startup command
  , appUpdate        :: msg -> model -> (model, Cmd msg) -- Pure state transitions
  , appSubscriptions :: model -> Sub msg                 -- Event sources
  , appView          :: model -> View msg                -- Render to UI
  }
```

Apps that never need a `Cmd` or a `Sub` can skip the boilerplate:

```haskell
sandbox :: model -> (msg -> model -> model) -> (model -> View msg) -> LayoutzApp model msg
```

Background threads handle rendering (capped at `optMaxFps`, 60 by default), timers, input and
commands. `appUpdate` runs on a single thread, so updates never race.

Press **ESC** to exit (see `optQuitKeys` below). Ctrl-C always quits.

### App Options

```haskell
runApp app                                                -- Full screen, default options
run defaultOptions { optAlignment = AppAlignCenter } app  -- Centered; returns the final model
runInline app                                             -- Animate in-place, no alt screen
```

```haskell
data Options = Options
  { optAlignment :: AppAlignment  -- AppAlignLeft (default), AppAlignCenter, AppAlignRight
  , optInline    :: Bool          -- draw under the current output instead of full screen
  , optQuitKeys  :: [Key]         -- default [KeyEscape, KeyCtrl 'D']
  , optMaxFps    :: Int           -- frame cap, default 60; changes in between share a frame
  }
```

`runInline` renders the app below existing terminal output without clearing the screen. Useful for embedding progress bars or spinners in build scripts... use `cmdQuit` to stop programmatically.

Terminal size comes from an ANSI cursor position report, re-queried on resize so `fill` layouts
follow along (on Windows it's read once at startup).

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/inline-demo.gif" width="650">
<br>
<sub><a href="examples/InlineBar.hs">InlineBar.hs</a></sub>
</p>

### Key Types

```haskell
-- Printable
KeyChar Char        -- 'a', '1', ' '

-- Editing
KeyEnter
KeyBackspace
KeyTab
KeyEscape
KeyDelete

-- Navigation
KeyUp
KeyDown
KeyLeft
KeyRight
KeyPageUp
KeyPageDown
KeyHome
KeyEnd

-- Modifiers
KeyCtrl Char        -- Ctrl+'C', Ctrl+'Q', etc.
KeySpecial String   -- Everything else: F-keys, Ctrl-arrows, Alt-keys...
```

The decoder behind these is pure, if you want to test against raw bytes or read input yourself:

```haskell
parseInput :: String -> Maybe (Input, String)  -- first input + leftover, Nothing if incomplete
parseInput "\ESC[Ax"  -- Just (InputKey KeyUp, "x")
parseInput "\ESC[<0;10;5M"  -- Just (InputMouse (MouseEvent 0 (4, 9) True), "")
```

A lone `"\ESC"` is `Nothing` too: it only counts as Esc once nothing follows within ~50ms.

### Subscriptions

```haskell
subKeyPress (\key -> ...)    -- Keyboard input
subEveryMs 100 msg           -- Periodic ticks (interval in ms)
subNone                      -- Nothing
sub1 <> sub2                 -- Combine (or mconcat [...]); every key handler sees every key
```

### Commands

```haskell
cmdNone                                -- No effect
cmdQuit                                -- Quit, after the other effects in the same command ran
cmdFire (writeFile "log.txt" "entry")  -- Fire and forget IO
cmdTask (readFile "data.txt")          -- IO whose result comes back as a message
cmdAfterMs 500 msg                     -- Send a message after a delay (ms)
cmd1 <> cmd2                           -- Combine (or mconcat [...]); every message is delivered
```

`Cmd` and `Sub` are `Functor`s, so a child component's commands embed with `fmap ChildMsg`.

**Example: Logger with file I/O**
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Layoutz

data Msg = Log | Saved
data State = State { count :: Int, status :: String }

loggerApp :: LayoutzApp State Msg
loggerApp = LayoutzApp
  { appInit = (State 0 "Ready", cmdNone)
  , appUpdate = \msg s -> case msg of
      Log   -> ( s { count = count s + 1 }
               , cmdTask $ Saved <$ appendFile "log.txt" ("Entry " <> show (count s) <> "\n") )
      Saved -> (s { status = "Saved!" }, cmdNone)
  , appSubscriptions = \_ -> subKeyPress $ \key -> case key of
      KeyChar 'l' -> Just Log
      _           -> Nothing
  , appView = \s -> vstack
      [ section "Logger" [text $ "Entries: " <> show (count s)]
      , text (status s)
      , ul ["'l' to log", "ESC to quit"]
      ]
  }

main :: IO ()
main = runApp loggerApp
```

### Mouse

Any element can take clicks and wheel events:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Layoutz

data Msg = Inc | Dec

counter :: LayoutzApp Int Msg
counter = sandbox 0 step $ \n ->
  row [button "-" Dec, display n, button "+" Inc]
    & onScroll (\d -> if d > 0 then Dec else Inc)
  where
    step Inc = (+ 1)
    step Dec = subtract 1

main :: IO ()
main = runApp counter
```

```haskell
onClick msg               -- left click anywhere on the view
onScroll (\notches -> ..) -- wheel: +1 per notch down, -1 up
button "label" msg        -- a clickable label in reverse video
```

- Clicks go to the innermost element that handles them, then bubble outward.
- Mouse reporting is only enabled while something on screen handles it, so text selection works
  otherwise. Shift-drag selects in most terminals regardless.
- Full-screen apps only; `runInline` doesn't know which screen row it starts on.
- Components embed with `fmap`: `Workers <$> stepper "workers" (workers m)`. See [ClickDemo.hs](examples/ClickDemo.hs).

Test without a terminal: `clickAt` and `scrollAt` say what a click at `(row, col)` sends.

```haskell
clickAt (0, 1) (appView counter 0)  -- Just Dec
```

### Focus

Mark the views that can hold the keyboard with `focusable`. Tab and Shift-Tab move between them in
screen order, a click focuses one, and the first starts out focused.

```haskell
box "services" [list] & focusable "services" & focusStyle (fg ColorCyan . border BorderDouble)
```

- `onKey (\key -> ...)` handlers inside the focused view get keys first, then handlers outside any
  focusable, then `subKeyPress`. A key that gets handled stops there.
- `focusStyle` applies only while the view has focus; your model doesn't track it.
- `cmdFocus "logs"` moves focus from `appUpdate`; `subFocus Focused` tells you when it changes.
- Esc still quits by default; change `optQuitKeys` if you need it.

### Widgets

Widget state lives in your model, and each message carries the widget's next state, so
`appUpdate` just stores it. Give them a height with `fill` or `fixedHeight`.

```haskell
data Msg = Moved (SelectList String) | Picked String | Scrolled Viewport

selectList Moved Picked text (services m) & fill           -- arrows, wheel, click; Enter picks
viewport Scrolled (logView m) (map text (logs m)) & fill   -- wheel, arrows, PgUp/PgDn, scrollbar

selectListOf ["api", "worker"] :: SelectList String        -- start a list
selectedItem (services m)      :: Maybe String
viewportTop, viewportBottom    :: Viewport                 -- at the bottom, it follows new lines
```

<sub>[FocusDemo.hs](examples/FocusDemo.hs) puts both together: a list of fighters and their fight logs, with Tab between them, under a chart that uses `sized` to fit the terminal.</sub>

Test without a terminal: `pressKey (Just "logs") KeyDown view` says what a key sends, and
`focusOrder view` lists the Tab order.

## Prompts (Ask)

You often just want a one-shot CLI prompt (a spinner, a filter, a file picker)
without dropping into "Elm-territory" and thinking about a whole flipbook each time.

**layoutz** offers one-shot interactive actions with the `ask*` family. Each takes
over the tty briefly, returns a value, and leaves a single committed line behind.
`Nothing` means the user cancelled (Esc / Ctrl-C / Ctrl-D).

```haskell
import Layoutz
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  name     <- askInput "Name › " "anonymous" ""
  ok       <- askConfirm "Venture on the quest?" True "Yes" "No"
  realm    <- askChoose "Choose a realm" ["The Shire", "Rivendell", "Mirkwood", "Lake-town", "Erebor"] id
  packs    <- askChooseMany "Pack provisions" ["lembas", "pipe-weed", "waybread", "miruvor", "rope"] 3 id
  riddle   <- askWrite "Pose a multi-line riddle" "This thing all things devours…" "" "Ctrl-D to save"
  member   <- askFilter "Search > " ["Bilbo", "Balin", "Dwalin", "Thorin", "Gandalf"] 8 id
  path     <- askFile "." 12
  askPager longString 0 True
  answer   <- askSpin "Awaking Smaug…" SpinnerDots (do threadDelay 1500000; pure (42 :: Int))
  pure ()
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/demos/ask-mini.gif" width="650">
</p>

Each `ask*` returns in `IO`... and a `Maybe` if the user can cancel midway:

```haskell
askInput      prompt placeholder initial       -- IO (Maybe String)
askConfirm    question default yes no          -- IO Bool
askChoose     prompt items render              -- IO (Maybe a)
askChooseMany prompt items limit render        -- IO (Maybe [a])
askWrite      prompt placeholder initial hint  -- IO (Maybe String)
askFilter     prompt items height render       -- IO (Maybe a)
askFile       start height                     -- IO (Maybe String)
askPager      content height lineNumbers       -- IO ()
askSpin       label style task                 -- IO a
```

## Progress (loader)

Wrap any list with `loader` to get a live progress bar while you process it: map
an `IO` action over each item. Unbounded work gets `loaderStream`, a spinner
with a running count.

```haskell
_ <- loader "Resizing" imageFiles resize
_ <- loader "Inserting" rows insertRow
_ <- loaderStream "Tailing" logLines indexLine
```

Pick a `LoaderStyle` with `loaderStyled` / `loaderStreamStyled`
```
loaderBlocks -- (default)
loaderBar
loaderAscii
loaderDots
loaderLine
loaderPipes
LoaderStyle { .. } -- (custom)
```

```haskell
_ <- loaderStyled loaderAscii "Reindexing" docIds reindex
_ <- loaderStyled loaderPipes "Crawling" urls fetch
```

Every built-in style, then an unbounded stream:

```haskell
import Layoutz
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  _ <- loaderStyled loaderBlocks "Blocks " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled loaderDots   "Dots   " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled loaderLine   "Line   " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled loaderPipes  "Pipes  " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled loaderBar    "Bar    " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled loaderAscii  "Ascii  " [1..60] (const (threadDelay 16000))
  _ <- loaderStream "Streaming" [1..90] (const (threadDelay 45000))
  pure ()
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/demos/loader.gif" width="600">
</p>

## Examples

Small, copy-pasteable snippets for everyday CLI chores.

<details>
<summary>Print a deploy / status summary (<code>section</code> + <code>statusCard</code> + <code>kv</code>)</summary>

```haskell
import Layoutz

main :: IO ()
main = putStrLn $ render $ section "Deploy"
  [ row
      [ statusCard "build"  "OK"      & fg ColorGreen
      , statusCard "tests"  "142 ✓"   & fg ColorGreen
      , statusCard "deploy" "staging" & fg ColorYellow
      ]
  , kv [("commit", "a1b2c3d"), ("branch", "master"), ("by", "matt")]
  ]
```
</details>

<details>
<summary>Compiler-style error message (<code>margin</code> + <code>underline</code>)</summary>

```haskell
import Layoutz

main :: IO ()
main = putStrLn $ render $ vstack
  [ "type mismatch"
  , row ["let x: Int = ", underline "getString()" & glyph "^"]
  , "  expected Int, found String"
  ] & margin "error:"
-- error: type mismatch
-- error: let x: Int =  getString()
-- error:               ^^^^^^^^^^^
-- error:   expected Int, found String
```
</details>

<details>
<summary>Render a table straight from records (<code>table</code>)</summary>

```haskell
import Layoutz

data Svc = Svc { svcName :: String, svcStatus :: String, svcCpu :: String }

svcs :: [Svc]
svcs =
  [ Svc "api"   "up"   "23%"
  , Svc "db"    "up"   "61%"
  , Svc "cache" "down" "0%"
  ]

main :: IO ()
main = putStrLn $ render $ table ["Service", "Status", "CPU"]
  [ [text (svcName s), text (svcStatus s), text (svcCpu s)] | s <- svcs ]
```
</details>

<details>
<summary>Progress bar over a batch of work (<code>loader</code>)</summary>

```haskell
import Layoutz
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let files = ["a.png", "b.png", "c.png"]
  _ <- loader "Resizing" files $ \f -> threadDelay 200000  -- your IO per item
  putStrLn "done"
```
</details>

<details>
<summary>Gate a destructive action behind a confirm (<code>askConfirm</code>)</summary>

```haskell
import Layoutz

main :: IO ()
main = do
  ok <- askConfirm "Drop production table?" False "Yes" "No"
  if ok then putStrLn "dropping…" else putStrLn "cancelled"
```
</details>

<details>
<summary>Wrap a slow query in a spinner (<code>askSpin</code>)</summary>

```haskell
import Layoutz
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  rows <- askSpin "Querying…" SpinnerDots runQuery
  putStrLn $ render $ section "Result" [text $ show rows <> " rows"]
  where
    runQuery = do
      threadDelay 1500000 -- stand-in for your slow IO
      pure (1234 :: Int)
```
</details>

## Contributing

You need [GHC](https://www.haskell.org/ghcup/) and [Cabal](https://www.haskell.org/cabal/). Layoutz needs `text >= 2.1.2`, so to use `Layoutz.hs` as a single file your GHC has to ship that (9.10.3 does).

```bash
make build     # build library
make test      # run tests
make repl      # GHCi with layoutz loaded
make bench     # layoutz vs Bubble Tea, Brick and ratatui, Linux only, results in bench/README.md
make clean     # clean build artifacts
```

Fork, make your change, `make test`, open a PR. Keep it to GHC's boot packages.

## Upgrading from 0.x

1.0 replaces the `Element` typeclass with a single `View` type and `&` attributes. Most code ports by renaming:

| 0.x | 1.0 |
|---|---|
| `L`, `Element a => a` | `View m` (helpers: `Model -> View m`) |
| `layout xs` | `vstack xs` |
| `tightRow xs` | `row xs & gap 0` |
| `columns xs` / `columns' n xs` | `row xs & gap 2` / `row xs & gap n` |
| `withColor c x` | `fg c x` or `x & fg c` |
| `withStyle StyleBold x` | `styleBold x` or `x & styleBold` |
| `withStyle (StyleBold <> StyleItalic) x` | `x & styleBold & styleItalic` |
| `withBorder b x` | `border b x` or `x & border b` |
| `center' w x` | `centerIn w x` |
| `underline' g x` | `underline x & glyph g` |
| `underlineColored g c x` | `underline x & glyph g & accent c` |
| `hr' g`, `hr'' g n` (same for `vr`) | `hr & glyph g`, `hr & glyph g & size n` |
| `section' g t xs`, `section'' g t n xs` | `section t xs & glyph g`, `... & size n` |
| `truncate' n x`, `truncate'' n e x` | `clip n x`, `clip n x & ellipsis e` |
| `margin p xs` | `vstack xs & margin p` |
| `alignLeft w "str"` and friends | same, but they take any view |
| `instance Element` | a function returning `View m` |
| `LayoutzApp` | same name, `appView` now returns `View msg` (or `sandbox` when there are no effects) |
| `runAppWith opts app`, `runAppFinal` | `run opts app` (returns the final model) |
| `AppOptions`, `defaultAppOptions` | `Options`, `defaultOptions` |
| `optClearOnStart = False, optClearOnExit = False` | `optInline = True` |
| `CmdNone`, `CmdExit` | `cmdNone`, `cmdQuit` |
| `CmdBatch cs`, `subBatch ss` | `mconcat cs`, `mconcat ss` (or `<>`) |
| `executeCmd :: Cmd msg -> IO (Maybe msg)` | `executeCmd :: Cmd msg -> IO [msg]` |

Behaviour changes:
- Unknown escape sequences (F-keys, Ctrl-arrows, Alt-keys) arrive as `KeySpecial` instead of quitting the app.
- Every `subKeyPress` handler sees every key, and every message in a combined `Cmd` is delivered.
- `cmdQuit` lets the other effects in the same command finish before the app exits.

## Inspiration
- Original Scala [layoutz](https://github.com/mattlianje/layoutz)
