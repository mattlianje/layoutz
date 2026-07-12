<div align="right">
  <sub><em>part of <a href="https://github.com/mattlianje/d4"><img src="https://raw.githubusercontent.com/mattlianje/d4/master/pix/d4.png" width="23"></a> <a href="https://github.com/mattlianje/d4">d4</a></em></sub>
</div>

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-demo.png" width="750">
</p>

# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="60"> layoutz

**Simple, beautiful CLI output 🪶**

A lightweight, zero-dep lib to build compositional ANSI strings, terminal plots,
and interactive Elm-style TUI's in pure Haskell.

Also in [Scala](https://github.com/mattlianje/layoutz), [OCaml](https://github.com/mattlianje/layoutz/tree/master/layoutz-ocaml)

## Features
- Pure Haskell, zero-dependencies (use `Layoutz.hs` like a header file)
- Elm-style TUIs
- Layout primitives, tables, trees, lists, CJK-aware
- Colors, ANSI styles, rich formatting
- Terminal charts and plots
- Widgets: text input, spinners, progress bars
- Inline raster images via the [kitty graphics protocol](https://sw.kovidgoyal.net/kitty/graphics-protocol/)
- One-shot interactive prompts (`askInput`, `askChoose`, `askFilter`, …)
- Drop-in progress `loader`s for build scripts and batch jobs
- Implement `Element` to add your own primitives
- Easy porting to MicroHs

<p align="center">
<img src="../demos/showcase.gif"" width="650">
<br>
<sub><a href="examples/ShowcaseApp.hs">ShowcaseApp.hs</a></sub>
</p>

Layoutz also lets you easily drop animations into build scripts or any processes that use Stdout:

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/inline-demo.gif" width="650">
<br>
<sub><a href="examples/InlineBar.hs">InlineBar.hs</a></sub>
</p>

## Table of Contents
- [Installation](#installation)
- [Quickstart](#quickstart)
- [Why layoutz?](#why-layoutz)
- [Core Concepts](#core-concepts)
- [Elements](#elements)
- [Border Styles](#border-styles)
- [Charts & Plots](#charts--plots)
- [Colors & Styles](#colors-ansi-support)
- [Inline Images](#inline-images)
- [Custom Elements](#custom-elements)
- [Collections](#collections)
- [Interactive Apps](#interactive-apps)
- [Prompts (Ask)](#prompts-ask)
- [Progress (loader)](#progress-loader)
- [Examples](#examples)
- [Contributing](#contributing)

## Installation

**Add Layoutz on [Hackage](https://hackage.haskell.org/package/layoutz) to your project's `.cabal` file:**
```haskell
build-depends: layoutz
```

All you need:
```haskell
import Layoutz
```

## Quickstart

**(1/3) Static rendering**: pretty, composable strings.

```haskell
import Layoutz

demo = layout
  [ center $ row
      [ withStyle StyleBold $ text "Layoutz"
      , withColor ColorCyan $ underline' "ˆ" $ text "DEMO"
      ]
  , br
  , row
    [ statusCard "Users" "1.2K"
    , withBorder BorderDouble $ statusCard "API" "UP"
    , withColor ColorRed $ withBorder BorderThick $ statusCard "CPU" "23%"
    , withStyle StyleReverse $ withBorder BorderRound $ table ["Name", "Role", "Skills"]
        [ ["Gegard", "Pugilist", ul ["Armenian", ul ["bad", ul["man"]]]]
        , ["Eve", "QA", "Testing"]
        ]
    ]
  ]

putStrLn $ render demo
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/intro-demo.png" width="700">
</p>


**(2/3) Interactive apps** - Build Elm-style TUI's:

```haskell
import Layoutz

data Msg = Inc | Dec

counterApp :: LayoutzApp Int Msg
counterApp = LayoutzApp
  { appInit = (0, CmdNone)
  , appUpdate = \msg count -> case msg of
      Inc -> (count + 1, CmdNone)
      Dec -> (count - 1, CmdNone)
  , appSubscriptions = \_ -> subKeyPress $ \key -> case key of
      KeyChar '+' -> Just Inc
      KeyChar '-' -> Just Dec
      _           -> Nothing
  , appView = \count -> layout
      [ section "Counter" [text $ "Count: " <> show count]
      , ul ["Press '+' or '-'", "ESC to quit"]
      ]
  }

main = runApp counterApp
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/counter-demo.gif" width="650">
</p>

**(3/3) Prompts (Ask)**

One-shot CLI prompts (inputs, choosers, filters, file pickers, spinners) that collapse to a single line as you answer them.

```haskell
import Layoutz
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  name   <- askInput "Name › " "anonymous" ""
  realm  <- askChoose "Choose a realm" ["The Shire", "Rivendell", "Mirkwood"] id
  packs  <- askChooseMany "Pack provisions" ["lembas", "pipe-weed", "rope"] 3 id
  member <- askFilter "Search a companion › " ["Bilbo", "Gandalf", "Thorin"] 8 id
  riddle <- askWrite "Pose a riddle" "This thing all things devours…" "" "Ctrl-D to save"
  quest  <- askConfirm "Venture on the quest?" True "Yes" "No"
  smaug  <- askSpin "Awaking Smaug…" SpinnerDots (threadDelay 1500000 >> pure "ready")
  pure ()
```
<p align="center">
  <img src="../demos/ask-mini.gif" width="600">
  <br>
  <sub><a href="examples/AskDemo.hs">AskDemo.hs</a></sub>
</p>

## Why layoutz?
- We have `printf` and [full-blown](https://hackage.haskell.org/package/brick) TUI libraries - but there's a gap in-between
- **layoutz** is a tiny, declarative DSL for structured CLI output
- On the side, it has a little Elm-style runtime + keyhandling DSL to animate your elements, much like a flipbook...
     - But you can just use **Layoutz** without any of the TUI stuff

## Core concepts
- Every piece of content is an `Element`
- Elements are **immutable** and **composable** - build complex layouts by combining simple elements
- A `layout` arranges elements **vertically**:
```haskell
layout [elem1, elem2, elem3]  -- Joins with "\n"
```
Call `render` on any element to get a string

The power comes from **uniform composition** - since everything has the `Element` typeclass, everything can be combined.

### String Literals
With `OverloadedStrings` enabled, you can use string literals directly:
```haskell
layout ["Hello", "World"]  -- Instead of layout [text "Hello", text "World"]
```

> [!NOTE]
> When passing to functions that take polymorphic `Element a` parameters (like `underline'`, `center'`, `pad`), use `text` explicitly:

```haskell
underline' "=" $ text "Title"  -- Correct
underline' "=" "Title"         -- Ambiguous type error
```

## Border Styles

Applied via `withBorder` to any element with the `HasBorder` typeclass (`box`, `statusCard`, `table`):
```haskell
withBorder BorderRound $ box "Info" ["content"]
withBorder BorderDouble $ statusCard "API" "UP"
withBorder BorderThick $ table ["Name"] [["Alice"]]
```

Write generic code over bordered elements:
```haskell
makeThick :: HasBorder a => a -> a
makeThick = setBorder BorderThick
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
layout ["First", "Second", "Third"]          -- vertical
-- First
-- Second
-- Third

row ["Left", "Middle", "Right"]              -- horizontal
-- Left Middle Right

columns [layout ["A", "B"], layout ["C", "D"]]
-- A  C
-- B  D

tightRow [text "A", text "B", text "C"]      -- no spacing
-- ABC
```

#### Spacing & rules
```haskell
layout [text "Line 1", br, text "Line 2"]    -- br is a blank line

hr                 -- default ──────────...
hr' "~"            -- custom char
hr'' "=" 20        -- ====================

vr                 -- vertical rule, 10 high with │
vr' "║"            -- custom char
vr'' "|" 5         -- custom char + height
```

#### Text transforms
```haskell
center $ text "Auto-centered"                -- width taken from siblings
center' 20 $ text "TITLE"                    -- │       TITLE        │
alignLeft 20 "Left"                          -- │Left                │
alignRight 20 "Right"                        -- │               Right│

justify 30 "Spread this out"
-- │Spread         this        out│

wrap 20 "Long text here that should wrap"
-- Long text here that
-- should wrap

truncate' 15 (text "Very long text that will be cut off")     -- Very long te...
truncate'' 20 "…" (text "Custom ellipsis example text here")  -- Custom ellipsis exa…

pad 2 $ text "content"                       -- 2 cells of padding all around

underline $ text "Title"                     -- Title
                                             -- ─────
underline' "=" $ text "Custom"               -- Custom
                                             -- ══════
underlineColored "~" ColorCyan $ text "Fancy"

margin "[error]"
  [ text "Ooops!"
  , row [text "val result: Int = ", underline' "^" (text "getString()")]
  , text "Expected Int, found String"
  ]
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

section "Status" [text "All systems operational"]
-- === Status ===
-- All systems operational
section' "-" "Status" [text "ok"]       -- custom glyph
section'' "#" "Report" 5 [text "42"]    -- custom glyph + width

kv [("Name", "Alice"), ("Age", "30"), ("City", "NYC")]
-- Name: Alice
-- Age:  30
-- City: NYC
```

#### Boxes, cards & banners
```haskell
box "Status" [text "All systems go"]
-- ┌──Status──────────┐
-- │ All systems go   │
-- └──────────────────┘

withBorder BorderDouble $ box "Fancy" [text "Double border"]
-- ╔══Fancy═══════════╗
-- ║ Double border    ║
-- ╚══════════════════╝

withBorder BorderRound $ box "Smooth" [text "Rounded corners"]
-- ╭──Smooth────────────╮
-- │ Rounded corners    │
-- ╰────────────────────╯

row [ withColor ColorGreen $ statusCard "CPU" "45%"
    , withColor ColorCyan  $ statusCard "MEM" "2.1G"
    ]
-- ┌──────┐ ┌───────┐
-- │ CPU  │ │ MEM   │
-- │ 45%  │ │ 2.1G  │
-- └──────┘ └───────┘

banner ["System Dashboard"]
-- ╔══════════════════╗
-- ║ System Dashboard ║
-- ╚══════════════════╝
```

Pipe any `HasBorder` element through `withBorder`:
```haskell
withBorder BorderRound $ box "Info" ["content"]
withBorder BorderDouble $ statusCard "API" "UP"
withBorder BorderThick $ table ["Name"] [["Alice"]]
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
  [ withBorder BorderRound $ box "the gracie hunter" [img]
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

Needs a kitty-graphics-capable terminal (kitty, WezTerm, Ghostty). Inside a
`LayoutzApp`, each distinct image is transmitted once and re-placed cheaply on
every frame.

## Charts & Plots

See also [Granite](https://github.com/mchav/granite) for terminal plots in Haskell.

#### Line Plot
```haskell
let sinePoints = [(x, sin x) | x <- [0, 0.1 .. 10.0]]
plotLine 40 10 [Series sinePoints "sine" ColorBrightCyan]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-function-1.png" width="500">
</p>

Multiple series:
```haskell
let sinPts = [(x, sin (x * 0.15) * 5) | x <- [0..50]]
    cosPts = [(x, cos (x * 0.15) * 5) | x <- [0..50]]
plotLine 50 12
  [ Series sinPts "sin(x)" ColorBrightCyan
  , Series cosPts "cos(x)" ColorBrightMagenta
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-function-2.png" width="500">
</p>

#### Pie Chart
```haskell
plotPie 20 10
  [ Slice 50 "A" ColorBrightCyan
  , Slice 30 "B" ColorBrightMagenta
  , Slice 20 "C" ColorBrightYellow
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-pie.png" width="500">
</p>

#### Bar Chart
```haskell
plotBar 40 10
  [ BarItem 85 "Mon" ColorBrightCyan
  , BarItem 120 "Tue" ColorBrightGreen
  , BarItem 95 "Wed" ColorBrightMagenta
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-bar.png" width="500">
</p>

```haskell
plotBar 40 10
  [ BarItem 100 "Sales" ColorBrightMagenta
  , BarItem 80  "Costs" ColorBrightRed
  , BarItem 20  "Profit" ColorBrightCyan
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-bar-custom.png" width="500">
</p>

#### Stacked Bar Chart
```haskell
plotStackedBar 40 10
  [ StackedBarGroup [BarItem 30 "Q1" ColorDefault, BarItem 20 "Q2" ColorDefault, BarItem 25 "Q3" ColorDefault] "2022"
  , StackedBarGroup [BarItem 35 "Q1" ColorDefault, BarItem 25 "Q2" ColorDefault, BarItem 30 "Q3" ColorDefault] "2023"
  , StackedBarGroup [BarItem 40 "Q1" ColorDefault, BarItem 30 "Q2" ColorDefault, BarItem 35 "Q3" ColorDefault] "2024"
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-stacked.png" width="500">
</p>

#### Sparkline
```haskell
plotSparkline [1, 4, 2, 8, 5, 7, 3, 6]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-sparkline.png" width="500">
</p>

#### Heatmap
```haskell
plotHeatmap $ HeatmapData
  [ [12, 15, 22, 28, 30, 25, 18]
  , [14, 18, 25, 32, 35, 28, 20]
  , [10, 13, 20, 26, 28, 22, 15]
  ]
  ["Mon", "Tue", "Wed"]
  ["6am", "9am", "12pm", "3pm", "6pm", "9pm", "12am"]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-heatmap.png" width="500">
</p>

## Colors

Add ANSI colors to any element:

```haskell
layout
  [ withColor ColorRed $ text "The quick brown fox..."
  , withColor ColorBrightCyan $ text "The quick brown fox..."
  , underlineColored "~" ColorRed $ text "The quick brown fox..."
  , margin "[INFO]" [withColor ColorCyan $ text "The quick brown fox..."]
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
let palette   = tightRow $ map (\i -> withColor (ColorFull i) $ text "█") [16, 19..205]
    redToBlue = tightRow $ map (\i -> withColor (ColorTrue i 100 (255 - i)) $ text "█") [0, 4..255]
    greenFade = tightRow $ map (\i -> withColor (ColorTrue 0 (255 - i) i) $ text "█") [0, 4..255]
    rainbow   = tightRow $ map colorBlock [0, 4..255]
      where
        colorBlock i =
          let r = if i < 128 then i * 2 else 255
              g = if i < 128 then 255 else (255 - i) * 2
              b = if i > 128 then (i - 128) * 2 else 0
          in withColor (ColorTrue r g b) $ text "█"

putStrLn $ render $ layout [palette, redToBlue, greenFade, rainbow]
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-color-1.png" width="700">
</p>


## Styles

Add ANSI styles to any element:

```haskell
layout
  [ withStyle StyleBold $ text "The quick brown fox..."
  , withColor ColorRed $ withStyle StyleBold $ text "The quick brown fox..."
  , withStyle StyleReverse $ withStyle StyleItalic $ text "The quick brown fox..."
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-styles-1.png" width="700">
</p>

```haskell
StyleBold
StyleDim
StyleItalic
StyleUnderline
StyleBlink
StyleReverse
StyleHidden
StyleStrikethrough
StyleDefault                  -- Conditional no-op
StyleBold <> StyleItalic      -- Combine with <>
```

**Combining Styles:**

Use `<>` to combine multiple styles at once:

```haskell
layout
  [ withStyle (StyleBold <> StyleItalic <> StyleUnderline) $ text "The quick brown fox..."
  , withStyle (StyleBold <> StyleReverse) $ text "The quick brown fox..."
  ]
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-styles-2.png" width="700">
</p>

You can also combine colors and styles:

```haskell
withColor ColorBrightYellow $ withStyle (StyleBold <> StyleItalic) $ text "The quick brown fox..."
```

## Custom Elements

Create your own components by implementing the `Element` typeclass

```haskell
data Square = Square Int

instance Element Square where
  renderElement (Square size)
    | size < 2 = ""
    | otherwise = intercalate "\n" (top : middle ++ [bottom])
    where
      w = size * 2 - 2
      top = "┌" ++ replicate w '─' ++ "┐"
      middle = replicate (size - 2) ("│" ++ replicate w ' ' ++ "│")
      bottom = "└" ++ replicate w '─' ++ "┘"

-- Helper to avoid wrapping with L
square :: Int -> L
square n = L (Square n)

-- Use it like any other element
putStrLn $ render $ row
  [ square 3
  , square 5
  , square 7
  ]
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
  [ layout
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

## REPL

Drop into GHCi to experiment:
```bash
cabal repl
```

```haskell
λ> :set -XOverloadedStrings
λ> import Layoutz
λ> putStrLn $ render $ center $ box "Hello" ["World!"]
┌──Hello──┐
│ World!  │
└─────────┘
λ> putStrLn $ render $ table ["A", "B"] [["1", "2"]]
┌───┬───┐
│ A │ B │
├───┼───┤
│ 1 │ 2 │
└───┴───┘
```

## Interactive Apps

`LayoutzApp` uses the [Elm Architecture](https://guide.elm-lang.org/architecture/) where your
view is a `layoutz` `Element`.

```haskell
data LayoutzApp state msg = LayoutzApp
  { appInit          :: (state, Cmd msg)                 -- Initial state + startup command
  , appUpdate        :: msg -> state -> (state, Cmd msg) -- Pure state transitions
  , appSubscriptions :: state -> Sub msg                 -- Event sources
  , appView          :: state -> L                       -- Render to UI
  }
```

Three daemon threads coordinate rendering (~30fps), tick/timers, and input capture. State updates flow through `appUpdate` synchronously.

Press **ESC** to exit.

### App Options

Customise how your app runs with `runAppWith` and the `AppOptions` record. Override only the fields you need:

```haskell
runApp app                                                          -- Default options
runAppWith defaultAppOptions { optAlignment = AppAlignCenter } app  -- Centered in terminal
runInline app                                                       -- Animate in-place, no alt screen
```

`runInline` renders the app below existing terminal output without clearing the screen. Useful for embedding progress bars or spinners in build scripts... use `CmdExit` to quit programmatically.

Terminal width is detected once at startup via ANSI cursor position report (zero dependencies).

### Subscriptions

```haskell
subKeyPress (\key -> ...)    -- Keyboard input
subEveryMs 100 msg           -- Periodic ticks (interval in ms)
subBatch [sub1, sub2, ...]   -- Combine subscriptions
```

### Commands

```haskell
CmdNone                                -- No effect
CmdExit                                -- Quit the app gracefully
cmdFire (writeFile "log.txt" "entry")  -- Fire and forget IO
cmdTask (readFile "data.txt")          -- IO that returns a message
cmdAfterMs 500 msg                     -- Fire a message after delay (ms)
CmdBatch [cmd1, cmd2, ...]             -- Combine multiple commands
```

**Example: Logger with file I/O**
```haskell
import Layoutz

data Msg = Log | Saved
data State = State { count :: Int, status :: String }

loggerApp :: LayoutzApp State Msg
loggerApp = LayoutzApp
  { appInit = (State 0 "Ready", CmdNone)
  , appUpdate = \msg s -> case msg of
      Log   -> (s { count = count s + 1 },
                cmdFire $ appendFile "log.txt" ("Entry " <> show (count s) <> "\n"))
      Saved -> (s { status = "Saved!" }, CmdNone)
  , appSubscriptions = \_ -> subKeyPress $ \key -> case key of
      KeyChar 'l' -> Just Log
      _           -> Nothing
  , appView = \s -> layout
      [ section "Logger" [text $ "Entries: " <> show (count s)]
      , text (status s)
      , ul ["'l' to log", "ESC to quit"]
      ]
  }

main = runApp loggerApp
```

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
KeySpecial String   -- Other unrecognized sequences
```

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
  name     <- askInput "Name › " "anonymous" ""              -- prompt placeholder initial
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
  <img src="../demos/ask-mini.gif" width="650">
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
styleBlocks -- (default)
styleBar
styleAscii
styleDots
styleLine
stylePipes
LoaderStyle { .. } -- (custom)

```haskell
_ <- loaderStyled styleAscii "Reindexing" docIds reindex
_ <- loaderStyled stylePipes "Crawling" urls fetch
```

Every built-in style, then an unbounded stream:

```haskell
import Layoutz
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  _ <- loaderStyled styleBlocks "Blocks " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled styleDots   "Dots   " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled styleLine   "Line   " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled stylePipes  "Pipes  " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled styleBar    "Bar    " [1..60] (const (threadDelay 16000))
  _ <- loaderStyled styleAscii  "Ascii  " [1..60] (const (threadDelay 16000))
  _ <- loaderStream "Streaming" [1..90] (const (threadDelay 45000))
  pure ()
```

<p align="center">
  <img src="../demos/loader.gif" width="600">
</p>

## Examples
- [ShowcaseApp.hs](examples/ShowcaseApp.hs) - Tours every layoutz element and visualization across 8 scenes
- [AskDemo.hs](examples/AskDemo.hs) - Tours the one-shot `ask*` prompts
- [SimpleGame.hs](SimpleGame.hs) - Grid game where you collect gems and dodge enemies with WASD
- [InlineBar.hs](examples/InlineBar.hs) - Renders a gradient progress bar in-place
- [InlineLoadingDemo.hs](examples/InlineLoadingDemo.hs) - Chained inline progress bars in a simulated build script

## Contributing

You need [GHC](https://www.haskell.org/ghcup/) (8.10+) and [Cabal](https://www.haskell.org/cabal/).

```bash
make build         # build library
make test          # run tests
make repl          # GHCi with layoutz loaded
make clean         # clean build artifacts
```

Fork, make your change, `make test`, open a PR. Keep it zero-dep.

## Inspiration
- Original Scala [layoutz](https://github.com/mattlianje/layoutz)
