<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-demo.png" width="750">
</p>

# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="60"> layoutz

**Simple, beautiful CLI output for Haskell 🪶**

Build declarative and composable sections, trees, tables, dashboards, and interactive Elm-style TUI's.

Part of [d4](https://github.com/mattlianje/d4)

## Features
- Zero dependencies, use `Layoutz.hs` like a header file
- Easy porting to MicroHs
- Rich text formatting: alignment, underlines, padding, margins
- Lists, trees, tables, charts, spinners...
- ANSI colors and wide character support
- Easily create new primitives (no component-library limitations)
- [`LayoutzApp`](#interactive-apps) for Elm-style TUI's

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/showcase-demo.gif" width="650">
<br>
<sub><a href="examples/ShowcaseApp.hs">ShowcaseApp.hs</a></sub>
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
- [Custom Components](#custom-components)
- [Interactive Apps](#interactive-apps)
- [Examples](#examples)

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

**(1/2) Static rendering** - Beautiful, compositional strings:

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


**(2/2) Interactive apps** - Build Elm-style TUI's:

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
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/counter-demo.gif" width="550">
</p>

`LayoutzApp`s can also run inline without clearing the screen, animating in place within existing output:

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/inline-demo.gif" width="550">
<br>
<sub><a href="examples/InlineBar.hs">InlineBar.hs</a></sub>
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

**Note:** When passing to functions that take polymorphic `Element a` parameters (like `underline'`, `center'`, `pad`), use `text` explicitly:
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

### Text: `text`
```haskell
text "hello"
"hello"                                      -- with OverloadedStrings
```

### Line Break: `br`
```haskell
layout [text "Line 1", br, text "Line 2"]
```

### Layout (vertical): `layout`
```haskell
layout ["First", "Second", "Third"]
```
```
First
Second
Third
```

### Row (horizontal): `row`, `tightRow`
```haskell
row ["Left", "Middle", "Right"]
tightRow [text "A", text "B", text "C"]      -- no spacing
```
```
Left Middle Right
ABC
```

### Horizontal Rule: `hr`, `hr'`, `hr''`
```haskell
hr                                           -- default ──────────
hr' "~"                                      -- custom char
hr'' "=" 20                                  -- custom char + width
```
```
──────────────────────────────────────────────────
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
====================
```

### Vertical Rule: `vr`, `vr'`, `vr''`
```haskell
vr                                           -- default: 10 high with │
vr' "║"                                      -- custom char
vr'' "|" 5                                   -- custom char + height
```

### Box: `box`
```haskell
box "Status" [text "All systems go"]
withBorder BorderDouble $ box "Fancy" [text "Double border"]
withBorder BorderRound $ box "Smooth" [text "Rounded corners"]
```
```
┌──Status──────────┐
│ All systems go   │
└──────────────────┘
╔══Fancy═══════════╗
║ Double border    ║
╚══════════════════╝
╭──Smooth────────────╮
│ Rounded corners    │
╰────────────────────╯
```

Pipe any element through `withBorder`:
```haskell
withBorder BorderRound $ box "Info" ["content"]
withBorder BorderDouble $ statusCard "API" "UP"
withBorder BorderThick $ table ["Name"] [["Alice"]]
```

### Status Card: `statusCard`
```haskell
row [ withColor ColorGreen $ statusCard "CPU" "45%"
    , withColor ColorCyan $ statusCard "MEM" "2.1G"
    ]
```
```
┌──────┐ ┌───────┐
│ CPU  │ │ MEM   │
│ 45%  │ │ 2.1G  │
└──────┘ └───────┘
```

### Table: `table`
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

### Key-Value: `kv`
```haskell
kv [("Name", "Alice"), ("Age", "30"), ("City", "NYC")]
```
```
Name: Alice
Age:  30
City: NYC
```

### Section: `section`, `section'`, `section''`
```haskell
section "Status" [text "All systems operational"]
section' "-" "Status" [text "ok"]            -- custom glyph
section'' "#" "Report" 5 [text "42"]         -- custom glyph + width
```
```
=== Status ===
All systems operational
```

### Unordered List: `ul`
```haskell
ul ["Backend", ul ["API", ul ["REST", "GraphQL"], "DB"], "Frontend"]
```
```
• Backend
  ◦ API
    ▪ REST
    ▪ GraphQL
  ◦ DB
• Frontend
```

### Ordered List: `ol`
```haskell
ol ["Setup", ol ["Install deps", ol ["npm", "pip"], "Configure"], "Deploy"]
```
```
1. Setup
  a. Install deps
    i. npm
    ii. pip
  b. Configure
2. Deploy
```

### Tree: `tree`, `branch`, `leaf`
```haskell
tree "Project"
  [ branch "src" [leaf "main.hs", leaf "test.hs"]
  , branch "docs" [leaf "README.md"]
  ]
```
```
Project
├── src
│   ├── main.hs
│   └── test.hs
└── docs
    └── README.md
```

### Progress Bar: `inlineBar`
```haskell
inlineBar "Download" 0.75
```
```
Download [███████████████─────] 75%
```

### Chart: `chart`
```haskell
chart [("Web", 10), ("Mobile", 20), ("API", 15)]
```
```
Web    │████████████████████────────────────────│ 10
Mobile │████████████████████████████████████████│ 20
API    │██████████████████████████████──────────│ 15
```

### Spinner: `spinner`
Styles: `SpinnerDots` (default), `SpinnerLine`, `SpinnerClock`, `SpinnerBounce`
```haskell
spinner "Loading" frame SpinnerDots            -- ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏
spinner "Working" frame SpinnerLine            -- | / - \
spinner "Waiting" frame SpinnerClock           -- 🕐 🕑 🕒 ...
spinner "Thinking" frame SpinnerBounce         -- ⠁ ⠂ ⠄ ⠂
```

### Alignment: `center`, `alignLeft`, `alignRight`, `justify`, `wrap`
```haskell
center $ text "Auto-centered"                  -- width from siblings
center' 30 $ text "Fixed width"
alignLeft 30 "Left"
alignRight 30 "Right"
justify 30 "Spaces are distributed evenly"
wrap 20 "Long text wrapped at word boundaries"
```

### Underline: `underline`, `underline'`, `underlineColored`
```haskell
underline $ text "Title"
underline' "=" $ text "Double"
underlineColored "~" ColorCyan $ text "Fancy"
```
```
Title
─────

Double
======
```

### Margin: `margin`
```haskell
margin "[error]" [text "Oops", text "fix it"]
```
```
[error] Oops
[error] fix it
```

### Padding: `pad`
```haskell
pad 2 $ text "Padded content"
```

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

## Colors (ANSI Support)

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
ColorBrightBlack              -- Bright 8
ColorBrightRed
ColorBrightGreen
ColorBrightYellow
ColorBrightBlue
ColorBrightMagenta
ColorBrightCyan
ColorBrightWhite
ColorFull 196                 -- 256-color palette (0-255)
ColorTrue 255 128 0           -- 24-bit RGB
ColorDefault                  -- Conditional no-op
```

### Color Gradients

Create beautiful gradients with extended colors:

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


## Styles (ANSI Support)

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

## Custom Components

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
view is simply a `layoutz` `Element`.

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
runAppWith defaultAppOptions { optAlignment = AppAlignRight } app   -- Right-aligned
```

Terminal width is detected once at startup via ANSI cursor position report (zero dependencies).

### Subscriptions

```haskell
subKeyPress (\key -> ...)              -- Keyboard input
subEveryMs 100 msg                     -- Periodic ticks (interval in ms)
subBatch [sub1, sub2, ...]             -- Combine subscriptions
```

### Commands

```haskell
CmdNone                                -- No effect
cmdFire (writeFile "log.txt" "entry")  -- Fire and forget IO
cmdTask (readFile "data.txt")          -- IO that returns a message
cmdAfterMs 500 msg                     -- Fire a message after delay (ms)
CmdBatch [cmd1, cmd2, ...]            -- Combine multiple commands
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
KeyChar Char                  -- 'a', '1', ' '

-- Editing
KeyEnter                      -- Enter/Return
KeyBackspace                  -- Backspace
KeyTab                        -- Tab
KeyEscape                     -- Escape
KeyDelete                     -- Delete

-- Navigation
KeyUp                         -- Arrow up
KeyDown                       -- Arrow down
KeyLeft                       -- Arrow left
KeyRight                      -- Arrow right

-- Modifiers
KeyCtrl Char                  -- Ctrl+'C', Ctrl+'Q', etc.
KeySpecial String             -- Other unrecognized sequences
```

## Examples
- [ShowcaseApp.hs](examples/ShowcaseApp.hs) - 7-scene interactive demo
- [SimpleGame.hs](SimpleGame.hs) - Collect gems, avoid enemies
- [InlineBar.hs](examples/InlineBar.hs) - Gradient loading bar

## Inspiration
- Original Scala [layoutz](https://github.com/mattlianje/layoutz)
