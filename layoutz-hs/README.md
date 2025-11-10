<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-hs/pix/layoutz-demo.png" width="700">
</p>

# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="60"> layoutz

**Simple, beautiful CLI output for Haskell 🪶**

Build declarative and composable sections, trees, tables, dashboards for your Haskell applications.

## Features
- Zero dependencies, use `Layoutz.hs` like a header file
- Rich text formatting: alignment, underlines, padding, margins
- Lists, trees, tables, charts, banners...

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

Beautiful, compositional text layouts:

```haskell
import Layoutz

demo = layout
  [ center $ row ["Layoutz", underline' "ˆ" $ text "DEMO"]
  , br
  , row
    [ statusCard "Users" "1.2K"
    , withBorder BorderDouble $ statusCard "API" "UP"
    , withBorder BorderThick $ statusCard "CPU" "23%"
    , withBorder BorderRound $ table ["Name", "Role", "Status"] 
        [ ["Alice", "Engineer", "Online"]
        , ["Eve", "QA", "Away"]
        ]
    , section "Pugilists" [kv [("Kazushi", "Sakuraba"), ("Jet", "Li")]]
    ]
  ]

putStrLn $ render demo
```

```
                                Layoutz DEMO
                                        ˆˆˆˆ

┌─────────┐ ╔═══════╗ ┏━━━━━━━┓ ╭───────┬──────────┬────────╮ === Pugilists ===
│ Users   │ ║ API   ║ ┃ CPU   ┃ │ Name  │ Role     │ Status │ Kazushi: Sakuraba
│ 1.2K    │ ║ UP    ║ ┃ 23%   ┃ ├───────┼──────────┼────────┤ Jet:     Li
└─────────┘ ╚═══════╝ ┗━━━━━━━┛ │ Alice │ Engineer │ Online │
                                │ Eve   │ QA       │ Away   │
                                ╰───────┴──────────┴────────╯
```

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

## Elements

### Text
```haskell
text "Simple text"
-- Or with OverloadedStrings:
"Simple text"
```
```
Simple text
```

### Line Break
Add line breaks with `br`:
```haskell
layout ["Line 1", br, "Line 2"]
```
```
Line 1

Line 2
```

### Section: `section`
```haskell
section "Config" [kv [("env", "prod")]]
section' "-" "Status" [kv [("health", "ok")]]
section'' "#" "Report" 5 [kv [("items", "42")]]
```
```
=== Config ===
env: prod

--- Status ---
health: ok

##### Report #####
items: 42
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

### Row (horizontal): `row`
Arrange elements side-by-side horizontally:
```haskell
row ["Left", "Middle", "Right"]
```
```
Left Middle Right
```

Multi-line elements are aligned at the top:
```haskell
row 
  [ layout ["Left", "Column"]
  , layout ["Middle", "Column"]
  , layout ["Right", "Column"]
  ]
```
```
Left   Middle Right 
Column Column Column
```

### Text alignment: `alignLeft`, `alignRight`, `alignCenter`, `justify`
Align text within a specified width:
```haskell
layout
  [ alignLeft 40 "Left aligned"
  , alignCenter 40 "Centered"
  , alignRight 40 "Right aligned"
  , justify 40 "This text is justified evenly"
  ]
```
```
Left aligned                            
               Centered                 
                           Right aligned
This  text  is  justified         evenly
```

### Horizontal rule: `hr`
```haskell
hr
hr' "~"
hr'' "-" 10
```
```
──────────────────────────────────────────────────
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
----------
```

### Vertical rule: `vr`
```haskell
row [vr, vr' "║", vr'' "x" 5]
```
```
│ ║ x
│ ║ x
│ ║ x
│ ║ x
│ ║ x
│ ║
│ ║
│ ║
│ ║
│ ║
```

### Key-value pairs: `kv`
```haskell
kv [("name", "Alice"), ("role", "admin")]
```
```
name: Alice
role: admin
```

### Table: `table`
Tables automatically handle alignment and borders:
```haskell
table ["Name", "Age", "City"] 
  [ ["Alice", "30", "New York"]
  , ["Bob", "25", ""]
  , ["Charlie", "35", "London"]
  ]
```
```
┌─────────┬─────┬─────────┐
│ Name    │ Age │ City    │
├─────────┼─────┼─────────┤
│ Alice   │ 30  │ New York│
│ Bob     │ 25  │         │
│ Charlie │ 35  │ London  │
└─────────┴─────┴─────────┘
```

### Unordered Lists: `ul`
Clean unordered lists with automatic nesting:
```haskell
ul ["Feature A", "Feature B", "Feature C"]
```
```
• Feature A
• Feature B
• Feature C
```

Nested lists with auto-styling:
```haskell
ul [ "Backend"
   , ul ["API", "Database"]
   , "Frontend"
   , ul ["Components", ul ["Header", ul ["Footer"]]]
   ]
```
```
• Backend
  ◦ API
  ◦ Database
• Frontend
  ◦ Components
    ▪ Header
      • Footer
```

### Ordered Lists: `ol`
Numbered lists with automatic nesting:
```haskell
ol ["First step", "Second step", "Third step"]
```
```
1. First step
2. Second step
3. Third step
```

Nested ordered lists with automatic style cycling (numbers → letters → roman numerals):
```haskell
ol [ "Setup"
   , ol ["Install dependencies", "Configure", ol ["Check version"]]
   , "Build"
   , "Deploy"
   ]
```
```
1. Setup
  a. Install dependencies
  b. Configure
    i. Check version
2. Build
3. Deploy
```

### Underline: `underline`
Add underlines to any element:
```haskell
underline "Important Title"
underline' "=" $ text "Custom"  -- Use text for custom underline char
```
```
Important Title
───────────────

Custom
══════
```

### Box: `box`
With title:
```haskell
box "Summary" [kv [("total", "42")]]
```
```
┌──Summary───┐
│ total: 42  │
└────────────┘
```

Without title:
```haskell
box "" [kv [("total", "42")]]
```
```
┌────────────┐
│ total: 42  │
└────────────┘
```

### Status card: `statusCard`
```haskell
statusCard "CPU" "45%"
```
```
┌───────┐
│ CPU   │
│ 45%   │
└───────┘
```

### Progress bar: `inlineBar`
```haskell
inlineBar "Download" 0.75
```
```
Download [███████████████─────] 75%
```

### Tree: `tree`
```haskell
tree "Project" 
  [ branch "src" 
      [ leaf "main.hs"
      , leaf "test.hs"
      ]
  , branch "docs"
      [ leaf "README.md"
      ]
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

### Chart: `chart`
```haskell
chart [("Web", 10), ("Mobile", 20), ("API", 15)]
```
```
Web    │████████████████████ 10
Mobile │████████████████████████████████████████ 20
API    │██████████████████████████████ 15
```

### Padding: `pad`
Add uniform padding around any element:
```haskell
pad 2 $ text "content"
```
```
        
        
  content  
        
        
```

### Centering: `center`
Smart auto-centering and manual width:
```haskell
center "Auto-centered"     -- Uses layout context
center' 20 "Manual width"  -- Fixed width
```
```
        Auto-centered        

    Manual width    
```

### Margin: `margin`
Use `margin` for "compiler-style" prefixes:

```haskell
layout
  [ margin "[error]" ["Type error: expected Int, got String"]
  , margin "[warn]" ["Unused variable 'temp'"] 
  , margin "[success]" ["Build completed successfully"]
  , margin "[info]" ["Pro tip: Use layoutz for beautiful output"]
  ]
```
```
[error] Type error: expected Int, got String
[warn] Unused variable 'temp'
[success] Build completed successfully
[info] Pro tip: Use layoutz for beautiful output
```

## Border Styles
Elements like `box`, `table`, and `statusCard` support different border styles:

**BorderNormal** (default):
```haskell
box "Title" ["content"]
```
```
┌──Title──┐
│ content │
└─────────┘
```

**BorderDouble**:
```haskell
withBorder BorderDouble $ statusCard "API" "UP"
```
```
╔═══════╗
║ API   ║
║ UP    ║
╚═══════╝
```

**BorderThick**:
```haskell
withBorder BorderThick $ table ["Name"] [["Alice"]]
```
```
┏━━━━━━━┓
┃ Name  ┃
┣━━━━━━━┫
┃ Alice ┃
┗━━━━━━━┛
```

**BorderRound**:
```haskell
withBorder BorderRound $ box "Info" ["content"]
```
```
╭──Info───╮
│ content │
╰─────────╯
```

**BorderNone** (invisible borders):
```haskell
withBorder BorderNone $ box "Info" ["content"]
```
```
  Info   
 content 
         
```

## Colors (ANSI Support)

```haskell
withColor ColorRed $ text "Error!"
withColor ColorGreen $ statusCard "Status" "OK"
underlineColored "=" ColorRed $ text "Section Title"
```

**Colors:**
- `ColorBlack` `ColorRed` `ColorGreen` `ColorYellow` `ColorBlue` `ColorMagenta` `ColorCyan` `ColorWhite`
- `ColorBrightBlack` `ColorBrightRed` `ColorBrightGreen` `ColorBrightYellow` `ColorBrightBlue` `ColorBrightMagenta` `ColorBrightCyan` `ColorBrightWhite`
- `ColorNoColor` *(for conditional formatting)*

## Styles (ANSI Support)

```haskell
withStyle StyleBold $ text "Important"
withColor ColorRed $ withStyle StyleBold $ text "Critical Error"
```

**Styles:**
- `StyleBold` `StyleDim` `StyleItalic` `StyleUnderline`
- `StyleBlink` `StyleReverse` `StyleHidden` `StyleStrikethrough`
- `StyleNoStyle` *(for conditional formatting)*

## Custom Components

Create your own components by implementing the `Element` typeclass — no component library needed:

```haskell
data Square = Square Int

instance Element Square where
  renderElement (Square size) 
    | size < 2 = ""
    | otherwise = intercalate "\n" (top : middle ++ [bottom])
    where
      width = size * 2 - 2
      top = "┌" ++ replicate width '─' ++ "┐"
      middle = replicate (size - 2) ("│" ++ replicate width ' ' ++ "│")
      bottom = "└" ++ replicate width '─' ++ "┘"

-- Use it like any other element
main = putStrLn $ render $ layout
  [ text "Here's a square:"
  , L (Square 5)
  , withColor ColorBlue $ L (Square 3)
  ]
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

## Inspiration
- Original Scala [layoutz](https://github.com/mattlianje/layoutz)
