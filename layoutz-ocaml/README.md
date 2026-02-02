<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-ocaml-demo.png" width="700">
</p>

# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="40"> layoutz

**Simple, beautiful CLI output for OCaml**

Build declarative and composable sections, trees, tables, and dashboards.
Easily create new primitives (no component-library limitations).

Part of [d4](https://github.com/mattlianje/d4) • Also in: [JavaScript](https://github.com/mattlianje/layoutz/tree/master/layoutz-ts), [Haskell](https://github.com/mattlianje/layoutz/tree/master/layoutz-hs), [Scala](https://github.com/mattlianje/layoutz)

## Installation

```bash
opam install layoutz
```

Then add to your `dune` file:

```lisp
(library
 (name myproject)
 (libraries layoutz))
```

## Features

- Use **Layoutz.ml** like a header-file
- Zero dependencies
- Effortless composition of elements
- Rich text formatting: alignment, wrapping, padding, truncation
- ANSI colors and styles
- Lists, trees, tables, charts, progress bars
- Easy creation of custom elements
- Thread-safe, purely functional rendering

## Quickstart

```ocaml
open Layoutz

let demo =
  layout
    [ center
        (row
           [ s "Layoutz" |> styleBold
           ; underlineColored ~char:"^" ~color:colorCyan (s "DEMO")
           ])
    ; br
    ; row
        [ statusCard ~label:(s "Users") ~content:(s "1.2K")
        ; statusCard ~label:(s "API") ~content:(s "UP") |> borderDouble
        ; statusCard ~label:(s "CPU") ~content:(s "23%") |> borderThick |> fg colorRed
        ; table
            ~headers:[ s "Name"; s "Role"; s "Skills" ]
            [ [ s "Gegard"; s "Pugilist"
              ; ul [ li ~c:[ li ~c:[ li (s "man") ] (s "bad") ] (s "Armenian") ]
              ]
            ; [ s "Eve"; s "QA"; s "Testing" ]
            ]
          |> borderRound |> styleReverse
        ]
    ]

let () = print demo
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-intro.png" width="700">
</p>

## Why layoutz?

- With LLMs, boilerplate "pretty-print" code is cheaper than ever to generate...
- ...which means more formatting code spawning and polluting domain logic
- **layoutz** is a tiny, declarative DSL to combat this
- Everything is an `Element` - immutable and composable
- Since you can implement the `ELEMENT` signature, you can create any elements you imagine... and they compose with all built-ins

## Core Concepts

- Every piece of content is an `element`
- Elements are **immutable** and **composable** - build complex layouts by combining simple ones
- A `layout` arranges elements **vertically**, a `row` arranges them **horizontally**
- Call `render` to get a string, or `print` to print directly
- The power comes from **uniform composition** - everything is an element, everything combines

## Elements

### Text: `s`, `text`

```ocaml
s "hello"           (* short form *)
text "hello"        (* verbose *)
```

### Line Break: `br`, `br'`

```ocaml
layout [ s "Line 1"; br; s "Line 2" ]
layout [ s "Top"; br' 3; s "3 lines down" ]
```

### Space: `space`, `space'`

```ocaml
row [ s "Left"; space; s "Right" ]         (* single space *)
row [ s "Left"; space' 10; s "Right" ]     (* 10 spaces *)
```

### Empty: `empty`

Useful for conditional rendering:
```ocaml
layout [
  s "Always shown";
  (if has_error then s "Error!" |> fg colorRed else empty);
  s "Footer"
]
```

### Layout (vertical): `layout`

```ocaml
layout [ s "First"; s "Second"; s "Third" ]
```
```
First
Second
Third
```

### Row (horizontal): `row`, `tightRow`

```ocaml
row [ s "Left"; s "Middle"; s "Right" ]
row ~tight:true [ s "["; s "no"; s "gaps"; s "]" ]
tightRow [ s "["; s "same"; s "thing"; s "]" ]  (* convenience *)
```
```
Left Middle Right
[nogaps]
```

### Horizontal Rule: `hr`, `hr'`

```ocaml
hr
hr' ~width:20 ()
hr' ~char:"=" ~width:20 ()
```
```
──────────────────────────────────────────────────
────────────────────
====================
```

### Vertical Rule: `vr`

```ocaml
vr ()                      (* default: 10 high with │ *)
vr ~height:5 ()
vr ~char:"┃" ~height:3 ()
```
```
│
│
│
│
│
```

### Box: `box`

```ocaml
box ~title:"Status" [ s "All systems go" ]
box ~title:"Fancy" [ s "Double border" ] |> borderDouble
box ~title:"Smooth" [ s "Rounded corners" ] |> borderRound
```
```
┌──Status───────┐
│ All systems go│
└───────────────┘
╔══Fancy════════╗
║ Double border ║
╚═══════════════╝
╭──Smooth─────────╮
│ Rounded corners │
╰─────────────────╯
```

Pipe any element through `borderRound`, `borderDouble`, `borderThick`, `borderNormal`, `borderNone`:

```ocaml
s "Hello" |> borderRound
box ~title:"X" [ s "content" ] |> borderDouble
table ~headers:[...] [...] |> borderThick
```

### Table: `table`

```ocaml
table
  ~headers:[ s "Name"; s "Age" ]
  [ [ s "Alice"; s "30" ]; [ s "Bob"; s "25" ] ]
```
```
┌───────┬─────┐
│ Name  │ Age │
├───────┼─────┤
│ Alice │ 30  │
│ Bob   │ 25  │
└───────┴─────┘
```

### Columns: `columns`

Multi-column layout with automatic height alignment:

```ocaml
columns [
  layout [ s "Left Column"; s "Line 2"; s "Line 3" ];
  layout [ s "Right Column"; s "More text" ]
]

columns ~spacing:4 [ s "Wide"; s "Gaps" ]
```
```
Left Column   Right Column
Line 2        More text
Line 3
```

### Banner: `banner`

Decorative bordered banner (double border by default):

```ocaml
banner (s "Welcome!")
banner (s "System Status") |> borderRound
```
```
╔═════════════╗
║             ║
║  Welcome!   ║
║             ║
╚═════════════╝
```

### Status Card: `statusCard`

Compact label-value display in a box:

```ocaml
row [
  statusCard ~label:(s "CPU") ~content:(s "45%") |> fg colorGreen;
  statusCard ~label:(s "MEM") ~content:(s "2.1G") |> fg colorCyan
]
```
```
┌──────┐ ┌───────┐
│ CPU  │ │ MEM   │
│ 45%  │ │ 2.1G  │
└──────┘ └───────┘
```

### Unordered List: `ul`

```ocaml
ul [ li (s "First"); li (s "Second"); li (s "Third") ]
ul ~bullet:"-" [ li (s "Custom"); li (s "Bullet") ]
```
```
• First
• Second
• Third

- Custom
- Bullet
```

### Ordered List: `ol`

```ocaml
ol [ li (s "Step one"); li (s "Step two"); li (s "Step three") ]
```
```
1. Step one
2. Step two
3. Step three
```

### Nested Lists

```ocaml
ul [
  li ~c:[ li (s "Nested A"); li (s "Nested B") ] (s "Item 1");
  li (s "Item 2")
]
```
```
• Item 1
  • Nested A
  • Nested B
• Item 2
```

### Tree: `tree`

```ocaml
tree
  (node ~c:[ node (s "src"); node (s "test"); node (s "README.md") ] (s "project"))
```
```
project
├── src
├── test
└── README.md
```

### Key-Value: `kv`

```ocaml
kv [ ("Name", "Alice"); ("Age", "30"); ("City", "NYC") ]
```
```
Name: Alice
Age:  30
City: NYC
```

### Section: `section`

```ocaml
section ~title:"Status" [ s "All systems operational" ]
```
```
=== Status ===
All systems operational
```

### Progress Bar: `inline_bar`

```ocaml
inline_bar ~label:"Download" ~progress:0.75
```
```
Download [███████████████─────] 75%
```

### Chart: `chart`

```ocaml
chart [ ("OCaml", 85.0); ("Haskell", 72.0); ("Scala", 90.0) ]
```
```
OCaml   │████████████████ 85.0
Haskell │█████████████ 72.0
Scala   │█████████████████ 90.0
```

### Alignment

```ocaml
(* Auto-center - width computed from siblings *)
layout [ s "A long title here"; center (s "Auto centered!"); s "Footer" ]

(* Explicit width *)
center ~width:30 (s "Centered")
left_align ~width:30 (s "Left")
right_align ~width:30 (s "Right")
```

### Truncation: `truncate`

```ocaml
truncate ~max_width:15 (s "This is a very long text")
truncate ~max_width:20 ~ellipsis:"…" (s "Custom ellipsis example")
```
```
This is a ve...
Custom ellipsis e…
```

### Word Wrap: `wrap`

```ocaml
wrap ~max_width:20 (s "This is a long line that will be wrapped at word boundaries")
```
```
This is a long line
that will be wrapped
at word boundaries
```

### Justification: `justify`, `justifyAll`

```ocaml
justify ~width:30 (s "Spaces are\ndistributed evenly")
justifyAll ~width:30 (s "Even the\nlast line")  (* justifies last line too *)
```
```
Spaces          are
distributed   evenly
```

### Padding: `pad`

Add uniform padding around any element:

```ocaml
pad ~padding:2 (s "Padded content")
box ~title:"Box" [ s "content" ] |> pad ~padding:1
```

### Margin: `margin`, `marginColor`

Prefix each line with a string (great for compiler-style output):

```ocaml
margin ~prefix:"[info]" (layout [ s "Line 1"; s "Line 2" ])
marginColor ~prefix:"[error]" ~color:colorRed (s "Something failed")
```
```
[info] Line 1
[info] Line 2
```

### Underline: `underline`, `underlineColored`

```ocaml
underline (s "Title")
underline ~char:"=" (s "Double")
underlineColored ~char:"~" ~color:colorCyan (s "Fancy")
```
```
Title
─────

Double
======
```

## Colors & Styles

### Colors

Add ANSI coloring with `|> fg` and `color<...>` to see what is available:

```ocaml
let colors =
  layout
    [ s "The quick brown fox..." |> fg colorRed
    ; s "The quick brown fox..." |> fg colorBrightCyan
    ; underlineColored ~char:"~" ~color:colorRed (s "The quick brown fox...")
    ; marginColor ~prefix:"[INFO]" ~color:colorCyan (s "The quick brown fox...")
    ]

let () = print colors
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-colour-2.png" width="700">
</p>

**Available Colors:**
- Standard: `colorBlack`, `colorRed`, `colorGreen`, `colorYellow`, `colorBlue`, `colorMagenta`, `colorCyan`, `colorWhite`
- Bright: `colorBrightBlack`, `colorBrightRed`, `colorBrightGreen`, `colorBrightCyan`, etc.
- 256 palette: `color256 201`
- True color: `colorRGB 255 128 0`

### Styles

ANSI styles are added the same way with `|> style<...>`:

```ocaml
let styles =
  layout
    [ s "The quick brown fox..." |> styleBold
    ; s "The quick brown fox..." |> fg colorRed |> styleBold
    ; s "The quick brown fox..." |> styleReverse |> styleItalic
    ]

let () = print styles
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-styles.png" width="700">
</p>

**Available Styles:** `styleBold`, `styleDim`, `styleItalic`, `styleUnderline`, `styleBlink`, `styleReverse`, `styleHidden`, `styleStrikethrough`

### Combining Styles

Use `++` to combine multiple styles at once:

```ocaml
let combined =
  layout
    [ s "Fancy!" |> (styleBold ++ styleItalic ++ styleUnderline)
    ; table
        ~headers:[ s "Name"; s "Role"; s "Status" ]
        [ [ s "Alice"; s "Engineer"; s "Online" ]
        ; [ s "Eve"; s "QA"; s "Away" ]
        ; [ ul [ li ~c:[ li ~c:[ li (s "was a BAD man") ] (s "Mousasi") ] (s "Gegard") ]
          ; s "Fighter"
          ; s "Nasty"
          ]
        ]
      |> borderRound |> (styleBold ++ styleReverse)
    ]

let () = print combined
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-combined.png" width="700">
</p>

### Color Gradients

```ocaml
(* 256-color palette gradient *)
let palette =
  List.init 31 (fun i -> s "█" |> fg (color256 (16 + i * 7)))
  |> tightRow

(* RGB gradients *)
let red_to_blue =
  List.init 32 (fun i ->
    let v = i * 8 in
    s "█" |> fg (colorRGB v 100 (255 - v)))
  |> tightRow

let rainbow =
  List.init 32 (fun i ->
    let v = i * 8 in
    let r = if v < 128 then v * 2 else 255 in
    let g = if v < 128 then 255 else (255 - v) * 2 in
    let b = if v > 128 then (v - 128) * 2 else 0 in
    s "█" |> fg (colorRGB r g b))
  |> tightRow

let () = print (layout [ palette; red_to_blue; rainbow ])
```
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-colour.png" width="700">
</p>

## Custom Elements

Create your own by implementing the `ELEMENT` signature:

```ocaml
module Square : ELEMENT with type t = int = struct
  type t = int
  let render size =
    let h = String.concat "" (List.init (size * 2) (fun _ -> "─")) in
    let top = "┌" ^ h ^ "┐" in
    let mid = "│" ^ String.make (size * 2) ' ' ^ "│" in
    let bot = "└" ^ h ^ "┘" in
    String.concat "\n" ([top] @ List.init size (fun _ -> mid) @ [bot])
  let width size = (size * 2) + 2
  let height size = size + 2
end

let square size = el (module Square) size
```

Then use it like any built-in:

```ocaml
row [ square 3; square 5; square 7 ]
```
```
┌──────┐ ┌──────────┐ ┌──────────────┐
│      │ │          │ │              │
│      │ │          │ │              │
│      │ │          │ │              │
└──────┘ │          │ │              │
         │          │ │              │
         └──────────┘ │              │
                      │              │
                      └──────────────┘
```

For bordered custom elements, implement `BORDERABLE` instead (adds `with_border` and `get_border`).
