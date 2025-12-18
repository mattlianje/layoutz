# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="40"> layoutz-ocaml

**Simple, beautiful CLI output for OCaml**

Build declarative and composable sections, trees, tables, and dashboards.
Easily create new primitives (no component-library limitations).

Part of [layoutz](https://github.com/mattlianje/layoutz)

## Features

- Use **Layoutz.ml** like a header-file
- Zero dependencies
- Effortless composition of elements
- Rich text formatting: alignment, wrapping, padding, truncation
- ANSI colors and styles
- Lists, trees, tables, charts, progress bars
- Easy creation of custom elements
- Thread-safe, purely functional rendering

## Install

```bash
opam install layoutz
```

Or add to dune-project:
```
(depends layoutz)
```

## Quickstart

```ocaml
open Layoutz

let status_card label value = box ~title:label [ s value ]

let demo =
  layout
    [
      s "Dashboard" |> styleBold |> center;
      hr;
      row
        [
          status_card "API" "LIVE" |> fg colorGreen;
          status_card "DB" "99.9%";
          status_card "Cache" "READY" |> fg colorCyan;
        ];
      (box ~title:"Services"
        [
          ul [ li (s "Production"); li (s "Staging") ];
          inline_bar ~label:"Health" ~progress:0.94;
        ]
      |> borderRound);
    ]

let () = print demo
```
```
              Dashboard
──────────────────────────────────────────────────
┌─API──┐ ┌─DB────┐ ┌─Cache─┐
│ LIVE │ │ 99.9% │ │ READY │
└──────┘ └───────┘ └───────┘
╭───────────Services────────────╮
│ • Production                  │
│ • Staging                     │
│ Health [██████████████████──] │
╰───────────────────────────────╯
```

## Why layoutz?

- We have `Printf.sprintf`, but there's a gap between raw strings and full TUI libraries
- With LLMs, boilerplate "pretty-print" code is cheaper than ever to generate...
- ...which means more formatting code spawning and polluting domain logic
- **layoutz** is a tiny, declarative DSL to combat this
- Everything is an `Element` - immutable and composable
- Since you can extend the `Element` interface, you can create any elements you imagine... and they compose with all built-ins

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

### Line Break: `br`

```ocaml
layout [ s "Line 1"; br; s "Line 2" ]
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

### Row (horizontal): `row`

```ocaml
row [ s "Left"; s "Middle"; s "Right" ]
row ~tight:true [ s "["; s "no"; s "gaps"; s "]" ]
```
```
Left Middle Right
[nogaps]
```

### Horizontal Rule: `hr`

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

## Colors & Styles

Pipe-friendly - compose with `|>`:

```ocaml
s "error" |> fg colorRed
s "success" |> fg colorGreen
s "highlighted" |> bg colorBlue
s "important" |> styleBold
s "emphasis" |> styleItalic

(* Combine them *)
s "critical" |> fg colorRed |> styleBold
s "WARNING" |> fg colorBlack |> bg colorYellow |> styleBold
```

### Available Colors

- Standard: `colorBlack`, `colorRed`, `colorGreen`, `colorYellow`, `colorBlue`, `colorMagenta`, `colorCyan`, `colorWhite`
- Bright: `colorBrightBlack`, `colorBrightRed`, `colorBrightGreen`, etc.
- 256 palette: `color256 201`
- True color: `colorRGB 255 128 0`

### Available Styles

`styleBold`, `styleDim`, `styleItalic`, `styleUnderline`, `styleBlink`, `styleReverse`, `styleHidden`, `styleStrikethrough`

### Composing Styles

Use `++` to combine styles:

```ocaml
let fancy = styleBold ++ styleItalic
let warning = styleBold ++ styleUnderline ++ fg colorRed

s "important" |> fancy
s "alert" |> warning
```

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

## License

Apache-2.0
