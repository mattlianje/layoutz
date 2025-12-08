# layoutz-ocaml

Friendly, expressive print-layout DSL for OCaml. Zero dependencies.

```ocaml
open Layoutz

let _ = put_str_ln (layout [
  s "Dashboard" |> style styleBold;
  hr;
  row [
    box ~title:"CPU" [s "███░░ 60%" |> fg colorGreen];
    box ~title:"RAM" [s "████░ 80%" |> fg colorYellow]
  ];
  table ~headers:[s"Service"; s"Status"] [
    [s"api"; s"running" |> fg colorGreen];
    [s"db"; s"running" |> fg colorGreen]
  ]
])
```

## Install

```bash
opam install layoutz
```

Or add to your dune-project:
```
(depends layoutz)
```

## Usage

```bash
# REPL
dune utop lib

# In code
open Layoutz
```

## Elements

### Text
```ocaml
s "hello"           (* short form *)
text "hello"        (* verbose form *)
```

### Layout (vertical)
```ocaml
layout [s "line 1"; s "line 2"; s "line 3"]
```

### Row (horizontal)
```ocaml
row [s "left"; s "middle"; s "right"]
row ~tight:true [s "["; s "no"; s "gaps"; s "]"]
```

### Box
```ocaml
box ~title:"Title" [s "content"]
box ~border:Border.Double ~title:"Fancy" [s "double border"]
box ~border:Border.Round ~title:"Smooth" [s "rounded corners"]
```

Border styles: `Border.Normal`, `Border.Double`, `Border.Thick`, `Border.Round`, `Border.None`

### Table
Tables take elements, so you can style cells:
```ocaml
table ~headers:[s"Name"; s"Status"] [
  [s"Alice"; s"online" |> fg colorGreen];
  [s"Bob"; s"offline" |> fg colorRed]
]
```

### Horizontal Rule
```ocaml
hr                              (* default: 50 chars wide *)
hr' ~width:30 ()                (* custom width *)
hr' ~char:"=" ~width:20 ()      (* custom char *)
```

### Line Break
```ocaml
br
```

### Tree
```ocaml
tree (
  node ~children:[
    node ~children:[
      node (s "file1.ml");
      node (s "file2.ml")
    ] (s "src");
    node ~children:[
      node (s "test1.ml");
      node (s "test2.ml")
    ] (s "test");
    node (s "README.md");
    node (s "dune-project")
  ] (s "my-project")
)
(* Output:
   my-project
   ├── src
   │   ├── file1.ml
   │   └── file2.ml
   ├── test
   │   ├── test1.ml
   │   └── test2.ml
   ├── README.md
   └── dune-project
*)
```

### Lists
```ocaml
(* Unordered list *)
ul [li (s "First"); li (s "Second"); li (s "Third")]

(* Ordered list *)
ol [li (s "Step one"); li (s "Step two"); li (s "Step three")]

(* Nested lists - bullets rotate: • ◦ ▪ ▫ *)
ul [
  li ~children:[
    li ~children:[li (s "Deep 1"); li (s "Deep 2")] (s "Sub A");
    li (s "Sub B")
  ] (s "Item 1");
  li (s "Item 2")
]
(* Output:
   • Item 1
     ◦ Sub A
       ▪ Deep 1
       ▪ Deep 2
     ◦ Sub B
   • Item 2
*)

(* Ordered nested - numbers rotate: 1. a. i. *)
ol [
  li ~children:[
    li ~children:[li (s "Do X"); li (s "Do Y")] (s "Part A");
    li (s "Part B")
  ] (s "First");
  li (s "Second")
]
(* Output:
   1. First
      a. Part A
          i. Do X
         ii. Do Y
      b. Part B
   2. Second
*)
```

## Styling

Pipe-friendly ANSI styles - compose with `|>`:

```ocaml
(* Colors *)
s "error" |> fg colorRed
s "success" |> fg colorGreen
s "orange" |> fg (colorRGB 255 128 0)
s "pink" |> fg (color256 201)
s "highlighted" |> bg colorBlue

(* Styles *)
s "important" |> style styleBold
s "emphasis" |> style styleItalic
s "link" |> style styleUnderline
s "muted" |> style styleDim

(* Combine them *)
s "critical error" |> fg colorRed |> style styleBold
s "WARNING" |> fg colorBlack |> bg colorYellow |> style styleBold

(* Full control *)
styled ~fg:colorRed ~bg:colorWhite ~style:(styleBold ++ styleUnderline) (s "fancy")
```

### Available Colors
- Standard: `colorBlack`, `colorRed`, `colorGreen`, `colorYellow`, `colorBlue`, `colorMagenta`, `colorCyan`, `colorWhite`
- Bright: `colorBrightBlack`, `colorBrightRed`, `colorBrightGreen`, `colorBrightYellow`, `colorBrightBlue`, `colorBrightMagenta`, `colorBrightCyan`, `colorBrightWhite`
- 256 palette: `color256 n` (0-255)
- True color: `colorRGB r g b`

### Available Styles
`styleBold`, `styleDim`, `styleItalic`, `styleUnderline`, `styleBlink`, `styleReverse`, `styleHidden`, `styleStrikethrough`

### Composing Styles
Styles can be composed with the `++` operator:
```ocaml
(* Combine multiple styles *)
let fancy = styleBold ++ styleItalic
let warning = styleBold ++ styleUnderline

(* Apply composed styles *)
s "important" |> style fancy
s "alert" |> style (styleBold ++ styleReverse)
s "fancy header" |> style (styleBold ++ styleItalic ++ styleUnderline) |> fg colorCyan
```

## Custom Elements

Implement the `ELEMENT` signature to create your own elements:

```ocaml
open Layoutz

module Square : ELEMENT = struct
  type t = int

  let create size = size

  let render size =
    let top = "┌" ^ String.concat "" (List.init size (fun _ -> "─")) ^ "┐" in
    let mid = "│" ^ String.make size ' ' ^ "│" in
    let bot = "└" ^ String.concat "" (List.init size (fun _ -> "─")) ^ "┘" in
    unlines ([top] @ List.init size (fun _ -> mid) @ [bot])

  let width size = size + 2
  let height size = size + 2
end

(* Wrap with `el` to use alongside built-in elements *)
let square size = el (module Square) (Square.create size)

let _ = put_str_ln (row [square 3; square 5])
(* Output:
   ┌───┐ ┌─────┐
   │   │ │     │
   │   │ │     │
   │   │ │     │
   └───┘ │     │
         │     │
         └─────┘
*)
```

## API

### Core Functions
- `render : element -> string` - render to string
- `width : element -> int` - get visual width
- `height : element -> int` - get height in lines
- `put_str_ln : element -> unit` - print to stdout

### Element Constructor
- `el : (module ELEMENT with type t = 'a) -> 'a -> element` - wrap custom element

### Utilities (for custom elements)
- `visible_length : string -> int` - UTF-8 aware string length (ignores ANSI)
- `lines : string -> string list` - split into lines
- `unlines : string list -> string` - join with newlines
- `pad_right : int -> string -> string` - right-pad to width
- `pad_left : int -> string -> string` - left-pad to width
- `center_string : int -> string -> string` - center in width

## License

Apache-2.0
