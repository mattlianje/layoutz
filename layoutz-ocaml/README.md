<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-ocaml-demo.png" width="700">
</p>

# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="40"> layoutz

**Simple, beautiful CLI output for OCaml**

A tiny, zero-dep lib for building composable ANSI strings, terminal plots, and interactive Elm-style TUIs in pure OCaml.

Part of [d4](https://github.com/mattlianje/d4) · Also in [Scala](https://github.com/mattlianje/layoutz), [Haskell](https://github.com/mattlianje/layoutz/tree/master/layoutz-hs)

## Features
- Zero dependencies (just stdlib)
- Use [`layoutz.ml`](lib/layoutz.ml) like a header file
- Elm-style TUIs (`run_app`, `run_app_final`)
- Layout primitives, tables, trees, lists
- Colors, ANSI styles, rich formatting
- Terminal charts and plots
- Border styles, spinners
- Extend `ELEMENT` and easily create new primitives
  - (No component library limitations)
- Thread-safe, purely functional rendering

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/showcase-demo.gif" width="650">
<br>
<sub><a href="showcase_app.ml">showcase_app.ml</a></sub>
</p>

Layoutz also lets you easily drop animations into build scripts or any processes that use Stdout:

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/inline-demo.gif" width="650">
<br>
<sub><a href="inline_loading_demo.ml">inline_loading_demo.ml</a></sub>
</p>

## Table of Contents
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Core Concepts](#core-concepts)
- [Border Styles](#border-styles)
- [Elements](#elements)
- [Colors & Styles](#colors--styles)
- [Charts & Plots](#charts--plots)
- [Interactive Apps](#interactive-apps)
- [Custom Elements](#custom-elements)

## Installation

```bash
opam install layoutz
```
```dune
(libraries layoutz)
```

Or just drop the zero-dep [`lib/layoutz.ml`](lib/layoutz.ml) into your project like a header file.

## Quick Start

There are two usage paths:

**(1/2) Static rendering**

Beautiful + compositional strings

<details>
<summary>show code</summary>

```ocaml
open Layoutz

let demo =
  layout
    [ center
        (row
           [ s "Layoutz" |> styleBold
           ; underlineColored ~char:"^" ~color:Color.cyan (s "DEMO")
           ])
    ; br
    ; row
        [ statusCard ~label:(s "Users") ~content:(s "1.2K")
        ; statusCard ~label:(s "API") ~content:(s "UP") |> borderDouble
        ; statusCard ~label:(s "CPU") ~content:(s "23%") |> borderThick |> colorRed
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

</details>
<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-intro.png" width="700">
</p>

**(2/2) Interactive apps**

Build Elm-style TUIs

```ocaml
open Layoutz

type msg = Inc | Dec

let counter_app = {
  init = (0, CmdNone);
  update = (fun msg count -> match msg with
    | Inc -> (count + 1, CmdNone)
    | Dec -> (count - 1, CmdNone));
  subscriptions = (fun _ -> sub_key_press (fun key -> match key with
    | KeyChar '+' -> Some Inc
    | KeyChar '-' -> Some Dec
    | _ -> None));
  view = (fun count ->
    layout [
      section ~title:"Counter" [ s ("Count: " ^ string_of_int count) ];
      br;
      ul [ li (s "Press `+` or `-`") ]
    ]);
}

let () = run_app counter_app
```

## Why layoutz?
- We have `Printf` and full-blown TUI libraries - but there is a gap in-between
- With LLMs, boilerplate "pretty-print" code is cheaper than ever to generate...
- ...which means more formatting code spawning and polluting domain logic
- **layoutz** is a tiny, declarative DSL to combat this
- Everything is an `element` - immutable and composable
- Implement the `ELEMENT` signature to create any elements you imagine - they compose with all built-ins

## Core Concepts

Every piece of content is an `element`. Elements are immutable and composable.

```ocaml
layout [ elem1; elem2; elem3 ]   (* vertical *)
row [ elem1; elem2 ]             (* horizontal *)
render elem                      (* -> string *)
print elem                       (* render + print *)
```

Styles and borders compose via `|>`:
```ocaml
s "Hello" |> borderRound |> colorCyan |> styleBold
s "Fancy!" |> (styleBold ++ styleItalic ++ styleUnderline)
```

## Border Styles

Applied via `|>` to any element:
```ocaml
box ~title:"Title" [ s "content" ] |> borderRound
table ~headers:[...] [...] |> borderThick
s "Hello" |> borderDouble
```

```
borderNormal                               (* ┌─┐ (default) *)
borderDouble                               (* ╔═╗ *)
borderThick                                (* ┏━┓ *)
borderRound                                (* ╭─╮ *)
borderAscii                                (* +-+ *)
borderBlock                                (* ███ *)
borderDashed                               (* ┌╌┐ *)
borderDotted                               (* ┌┈┐ *)
borderInnerHalfBlock                       (* ▗▄▖ *)
borderOuterHalfBlock                       (* ▛▀▜ *)
borderMarkdown                             (* |-| *)
borderCustom ~corner:"+" ~h:"=" ~v:"|"     (* custom *)
borderNone                                 (* no borders *)
```

## Elements

### Text
```ocaml
s "hello"                                    (* short form *)
text "hello"                                 (* verbose *)
```

### Line Break: `br`, `br'`
```ocaml
layout [ s "Line 1"; br; s "Line 2" ]
layout [ s "Top"; br' 3; s "3 lines down" ]
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
tightRow [ s "["; s "no"; s "gaps"; s "]" ]
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

### Vertical Rule: `vr`
```ocaml
vr ()                                        (* default: 10 high with │ *)
vr ~height:5 ()
```

### Section: `section`
```ocaml
section ~title:"Config" [ kv [ ("env", "prod") ] ]
```
```
=== Config ===
env : prod
```

### Box: `box`
```ocaml
box ~title:"Summary" [ s "All systems go" ]
box ~title:"Fancy" [ s "content" ] |> borderDouble
box ~title:"Smooth" [ s "content" ] |> borderRound
```
```
┌──Summary─────────┐
│ All systems go   │
└──────────────────┘
╔══Fancy═══════╗
║ content      ║
╚══════════════╝
╭──Smooth──────╮
│ content      │
╰──────────────╯
```

### Status Card: `statusCard`
```ocaml
row [
  statusCard ~label:(s "CPU") ~content:(s "45%") |> colorGreen;
  statusCard ~label:(s "MEM") ~content:(s "2.1G") |> colorCyan
]
```
```
┌──────┐ ┌───────┐
│ CPU  │ │ MEM   │
│ 45%  │ │ 2.1G  │
└──────┘ └───────┘
```

### Banner: `banner`
```ocaml
banner (s "System Dashboard") |> borderDouble
```
```
╔════════════════════╗
║                    ║
║  System Dashboard  ║
║                    ║
╚════════════════════╝
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
```ocaml
columns [
  layout [ s "Left Column"; s "Line 2"; s "Line 3" ];
  layout [ s "Right Column"; s "More text" ]
]
```
```
Left Column   Right Column
Line 2        More text
Line 3
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

### Unordered List: `ul`
```ocaml
ul [ li (s "First"); li (s "Second"); li (s "Third") ]
ul [ li ~c:[ li (s "Nested A"); li (s "Nested B") ] (s "Item 1"); li (s "Item 2") ]
```
```
• First
• Second
• Third

• Item 1
  • Nested A
  • Nested B
• Item 2
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

### Tree: `tree`
```ocaml
tree (node ~c:[ node (s "src"); node (s "test"); node (s "README.md") ] (s "project"))
```
```
project
├── src
├── test
└── README.md
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

### Spinner: `spinner`
8 built-in styles:
```ocaml
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Dots     (* ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏ *)
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Line     (* | / - \ *)
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Clock    (* 🕐 🕑 🕒 ... *)
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Bounce   (* ⠁ ⠂ ⠄ ⠂ *)
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Earth    (* 🌍 🌎 🌏 *)
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Moon     (* 🌑 🌒 🌓 🌔 🌕 🌖 🌗 🌘 *)
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Grow     (* ▏ ▎ ▍ ▌ ▋ ▊ ▉ █ ... *)
spinner ~label:"Loading" ~frame:0 ~style:SpinnerStyle.Arrow    (* ← ↖ ↑ ↗ → ↘ ↓ ↙ *)
```

### Alignment: `center`, `left_align`, `right_align`, `justify`, `wrap`
```ocaml
center ~width:30 (s "Centered")
left_align ~width:30 (s "Left")
right_align ~width:30 (s "Right")
justify ~width:30 (s "Spread this out")
wrap ~max_width:20 (s "Long text that should wrap")
```

### Underline: `underline`, `underlineColored`
```ocaml
underline (s "Title")
underline ~char:"=" (s "Double")
underlineColored ~char:"~" ~color:Color.cyan (s "Fancy")
```
```
Title
─────
Double
======
```

### Margin: `margin`, `marginColor`
```ocaml
margin ~prefix:"[info]" (layout [ s "Line 1"; s "Line 2" ])
marginColor ~prefix:"[error]" ~color:Color.red (s "Something failed")
```
```
[info] Line 1
[info] Line 2
```

### Padding & Truncation: `pad`, `truncate`
```ocaml
pad ~padding:2 (s "Padded content")
truncate ~max_width:15 (s "This is a very long text")
truncate ~max_width:20 ~ellipsis:"…" (s "Custom ellipsis example")
```

### Spacing: `space`, `empty`
```ocaml
row [ s "Left"; space; s "Right" ]           (* single space *)
row [ s "Left"; space' 10; s "Right" ]       (* 10 spaces *)
empty                                         (* no-op, conditional rendering *)
```

## Colors & Styles

Foreground with `color*`, background with `bg*`:

```ocaml
s "Error!" |> colorRed
s "Warning" |> colorBrightCyan |> styleBold
s "Alert" |> bgRed |> colorWhite
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-colour-2.png" width="700">
</p>

```ocaml
colorBlack
colorRed
colorGreen
colorYellow
colorBlue
colorMagenta
colorCyan
colorWhite
colorBrightBlack                             (* Bright 8 *)
colorBrightRed
colorBrightGreen
colorBrightYellow
colorBrightBlue
colorBrightMagenta
colorBrightCyan
colorBrightWhite
color256 201                                 (* 256-color palette *)
colorRGB 255 128 0                           (* 24-bit RGB *)
```

Background variants: `bgBlack`, `bgRed`, ... `bg256 201`, `bgRGB 255 128 0`

Raw values for `marginColor`, `underlineColored`:
```ocaml
Color.red, Color.cyan, Color.rgb 255 128 0, Color.None
```

### Styles
```ocaml
s "text" |> styleBold
s "text" |> colorRed |> styleBold
s "text" |> (styleBold ++ styleItalic ++ styleUnderline)
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-styles.png" width="700">
</p>

```ocaml
styleBold
styleDim
styleItalic
styleUnderline
styleBlink
styleReverse
styleHidden
styleStrikethrough
styleBold ++ styleItalic                     (* combine with ++ *)
```

### Color Gradients

```ocaml
(* 256-color palette gradient *)
let palette =
  List.init 31 (fun i -> s "█" |> color256 (16 + i * 7))
  |> tightRow

(* RGB gradients *)
let rainbow =
  List.init 32 (fun i ->
    let v = i * 8 in
    let r = if v < 128 then v * 2 else 255 in
    let g = if v < 128 then 255 else (255 - v) * 2 in
    let b = if v > 128 then (v - 128) * 2 else 0 in
    s "█" |> colorRGB r g b)
  |> tightRow
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ocaml/pix/layoutz-colour.png" width="700">
</p>

## Charts & Plots

### Sparkline
```ocaml
sparkline [1.0; 3.0; 5.0; 7.0; 2.0; 4.0; 8.0; 1.0]
```
```
▁▃▅▇▂▄█▁
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-sparkline.png" width="500">
</p>

### Line Plot
```ocaml
let s1 = series ~points:[(0.,0.); (1.,1.); (2.,4.); (3.,9.)] ~label:"x^2" ~color:Color.None in
plotLine ~width:30 ~height:10 [s1]
```

Multiple series:
```ocaml
let sin_pts = List.init 50 (fun i ->
  let x = float_of_int i in (x, sin (x *. 0.15) *. 5.0)) in
let cos_pts = List.init 50 (fun i ->
  let x = float_of_int i in (x, cos (x *. 0.15) *. 5.0)) in
plotLine ~width:50 ~height:12 [
  series ~points:sin_pts ~label:"sin(x)" ~color:(Color.rgb 0 255 255);
  series ~points:cos_pts ~label:"cos(x)" ~color:(Color.rgb 255 0 255);
]
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-function-2.png" width="500">
</p>

### Pie Chart
```ocaml
plotPie ~width:20 ~height:10 [
  slice ~value:40.0 ~label:"OCaml" ~color:Color.None;
  slice ~value:30.0 ~label:"Haskell" ~color:Color.None;
  slice ~value:30.0 ~label:"Scala" ~color:Color.None;
]
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-pie.png" width="500">
</p>

### Bar Chart
```ocaml
plotBar ~width:20 ~height:8 [
  bar_item ~value:80.0 ~label:"A" ~color:Color.None;
  bar_item ~value:60.0 ~label:"B" ~color:Color.None;
  bar_item ~value:40.0 ~label:"C" ~color:Color.None;
]
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-bar.png" width="500">
</p>

### Stacked Bar Chart
```ocaml
plotStackedBar ~width:20 ~height:8 [
  stacked_group ~segments:[
    bar_item ~value:30.0 ~label:"X" ~color:Color.None;
    bar_item ~value:20.0 ~label:"Y" ~color:Color.None;
  ] ~label:"G1";
  stacked_group ~segments:[
    bar_item ~value:20.0 ~label:"X" ~color:Color.None;
    bar_item ~value:40.0 ~label:"Y" ~color:Color.None;
  ] ~label:"G2";
]
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-stacked.png" width="500">
</p>

### Heatmap
```ocaml
plotHeatmap (heatmap_data
  ~grid:[[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]; [7.0; 8.0; 9.0]]
  ~row_labels:["R1"; "R2"; "R3"]
  ~col_labels:["C1"; "C2"; "C3"])

plotHeatmap ~cell_width:10 data              (* custom cell width *)
```

<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/chart-heatmap.png" width="500">
</p>

## Interactive Apps

The runtime uses the [Elm Architecture](https://guide.elm-lang.org/architecture/) where your view is simply a layoutz `element`.

```ocaml
type ('state, 'msg) app = {
  init : 'state * 'msg cmd;
  update : 'msg -> 'state -> 'state * 'msg cmd;
  subscriptions : 'state -> 'msg sub;
  view : 'state -> element;
}
```

Three daemon threads coordinate rendering, tick/timers, and input capture. State updates flow through `update` synchronously. Rendering uses incremental line-diffing: only changed lines are redrawn.

```ocaml
run_app ~options:{
  alignment = AlignCenter;
  quit_key = KeyCtrl 'Q';
  clear_on_start = true;
  clear_on_exit = true;
  render_interval_ms = 33;
} app

run_app app                                  (* default options *)
run_app_final app                            (* returns final state *)
```

### Key Types
```ocaml
(* Printable *)
KeyChar c                                    (* regular character *)

(* Editing *)
KeyEnter
KeyBackspace
KeyTab
KeyEscape
KeyDelete

(* Navigation *)
KeyUp
KeyDown
KeyLeft
KeyRight
KeyHome
KeyEnd
KeyPageUp
KeyPageDown

(* Modifiers *)
KeyCtrl c                                    (* Ctrl+A, Ctrl+S, etc. *)
```

### Subscriptions

```ocaml
sub_none                                     (* no subscriptions *)
sub_key_press (fun key ->                    (* keyboard input *)
  match key with
  | KeyChar 'q' -> Some Quit
  | _ -> None)
sub_every_ms 100 Tick                        (* periodic ticks *)
sub_batch [sub1; sub2]                       (* combine multiple *)
```

```ocaml
let subscriptions state = sub_batch [
  sub_every_ms 100 Tick;
  sub_key_press (fun key -> match key with
    | KeyChar 'q' -> Some Quit
    | _ -> None);
]
```

### Commands

```ocaml
cmd_none                                     (* no-op *)
cmd_exit                                     (* exit the application *)
cmd_batch [cmd1; cmd2]                       (* execute multiple *)
cmd_task (fun () -> Some msg)                (* async task *)
cmd_after_ms 500 msg                         (* one-shot delayed message *)
```

## Custom Elements

Implement `ELEMENT` to create reusable components:
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
