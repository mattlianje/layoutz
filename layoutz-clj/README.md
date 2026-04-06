# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="40"> layoutz

**Simple, beautiful CLI output for Clojure**

A tiny, zero-dep lib for building composable ANSI strings, terminal plots, and interactive Elm-style TUIs in pure Clojure.

Part of [d4](https://github.com/mattlianje/d4) · Also in [Scala](https://github.com/mattlianje/layoutz), [OCaml](https://github.com/mattlianje/layoutz/tree/master/layoutz-ocaml), [Haskell](https://github.com/mattlianje/layoutz/tree/master/layoutz-hs), [TypeScript](https://github.com/mattlianje/layoutz/tree/master/layoutz-ts)

## Features
- Zero dependencies
- Use [`core.clj`](src/layoutz/core.clj) like a header file
- Elm-style TUIs (`run-app`, `run-inline`)
- Layout primitives, tables, trees, lists
- Colors, ANSI styles, rich formatting
- Terminal charts and plots
- Border styles, spinners
- Implement `Element` protocol to create new primitives
- Thread-safe, purely functional rendering

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/showcase-demo.gif" width="650">
<br>
<sub><a href="src/layoutz/showcase.clj">showcase.clj</a></sub>
</p>

Layoutz also lets you easily drop animations into build scripts or any processes that use Stdout:

<p align="center">
<img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/inline-demo.gif" width="650">
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

## Installation

Add to `deps.edn`:

```clojure
{:deps {xyz.matthieucourt/layoutz {:git/url "https://github.com/mattlianje/layoutz"
                                    :git/sha "..."
                                    :deps/root "layoutz-clj"}}}
```

Or just drop [`src/layoutz/core.clj`](src/layoutz/core.clj) into your project like a header file.

## Quick Start

There are two usage paths:

**(1/2) Static rendering**

Beautiful + compositional strings

```clojure
(require '[layoutz.core :refer :all])

(def demo
  (layout
    [(center
       (row
         [(-> "Layoutz" style-bold)
          (underline-colored "DEMO" "^" bright-magenta)]))
     br
     (row
       [(status-card "Users" "1.2K")
        (-> (status-card "API" "UP") border-double)
        (-> (status-card "CPU" "23%") border-thick color-red)
        (-> (table ["Name" "Role" "Skills"]
                   [["Gegard" "Pugilist"
                     (ul [(li "Armenian"
                              :c [(li "bad"
                                      :c [(li "man")])])])]
                    ["Eve" "QA" "Testing"]])
            border-round style-reverse)])]))

(print-elem demo)
```

**(2/2) Interactive apps**

Build Elm-style TUIs

```clojure
(require '[layoutz.core :refer :all])

(run-app
  {:init (fn [] [{:n 0} (cmd-none)])
   :update (fn [msg state]
             (case msg
               :inc [(update state :n inc) (cmd-none)]
               :dec [(update state :n dec) (cmd-none)]
               [state (cmd-none)]))
   :subscriptions (fn [_]
                    (sub-key-press
                      (fn [{:keys [type]}]
                        (case type :up :inc :down :dec nil))))
   :view (fn [{:keys [n]}]
           (layout
             [(section "Counter" [(str "Count: " n)])
              br
              (ul [(li "Press up/down") (li "Ctrl-Q to quit")])]))})
```

## Why layoutz?
- We have `printf` and full-blown TUI libraries - but there is a gap in-between
- **layoutz** is a tiny, declarative DSL to combat this
- Everything is an `Element` - immutable and composable
- Implement the `Element` protocol to create any elements you imagine - they compose with all built-ins

## Core Concepts

Every piece of content is an `Element`. Elements are immutable and composable.

```clojure
(layout [elem1 elem2 elem3])         ;; vertical
(row [elem1 elem2])                  ;; horizontal
(render elem)                        ;; -> string
(print-elem elem)                    ;; render + print
```

Styles and borders compose via `->`:
```clojure
(-> "Hello" border-round color-cyan style-bold)
```

Plain strings implement `Element` - use them directly everywhere.

## Border Styles

Applied via `->` to any element:
```clojure
(-> (box "Title" ["content"]) border-round)
(-> (table headers rows) border-thick)
(-> "Hello" border-double)
```

```
border-normal                        ;; ┌─┐ (default)
border-double                        ;; ╔═╗
border-thick                         ;; ┏━┓
border-round                         ;; ╭─╮
border-ascii                         ;; +-+
border-block                         ;; ███
border-dashed                        ;; ┌╌┐
border-dotted                        ;; ┌┈┐
border-inner-half-block              ;; ▗▄▖
border-outer-half-block              ;; ▛▀▜
border-markdown                      ;; |-|
(border-custom "+" "=" "|")          ;; custom
border-none                          ;; no borders
```

```clojure
(-> (box "Status" ["All systems go"]) border-round)
;; ╭──Status──────────╮
;; │ All systems go   │
;; ╰──────────────────╯

(-> (box "Fancy" ["Double border"]) border-double)
;; ╔══Fancy═══════════╗
;; ║ Double border    ║
;; ╚══════════════════╝
```

## Elements

### Text
```clojure
(s "hello")                          ;; short form
(text "hello")                       ;; verbose
"hello"                              ;; strings are elements
```

### Line Break: `br`, `br'`
```clojure
(layout ["Line 1" br "Line 2"])
(layout ["Top" (br' 3) "3 lines down"])
```

### Layout (vertical): `layout`
```clojure
(layout ["First" "Second" "Third"])
```
```
First
Second
Third
```

### Row (horizontal): `row`, `tight-row`
```clojure
(row ["Left" "Middle" "Right"])
(tight-row ["[" "no" "gaps" "]"])
```
```
Left Middle Right
[nogaps]
```

### Horizontal Rule: `hr`, `hr'`
```clojure
hr                                   ;; ──────────────────────────────
(hr' :width 20)                      ;; ────────────────────
(hr' :char "=" :width 20)            ;; ====================
```

### Vertical Rule: `vr`
```clojure
(vr)                                 ;; default: 10 high with │
(vr :height 5)
```

### Section: `section`
```clojure
(section "Config" [(kv [["env" "prod"] ["region" "us-east-1"]])])
```
```
=== Config ===
env:    prod
region: us-east-1
```

### Box: `box`
```clojure
(box "Summary" ["All systems go"])
(-> (box "Fancy" ["content"]) border-double)
(-> (box "Smooth" ["content"]) border-round)
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
```clojure
(row [(-> (status-card "CPU" "45%") color-green)
      (-> (status-card "MEM" "2.1G") color-cyan)])
```
```
┌──────┐ ┌───────┐
│ CPU  │ │ MEM   │
│ 45%  │ │ 2.1G  │
└──────┘ └───────┘
```

### Banner: `banner`
```clojure
(banner "System Dashboard")
```
```
╔════════════════════╗
║                    ║
║  System Dashboard  ║
║                    ║
╚════════════════════╝
```

### Table: `table`
```clojure
(table ["Name" "Age"] [["Alice" "30"] ["Bob" "25"]])
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
```clojure
(columns [(layout ["Left Column" "Line 2" "Line 3"])
          (layout ["Right Column" "More text"])])
```
```
Left Column   Right Column
Line 2        More text
Line 3
```

### Key-Value: `kv`
```clojure
(kv [["Name" "Alice"] ["Age" "30"] ["City" "NYC"]])
```
```
Name: Alice
Age:  30
City: NYC
```

### Unordered List: `ul`
```clojure
(ul [(li "First") (li "Second") (li "Third")])
(ul [(li "Item 1" :c [(li "Nested A") (li "Nested B")]) (li "Item 2")])
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
```clojure
(ol [(li "Step one") (li "Step two") (li "Step three")])
```
```
1. Step one
2. Step two
3. Step three
```

### Tree: `tree`
```clojure
(tree (node "project" :c [(node "src") (node "test") (node "README.md")]))
```
```
project
├── src
├── test
└── README.md
```

### Progress Bar: `inline-bar`
```clojure
(inline-bar "Download" 0.75)
```
```
Download [███████████████─────] 75%
```

### Chart: `chart`
```clojure
(chart [["OCaml" 85.0] ["Haskell" 72.0] ["Scala" 90.0]])
```
```
OCaml   │████████████████ 85
Haskell │█████████████ 72
Scala   │█████████████████ 90
```

### Spinner: `spinner`
8 built-in styles:
```clojure
(spinner "Loading" 0 :dots)          ;; ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏
(spinner "Loading" 0 :line)          ;; | / - \
(spinner "Loading" 0 :clock)         ;; 🕐 🕑 🕒 ...
(spinner "Loading" 0 :bounce)        ;; ⠁ ⠂ ⠄ ⠂
(spinner "Loading" 0 :earth)         ;; 🌍 🌎 🌏
(spinner "Loading" 0 :moon)          ;; 🌑 🌒 🌓 🌔 🌕 🌖 🌗 🌘
(spinner "Loading" 0 :grow)          ;; ▏ ▎ ▍ ▌ ▋ ▊ ▉ █
(spinner "Loading" 0 :arrow)         ;; ← ↖ ↑ ↗ → ↘ ↓ ↙
```

### Alignment: `center`, `left-align`, `right-align`, `justify`, `wrap`
```clojure
(center "Auto-centered")             ;; width from siblings
(center 30 "Fixed width")
(left-align 30 "Left")
(right-align 30 "Right")
(justify 30 "Spread this out")
(wrap 20 "Long text that should wrap")
```

### Underline: `underline-elem`, `underline-colored`
```clojure
(underline-elem "Title")
(underline-elem "=" "Double")
(underline-colored "Fancy" "~" bright-cyan)
```
```
Title
─────
Double
======
```

### Margin: `margin`, `margin-color`
```clojure
(margin "[info]" (layout ["Line 1" "Line 2"]))
(margin-color "[error]" red "Something failed")
```
```
[info] Line 1
[info] Line 2
```

### Padding & Truncation: `pad`, `truncate`
```clojure
(pad 2 "Padded content")
(truncate 15 "This is a very long text")
(truncate 20 "Custom ellipsis example" :ellipsis "…")
```

### Spacing: `space`, `empty-elem`
```clojure
(row ["Left" space "Right"])         ;; single space
(row ["Left" (space' 10) "Right"])   ;; 10 spaces
empty-elem                           ;; no-op, conditional rendering
```

## Colors & Styles

Foreground with `color-*`, background with `bg-*`:

```clojure
(-> "Error!" color-red)
(-> "Warning" color-bright-cyan style-bold)
(-> "Alert" bg-red color-white)
```

```clojure
color-black
color-red
color-green
color-yellow
color-blue
color-magenta
color-cyan
color-white
color-bright-black                   ;; Bright 8
color-bright-red
color-bright-green
color-bright-yellow
color-bright-blue
color-bright-magenta
color-bright-cyan
color-bright-white
(color-256 201)                      ;; 256-color palette
(color-rgb 255 128 0)               ;; 24-bit RGB
```

Background variants: `bg-black`, `bg-red`, ... `(bg-256 201)`, `(bg-rgb 30 30 60)`

Raw color values for `margin-color`, `underline-colored`:
```clojure
red, cyan, (rgb 255 128 0)
```

### Styles
```clojure
(-> "text" style-bold)
(-> "text" color-red style-bold)
(-> "text" style-bold style-italic style-underline-s)
```

```
style-bold
style-dim
style-italic
style-underline-s
style-blink
style-reverse
style-hidden
style-strikethrough
```

### Color Gradients

```clojure
;; 256-color palette gradient
(tight-row (map (fn [i] (-> "█" (color-256 (+ 16 (* i 7))))) (range 31)))

;; RGB gradients
(tight-row (map (fn [i]
                  (let [v (* i 8)
                        r (if (< v 128) (* v 2) 255)
                        g (if (< v 128) 255 (* (- 255 v) 2))
                        b (if (> v 128) (* (- v 128) 2) 0)]
                    (-> "█" (color-rgb r g b))))
                (range 32)))
```

## Charts & Plots

### Sparkline
```clojure
(sparkline [1.0 3.0 5.0 7.0 2.0 4.0 8.0 1.0])
```
```
▁▃▅▇▂▄█▁
```

### Line Plot
```clojure
(plot-line 30 10
  [(series [[0 0] [1 1] [2 4] [3 9]] "x^2" bright-cyan)])
```

Multiple series:
```clojure
(let [sin-pts (mapv (fn [i] (let [x (double i)] [x (* (Math/sin (* x 0.15)) 5)])) (range 50))
      cos-pts (mapv (fn [i] (let [x (double i)] [x (* (Math/cos (* x 0.15)) 5)])) (range 50))]
  (plot-line 50 12
    [(series sin-pts "sin(x)" bright-cyan)
     (series cos-pts "cos(x)" bright-magenta)]))
```

### Pie Chart
```clojure
(plot-pie 20 10
  [(slice 40 "OCaml")
   (slice 30 "Haskell")
   (slice 30 "Scala")])
```

### Bar Chart
```clojure
(plot-bar 20 8
  [(bar-item 80 "A" bright-cyan)
   (bar-item 60 "B" bright-green)
   (bar-item 40 "C" bright-magenta)])
```

### Stacked Bar Chart
```clojure
(plot-stacked-bar 20 8
  [(stacked-bar-group "G1"
     [(bar-item 30 "X" bright-cyan)
      (bar-item 20 "Y" bright-green)])
   (stacked-bar-group "G2"
     [(bar-item 20 "X" bright-cyan)
      (bar-item 40 "Y" bright-green)])])
```

### Heatmap
```clojure
(plot-heatmap
  (heatmap-data [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]]
                ["R1" "R2" "R3"]
                ["C1" "C2" "C3"]))

;; Custom cell width
(plot-heatmap 10 (heatmap-data ...))
```

## Interactive Apps

The runtime uses the [Elm Architecture](https://guide.elm-lang.org/architecture/) where your view is simply a layoutz element.

```clojure
{:init          (fn [] [state (cmd-none)])            ;; initial state + startup command
 :update        (fn [msg state] [state (cmd-none)])   ;; pure state transitions
 :subscriptions (fn [state] (sub-none))               ;; event sources
 :view          (fn [state] "Hello")}                 ;; render to UI
```

Three daemon threads coordinate rendering (~30fps), tick/timers, and input capture. State updates flow through `:update` synchronously.

Press **Ctrl-Q** to exit (configurable).

### App Options

```clojure
(run-app app)                                    ;; default options
(run-app app {:alignment :center})               ;; centered in terminal
(run-inline app)                                 ;; animate in-place, no alt screen
```

`run-inline` renders below existing terminal output without clearing. Useful for progress bars in build scripts — use `(cmd-exit)` to quit programmatically.

### Key Types
```clojure
;; Printable
{:type :char :char \a}               ;; or (key-char \a)

;; Editing
{:type :enter}                       ;; key-enter
{:type :backspace}                   ;; key-backspace
{:type :tab}                         ;; key-tab
{:type :escape}                      ;; key-escape
{:type :delete}                      ;; key-delete

;; Navigation
{:type :up}                          ;; key-up
{:type :down}                        ;; key-down
{:type :left}                        ;; key-left
{:type :right}                       ;; key-right
{:type :home}                        ;; key-home
{:type :end}                         ;; key-end
{:type :page-up}                     ;; key-page-up
{:type :page-down}                   ;; key-page-down

;; Modifiers
{:type :ctrl :char \C}               ;; or (key-ctrl \C)
```

### Subscriptions

```clojure
(sub-none)                                       ;; no subscriptions
(sub-key-press (fn [key] ...))                   ;; keyboard input
(sub-every-ms 100 :tick)                         ;; periodic ticks
(sub-batch sub1 sub2 ...)                        ;; combine multiple
```

```clojure
(defn subscriptions [state]
  (sub-batch
    (sub-every-ms 100 :tick)
    (sub-key-press
      (fn [{:keys [type char]}]
        (case type
          :char (case char \q :quit nil)
          nil)))))
```

### Commands

```clojure
(cmd-none)                                       ;; no-op
(cmd-exit)                                       ;; exit the application
(cmd-batch cmd1 cmd2 ...)                        ;; execute multiple
(cmd-task (fn [] (spit "log.txt" "entry") nil))  ;; fire and forget IO
(cmd-task (fn [] (slurp "data.txt")))            ;; IO that returns a message
(cmd-after-ms 500 :delayed-msg)                  ;; one-shot delayed message
```

## Development

```bash
make test       # run tests
make demo       # run demo
make tui-demo   # run interactive TUI demo
make showcase   # run showcase (all elements)
make repl       # start REPL
make fmt        # format code (cljfmt)
make fmt-check  # check formatting
make clean      # clean caches
```
