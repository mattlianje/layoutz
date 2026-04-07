## 0.1.0

Elm-style TUI runtime and terminal visualizations.

- `run_app`, `run_app_final`: interactive apps with the Elm Architecture
  - Key input, periodic ticks, commands (`CmdTask`, `CmdAfterMs`, `CmdExit`)
  - Differential rendering (only redraws changed lines)
  - Raw terminal mode via `tcsetattr`, three daemon threads
- Braille line plots (`plotLine`), pie charts (`plotPie`)
- Vertical bar charts (`plotBar`), stacked bar charts (`plotStackedBar`)
- Heatmaps (`plotHeatmap`) with 256-color gradient
- Sparklines (`sparkline`)
- Spinners: 8 built-in styles (Dots, Line, Clock, Bounce, Earth, Moon, Grow, Arrow)
- `input_handle` helper for text field key handling in TUI apps
- CJK / emoji-aware `display_width` (replaces naive `utf8_length`)
- Additional border styles: `borderAscii`, `borderBlock`, `borderDashed`, `borderDotted`,
  `borderInnerHalfBlock`, `borderOuterHalfBlock`, `borderMarkdown`, `borderCustom`
- `borderNone` to strip borders
- Ordered lists with Roman/letter numbering at depth
- `AutoCenter` elements (auto-center within parent `layout` width)
- `justify`, `justifyAll` text alignment
- `columns` multi-column layout with configurable spacing

## 0.0.2

- Simplified color API: `colorRed`, `colorRGB` etc. apply foreground directly
- Added background color functions: `bgRed`, `bgRGB`, `bg256` etc.
- Raw color values via `Color.red`, `Color.rgb` etc. for `marginColor`/`underlineColored`

## 0.0.1

Initial release.

- Boxes, tables, trees, unordered lists, horizontal/vertical rules
- Row-based layouts with alignment
- UTF-8 box-drawing
- Zero dependencies
