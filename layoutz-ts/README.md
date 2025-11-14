<p align="center">
  <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/layoutz-ts/pix/layoutz-ts-demo.png" width="700">
</p>

# <img src="https://raw.githubusercontent.com/mattlianje/layoutz/refs/heads/master/pix/layoutz.png" width="60"> layoutz

**Simple, beautiful CLI output 🪶**

Declarative and composable text layouts for JavaScript and TypeScript.

## Features

- Zero dependencies
- Fluent, composable API
- Full ANSI color and styling support
- Text styles: bold, italic, underline, and more
- Rich text formatting: alignment, wrapping, justification, padding, truncation
- Lists, trees, tables, charts, boxes, banners, progress bars

## Installation

**Node.js / npm:**

```bash
npm install layoutz
```

```typescript
import * as L from "layoutz";
```

**Browser / CDN:**

```html
<script type="module">
  import { layout, box, text, Color } from 'https://unpkg.com/layoutz@latest/dist/index.esm.js';
  
  const demo = layout(
    box("Browser Demo")(
      text("Works in browser").color(Color.Green),
      text("No dependencies"),
      text("ES modules")
    )
  );
  
  console.log(demo.render());
</script>
```

## Quickstart

```typescript
import * as L from "layoutz";

const demo = L.layout(
  L.underline("ˆ")("Test Dashboard").center(),
  L.row(
    L.statusCard("API", "LIVE").border(L.Border.Double),
    L.statusCard("DB", "99.9%"),
    L.statusCard("Cache", "READY").border(L.Border.Thick)
  ),
  L.box("Services")(
    L.ul("Production", "Staging", L.ul("test-api", L.ul("more nest"))),
    L.inlineBar("Health", 0.94)
  ).border(L.Border.Round)
);

console.log(demo.render());
```

```
            Test Dashboard
            ˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
╔════════╗ ┌─────────┐ ┏━━━━━━━━━┓
║ API    ║ │ DB      │ ┃ Cache   ┃
║ LIVE   ║ │ 99.9%   │ ┃ READY   ┃
╚════════╝ └─────────┘ ┗━━━━━━━━━┛


╭─────────────Services──────────────╮
│ • Production                      │
│ • Staging                         │
│   ◦ test-api                      │
│     ▪ more nest                   │
│                                   │
│                                   │
│ Health [██████████████████──] 94% │
╰───────────────────────────────────╯
```

## Core concepts

- Every piece of content is an `Element`
- Elements are immutable and composable
- `layout` arranges elements vertically with consistent spacing:

```typescript
layout(elem1, elem2, elem3); /* Joins with "\n" */
```

Call `.render()` on an element to get a string.

## API Style

Functional approach with builder methods:

```typescript
/* Compose elements */
const element = box("Title")(
  ul("Item 1", "Item 2"),
  inlineBar("Progress", 0.75)
).border(Border.Double);

/* Chain transformations */
const formatted = text("Hello World").center(20).underline().pad(2);
```

## Elements

All components that implement the Element interface you can use in your layouts...

### Text

Strings are automatically converted to `Text` elements:

```typescript
"Simple text"; /* automatically converted */
text("Simple text"); /* explicit constructor */
```

### Line Break

```typescript
layout("Line 1", "", "Line 2");
```

### Section: `section`

```typescript
section("Config")(kv("env", "prod"));
section("Status", "-")(kv("health", "ok"));
section("Report", "#", 5)(kv("items", "42"));
```

```
=== Config ===
env : prod

--- Status ---
health : ok

##### Report #####
items : 42
```

### Layout (vertical): `layout`

```typescript
layout("First", "Second", "Third");
```

```
First
Second
Third
```

### Row (horizontal): `row`

Horizontal layout with spacing:

```typescript
row("Left", "Middle", "Right");
```

```
Left Middle Right
```

### Tight Row: `tightRow`

Horizontal layout without spacing between elements:

```typescript
tightRow(
  text("█").color(Color.Red),
  text("█").color(Color.Green),
  text("█").color(Color.Blue)
);
```

```
███
```

### Columns: `columns`

```typescript
columns(
  layout("Tasks", ul("Setup", "Code", ul("more stuff"))),
  layout("Status", "foo", "bar", "baz")
);
```

```
Tasks           Status
• Setup         foo
• Code          bar
  ◦ more stuff  baz
```

### Horizontal rule: `hr`

```typescript
hr();
hr().width(10).char("~");
```

```
──────────────────────────────────────────────────
~~~~~~~~~~
```

### Key-value pairs: `kv`

```typescript
kv("name", "Alice", "role", "admin");
// or with tuples:
kv(["name", "Alice"], ["role", "admin"]);
```

```
name : Alice
role : admin
```

### Table: `table`

Tables automatically normalize row lengths:

```typescript
table()(
  ["Name", "Age", "City"],
  [
    ["Alice", "30", "New York"],
    ["Bob", "25"], /* auto-padded */
    ["Charlie", "35", "London", "Extra"], /* auto-truncated */
  ]
);
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

### Ordered Lists: `ol`

```typescript
ol("First step", "Second step", "Third step");
```

```
1. First step
2. Second step
3. Third step
```

Nested numbering:

```typescript
ol(
  "Setup",
  ol("Install tools", "Configure IDE"),
  "Development",
  ol("Write code", ol("Unit tests", "Integration tests")),
  "Deploy"
);
```

```
1. Setup
  a. Install tools
  b. Configure IDE
2. Development
  a. Write code
    i. Unit tests
    ii. Integration tests
3. Deploy
```

### Unordered Lists: `ul`

```typescript
ul("Feature A", "Feature B", "Feature C");
ul("→")("Item 1", "Item 2");
```

```
• Feature A
• Feature B
• Feature C

→ Item 1
→ Item 2
```

Nested lists:

```typescript
ul(
  "Backend",
  ul("API", "Database"),
  "Frontend",
  ul("Components", ul("Header", ul("Footer")))
);
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

### Underline: `underline`

Add underlines to any element

```typescript
underline()("Important Title");
underline("=")("Custom");
```

```
Important Title
───────────────

Custom
══════
```

### Colors: `Color`

Apply ANSI colors to any element:

```typescript
import { Color } from "layoutz";

statusCard("API", "LIVE").color(Color.Green);
box("Error")("Something failed").color(Color.Red);
text("Important").color(Color.BrightMagenta);
```

Standard colors:
- `Black` `Red` `Green` `Yellow` `Blue` `Magenta` `Cyan` `White`
- `BrightBlack` `BrightRed` `BrightGreen` `BrightYellow` `BrightBlue` `BrightMagenta` `BrightCyan` `BrightWhite`
- `NoColor` (for conditional formatting)

256-color palette:

```typescript
import { colorFull } from "layoutz";

text("Orange").color(colorFull(208));
text("Deep blue").color(colorFull(21));
```

True color (RGB):

```typescript
import { colorTrue } from "layoutz";

text("Custom").color(colorTrue(255, 100, 50));
text("█").color(colorTrue(128, 200, 255));
```

Conditional coloring:

```typescript
const isError = true;
statusCard("Status", message).color(isError ? Color.Red : Color.NoColor);
```

Colored underlines:

```typescript
underlineColored("^", Color.BrightMagenta)("Title");
underlineColored("=", colorTrue(255, 0, 255))("RGB underline");
```

### Styles: `Style`

Apply ANSI text styles to any element:

```typescript
import { Style } from "layoutz";

text("Important").style(Style.Bold);
text("Error").color(Color.Red).style(Style.Bold);
```

Combine multiple styles:

```typescript
text("Highlighted").styles(Style.Bold, Style.Reverse);
box("Alert")("Critical").styles(Style.Bold, Style.Italic, Style.Underline);
```

Available styles:

- `Bold` `Dim` `Italic` `Underline`
- `Blink` `Reverse` `Hidden` `Strikethrough`
- `NoStyle` (for conditional formatting)

### Box: `box`

With title:

```typescript
box("Summary")(kv("total", "42"));
```

```
┌──Summary───┐
│ total : 42 │
└────────────┘
```

Without title:

```typescript
box()(kv("total", "42"));
```

```
┌────────────┐
│ total : 42 │
└────────────┘
```

### Status card: `statusCard`

```typescript
statusCard("CPU", "45%");
```

```
┌───────┐
│ CPU   │
│ 45%   │
└───────┘
```

### Progress bar: `inlineBar`

```typescript
inlineBar("Download", 0.75);
```

```
Download [███████████████─────] 75%
```

### Tree: `tree`

```typescript
tree("Project")(
  tree("src")(tree("main")(tree("App.ts")), tree("test")(tree("App.spec.ts")))
);
```

```
Project
└── src/
    ├── main/
    │   └── App.ts
    └── test/
        └── App.spec.ts
```

### Banner: `banner`

```typescript
banner("System Dashboard").border(Border.Double);
```

```
╔═══════════════════╗
║ System Dashboard  ║
╚═══════════════════╝
```

### Chart: `chart`

```typescript
chart(["Web", 10], ["Mobile", 20], ["API", 15]);
```

```
Web            │████████████████ 10.0
Mobile         │████████████████████████████████ 20.0
API            │███████████████████████████ 15.0
```

### Padding: `pad`

```typescript
pad(2)("content");
```

```

  content

```

### Truncation: `truncate`

```typescript
truncate(15)("This is a very long text that will be cut off");
truncate(20, "…")("Custom ellipsis example text here");
```

```
This is a ve...
Custom ellipsis ex…
```

### Empty Element: `empty`

```typescript
layout(
  "Always shown",
  hasError ? margin.error()("Something failed!") : empty(),
  "Also always shown"
);
```

### Vertical Rule: `vr`

```typescript
vr(3); /* 3-line vertical separator */
vr(5, "┃"); /* Custom character */
```

```
│
│
│
```

### Margin: `margin`

```typescript
/* Functional style */
margin("[LOG]")("Error occurred", "Stack trace here");

/* Fluent style */
text("Error occurred").margin("[LOG]");
box("Status")("All systems nominal").margin("[INFO]");

/* Predefined colored margins */
margins.error("Connection failed");
margins.warn("Deprecated API");
margins.success("Build complete");
margins.info("Starting server...");

/* Chain with colors and styles */
box("Deploy")(text("Build successful"))
  .color(Color.Green)
  .style(Style.Bold)
  .margin("[BUILD]");
```

## Text Formatting & Layout

### Alignment: `center`/`leftAlign`/`rightAlign`

```typescript
center("TITLE", 20);
leftAlign("Left side", 20);
rightAlign("Right side", 20);
```

```
        TITLE
Left side
          Right side
```

Multiline text:

```typescript
center("Line 1\nLine 2", 15);
```

```
   Line 1
   Line 2
```

### Text Wrapping: `wrap`

```typescript
wrap("This is a very long line that should be wrapped at word boundaries", 20);
```

```
This is a very long
line that should be
wrapped at word
boundaries
```

### Text Justification: `justify`/`justifyAll`

```typescript
justify("All the lines\nmaybe the last", 20);
justifyAll("All the lines\nmaybe the last", 20);
```

```
All     the    lines
maybe the last

All     the    lines
maybe    the    last
```

### Border Styles

Elements like `box`, `table`, and `banner` support different `Border` options:

Single (default):

```typescript
box("Title")("").border(Border.Single);
/* default style is Border.Single, so same as: box("Title")("") */
```

```
┌─Title─┐
│       │
└───────┘
```

Double:

```typescript
banner("Welcome").border(Border.Double);
```

```
╔═════════╗
║ Welcome ║
╚═════════╝
```

Thick:

```typescript
table().border(Border.Thick)(headers, rows);
```

```
┏━━━━━━━┳━━━━━━━━┓
┃ Name  ┃ Status ┃
┣━━━━━━━╋━━━━━━━━┫
┃ Alice ┃ Online ┃
┗━━━━━━━┻━━━━━━━━┛
```

Round:

```typescript
box("Info")("").border(Border.Round);
```

```
╭─Info─╮
│      │
╰──────╯
```

Custom:

```typescript
box("Hello hello")("World!").border(Border.Custom("+", "=", "|"));
```

```
+==Hello hello==+
| World!        |
+===============+
```

### Custom Components

Create your own components by implementing the `Element` interface:

```typescript
class Square implements Element {
  constructor(private size: number) {}

  render(): string {
    if (this.size < 2) return "";
    const width = this.size * 2 - 2;
    const top = "┌" + "─".repeat(width) + "┐";
    const middle = Array(this.size - 2)
      .fill(null)
      .map(() => "│" + " ".repeat(width) + "│");
    const bottom = "└" + "─".repeat(width) + "┘";
    return [top, ...middle, bottom].join("\n");
  }
}

// Use it like any other element
const demo = row(new Square(5), new Square(3), new Square(7));

console.log(demo.render());
```

## Inspiration

- Original Scala [layoutz](https://github.com/mattlianje/layoutz)
- [ScalaTags](https://github.com/com-lihaoyi/scalatags) by Li Haoyi
