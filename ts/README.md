<p align="center">
  <img src="/pix/layoutz-ts.png" width="700">
</p>

# <img src="../pix/layoutz.png" width="60"> layoutz

**Simple, beautiful CLI output 🪶**

Build declarative and composable sections, trees, tables, dashboards for your Node.js console applications.

## Features
- **Zero dependencies** - Pure TypeScript implementation
- Effortless composition of elements
- Rich text formatting: alignment, wrapping, justification, underlines, padding, truncation
- Lists, trees, tables, charts, progress bars, banners...
- Type-safe, immutable rendering
- Works in both **Node.js** and **browsers**
- **ES modules** and **CommonJS** compatible

## Installation

Install via npm:
```bash
npm install layoutz
```

Or with yarn:
```bash
yarn add layoutz
```

All you need:
```typescript
import * as L from 'layoutz';
// or destructured imports:
import { layout, ul, box, inlineBar } from 'layoutz';
```

Or with CommonJS:
```javascript
const L = require('layoutz');
// or destructured:
const { layout, ul, box, inlineBar } = require('layoutz');
```

## Quickstart

Beautiful, compositional text layouts:

```typescript
import * as L from 'layoutz';

const demo = L.layout(
  L.underline("ˆ")("Test Dashboard").center(),
  L.row(
    L.statusCard("API", "LIVE").border(L.Border.Double),
    L.statusCard("DB", "99.9%"),
    L.statusCard("Cache", "READY").border(L.Border.Thick)
  ),
  "",
  L.box("Services")(
    L.ul("Production", "Staging", L.ul("test-api", L.ul("more nest"))),
    "",
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
- Elements are **immutable** and **composable** - you build complex layouts by combining simple elements
- A `layout` arranges elements **vertically** with consistent spacing:
```typescript
layout(elem1, elem2, elem3)  // Joins with "\n"
```
Call `.render()` on an element to get a string

The power comes from **uniform composition** - since everything is an `Element`, everything can be combined with everything else.

## API Style

TypeScript `layoutz` uses a **functional** approach with builder methods:

```typescript
// Compose elements functionally
const element = box("Title")(
  ul("Item 1", "Item 2"),
  inlineBar("Progress", 0.75)
).border(Border.Double);

// Chain transformations
const formatted = "Hello World"
  .center(20)
  .underline()
  .pad(2);
```

## Elements

All components that implement the Element interface you can use in your layouts...

### Text

TypeScript `layoutz` automatically converts strings to `Text` elements:
```typescript
"Simple text"       // <- automatically converted to Text element
text("Simple text") // <- explicit Text constructor
```

### Line Break

Add extra line-break "\n" with empty string:
```typescript
layout("Line 1", "", "Line 2")
```

### Section: `section`
```typescript
section("Config")(kv("env", "prod"))
section("Status", "-")(kv("health", "ok"))
section("Report", "#", 5)(kv("items", "42"))
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
layout("First", "Second", "Third")
```
```
First
Second
Third
```

### Row (horizontal): `row`
```typescript
row("Left", "Middle", "Right")
```
```
Left Middle Right
```

### Columns: `columns`

```typescript
columns(
  layout("Tasks", ul("Setup", "Code", ul("more stuff"))),
  layout("Status", "foo", "bar", "baz")
)
```
```
Tasks           Status
• Setup         foo
• Code          bar
  ◦ more stuff  baz
```

### Horizontal rule: `hr`
```typescript
hr()
hr().width(10).char("~")
```
```
──────────────────────────────────────────────────
~~~~~~~~~~
```

### Key-value pairs: `kv`
```typescript
kv("name", "Alice", "role", "admin")
// or with tuples:
kv(["name", "Alice"], ["role", "admin"])
```
```
name : Alice
role : admin
```

### Table: `table`
Tables automatically normalize row lengths - truncating long rows and padding short ones:
```typescript
table()(
  ["Name", "Age", "City"],               // headers
  [
    ["Alice", "30", "New York"],
    ["Bob", "25"],                       // Short row - auto-padded
    ["Charlie", "35", "London", "Extra"] // Long row - auto-truncated
  ]
)
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
Automatically numbered lists
```typescript
ol("First step", "Second step", "Third step")
```
```
1. First step
2. Second step
3. Third step
```

Hierarchical nested numbering
```typescript
ol(
  "Setup",
  ol("Install tools", "Configure IDE"),
  "Development",
  ol("Write code", ol("Unit tests", "Integration tests")),
  "Deploy"
)
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
Clean unordered lists with custom bullets
```typescript
ul("Feature A", "Feature B", "Feature C")
ul("→")("Item 1", "Item 2")
```
```
• Feature A
• Feature B
• Feature C

→ Item 1
→ Item 2
```

Nested lists with auto-styling
```typescript
ul(
  "Backend",
  ul("API", "Database"),
  "Frontend",
  ul("Components", ul("Header", ul("Footer")))
)
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
underline()("Important Title")
underline("=")("Custom")
```
```
Important Title
───────────────

Custom
══════
```

### Box: `box`
With title:
```typescript
box("Summary")(kv("total", "42"))
```
```
┌──Summary───┐
│ total : 42 │
└────────────┘
```

Without title:
```typescript
box()(kv("total", "42"))
```
```
┌────────────┐
│ total : 42 │
└────────────┘
```

### Status card: `statusCard`
```typescript
statusCard("CPU", "45%")
```
```
┌───────┐
│ CPU   │
│ 45%   │
└───────┘
```

### Progress bar: `inlineBar`
```typescript
inlineBar("Download", 0.75)
```
```
Download [███████████████─────] 75%
```

### Tree: `tree`
```typescript
tree("Project")(
  tree("src")(
    tree("main")(tree("App.ts")),
    tree("test")(tree("App.spec.ts"))
  )
)
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
banner("System Dashboard").border(Border.Double)
```
```
╔═══════════════════╗
║ System Dashboard  ║
╚═══════════════════╝
```

### Chart: `chart`
```typescript
chart(
  ["Web", 10],
  ["Mobile", 20],
  ["API", 15]
)
```
```
Web            │████████████████ 10.0
Mobile         │████████████████████████████████ 20.0
API            │███████████████████████████ 15.0
```

### Padding: `pad`
Add uniform padding around any element
```typescript
pad(2)("content")
```
```
      
  content  
      
```

### Truncation: `truncate`
Truncate long text with ellipsis
```typescript
truncate(15)("This is a very long text that will be cut off")
truncate(20, "…")("Custom ellipsis example text here")
```
```
This is a ve...
Custom ellipsis ex…
```

### Empty Element: `empty`
Useful for conditional rendering
```typescript
layout(
  "Always shown",
  hasError ? margin.error()("Something failed!") : empty(),
  "Also always shown"
)
```

### Vertical Rule: `vr`
Vertical separators to complement horizontal rules
```typescript
vr(3)           // 3-line vertical separator
vr(5, "┃")      // Custom character
```
```
│
│
│
```

### Margin: `margin`
Use `margin` for nice & colourful "compiler-style" margin strings:

```typescript
layout(
  layout(
    "Ooops",
    "",
    row("const result: number = ", underline("^")("getUserName()")),
    "Expected number, found string"
  ).marginError(),
  "",
  layout(
    "Unused variable detected",
    row("const ", underline("~")("temp"), " = calculateTotal(items)")
  ).marginWarn(),
  "Clean code, cleaner layouts with layoutz",
  layout(
    "Pro tip",
    "",
    row("const ", underline("~")("beauty"), " = renderCode(perfectly)").margin("[layoutz ~>]")
  ).marginInfo()
)
```

## Text Formatting & Layout

### Alignment: `center`/`leftAlign`/`rightAlign`
Align text within a specified width
```typescript
center("TITLE", 20)
leftAlign("Left side", 20)  
rightAlign("Right side", 20)
```
```
        TITLE  
Left side           
          Right side
```

Works with multiline text:
```typescript
center("Line 1\nLine 2", 15)
```
```
   Line 1   
   Line 2   
```

### Text Wrapping: `wrap`
Wrap long text at word boundaries
```typescript
wrap("This is a very long line that should be wrapped at word boundaries", 20)
```
```
This is a very long
line that should be
wrapped at word
boundaries
```

### Text Justification: `justify`/`justifyAll`
Distribute spaces to fit exact width
```typescript
justify("All the lines\nmaybe the last", 20)
justifyAll("All the lines\nmaybe the last", 20)
```
```
All     the    lines
maybe the last

All     the    lines
maybe    the    last
```

### Border Styles
Elements like `box`, `table`, and `banner` support different `Border` options:

**Single** (default):
```typescript
box("Title")("").border(Border.Single)
// default style is Border.Single, so same as: box("Title")("")
```
```
┌─Title─┐
│       │
└───────┘
```

**Double**:
```typescript
banner("Welcome").border(Border.Double)
```
```
╔═════════╗
║ Welcome ║
╚═════════╝
```

**Thick**:
```typescript
table().border(Border.Thick)(headers, rows)
```
```
┏━━━━━━━┳━━━━━━━━┓
┃ Name  ┃ Status ┃
┣━━━━━━━╋━━━━━━━━┫
┃ Alice ┃ Online ┃
┗━━━━━━━┻━━━━━━━━┛
```

**Round**:
```typescript
box("Info")("").border(Border.Round)
```
```
╭─Info─╮
│      │
╰──────╯
```

**Custom**:
```typescript
box("Hello hello")("World!").border(
  Border.Custom("+", "=", "|")
)
```
```
+==Hello hello==+
| World!        |
+===============+
```

## Working with arrays and objects

The full power of JavaScript functional programming is at your fingertips:

```typescript
interface User {
  name: string;
  role: string;
}

const users: User[] = [
  { name: "Alice", role: "Admin" },
  { name: "Bob", role: "User" },
  { name: "Tom", role: "User" }
];

const usersByRole = users.reduce((acc, user) => {
  if (!acc[user.role]) acc[user.role] = [];
  acc[user.role].push(user);
  return acc;
}, {} as Record<string, User[]>);

const report = section("Users by Role")(
  layout(
    ...Object.entries(usersByRole).map(([role, roleUsers]) =>
      box(role)(
        ul(...roleUsers.map(u => u.name))
      )
    )
  )
);
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

## TypeScript Support

Full TypeScript definitions are included:

```typescript
import { Element, Layout, Box, Table } from 'layoutz';

// All elements implement the Element interface
const element: Element = layout("Hello", "World");

// Type-safe border options
const boxElement: Box = box("Title")("Content").border(Border.Double);

// Strongly typed table data
const tableData: string[][] = [
  ["Alice", "Engineer"], 
  ["Bob", "Designer"]
];
const tableElement: Table = table()(["Name", "Role"], tableData);
```

## Browser Usage

Works in browsers too! Import via CDN or bundle with your favorite bundler:

```html
<!-- Via CDN -->
<script type="module">
  import { layout, box, ul } from 'https://unpkg.com/layoutz@latest/dist/index.esm.js';
  
  const demo = layout(
    box("Browser Demo")(
      ul("Works in browser", "No dependencies", "ES modules")
    )
  );
  
  console.log(demo.render());
</script>
```

## Interactive REPL Usage

**Node.js REPL:**
```bash
node
> const L = require('layoutz');
> console.log(L.layout("Hello", L.statusCard("API", "UP")).render())
```

**TypeScript REPL:**
```bash
npx ts-node
> import * as L from 'layoutz';
> console.log(L.layout("Demo", L.box("World")(L.ul("Item 1", "Item 2"))).render())
```

## Inspiration
- Original Scala [layoutz](https://github.com/mattlianje/layoutz)
- [ScalaTags](https://github.com/com-lihaoyi/scalatags) by Li Haoyi
