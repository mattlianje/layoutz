<p align="center">
  <img src="pix/layoutz-demo.png" width="700">
</p>

# <img src="pix/layoutz.png" width="60"> layoutz
**Simple, beautiful CLI output 🪶**

Build declarative and composable sections, trees, tables, dashboards, and interactive Elm-style apps for your consoles. Part of [d4](https://github.com/mattlianje/d4)

## Features
- Zero dependencies, use **Layoutz.scala** like a header-file
- Effortless composition of elements
- Rich text formatting: alignment, wrapping, justification, underlines
- Lists, trees, tables, charts, progress bars, and more
- Thread-safe, purely functional rendering
- Use `LayoutzApp` trait + `LayoutzRuntime` for Elm-style TUI's

## Installation
**layoutz** is on MavenCentral and cross-built for Scala, 2.12, 2.13, 3.x
```scala
"xyz.matthieucourt" %% "layoutz" % "0.1.0"
```
Or try in REPL:
```bash
scala-cli repl --scala 3 --dep xyz.matthieucourt:layoutz_3:0.1.0
```

All you need:
```scala
import layoutz._
```

## Quickstart
```scala
import layoutz._

val build = layout(
  center(underline("DEPLOY PIPELINE"), 30),
  br,
  row(
    statusCard("API", "LIVE", Border.Double),
    statusCard("DB", "99.9%"),
    statusCard("Cache", "READY", Border.Thick)
  ),
  br,
  box("Latest")(
    ul("→")("Fix auth bug", "Add metrics", "Update deps")
  ),
  br,
  inlineBar("Progress", 0.75)
).render
```
```
           Test Dashboard
           ^^^^^^^^^^^^^^

╔════════╗ ┌─────────┐ ┏━━━━━━━━━┓
║ API    ║ │ DB      │ ┃ Cache   ┃
║ LIVE   ║ │ 99.9%   │ ┃ READY   ┃
╚════════╝ └─────────┘ ┗━━━━━━━━━┛

╭─────────────Services──────────────╮
│ • Production                      │
│   → auth-service                  │
│ • Staging                         │
│   ◦ test-api                      │
│     ▪ more nest                   │
│                                   │
│ Health [██████████████████──] 94% │
╰───────────────────────────────────╯
```

## Motivation
- We have `s"..."`, and full-blown TUI libraries - but there is a gap in-between.
- With LLM's, boilerplate code that formats & "pretty-prints" is **_cheaper than ever_**...
- Thus, **_more than ever_**, "string formatting code" is spawning, and polluting domain logic
- Utlimately, **layoutz** is just a tiny, declarative DSL to combat this

## Core concepts
- Every piece of content is an `Element`
- Elements are **immutable** and **composable** - you build complex layouts by combining simple elements.
- A `layout` is just a special element that arranges other elements **vertically** with consistent spacing:
```scala
layout(elem1, elem2, elem3)  /* Joins with "\n" */
```
Call `.render` on an element to get a String

The power comes from **uniform composition**, since everything is an `Element`, everything can be combined with everything else.

## Elements
All components implementing the Element interface you can use in your layouts...

### Text: `Text` *(optional)*
**layoutz** implicitly converts Strings to `Text` elements - the `Text()` wrapper is technically redundant:
```scala
"Simple text" // <- automatically converted to Text element
Text("Simple text") // <- you don't need to do this
```

### Line Break: `br`
Add extra line-break "\n" with `br`:
```scala
layout("Line 1", br, "Line 2")
```

### Section: `section`
```scala
section("Config")(kv("env" -> "prod"))
section("Status", "-")(kv("health" -> "ok"))
section("Report", "#", 5)(kv("items" -> "42"))
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
```scala
layout("First", "Second", "Third")
```
```
First
Second
Third
```

### Row (horizontal): `row`
```scala
row("Left", "Middle", "Right")
```
```
Left Middle Right
```

### Columns: `columns`

```scala
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
```scala
hr
hr("~", 10)
```
```
──────────────────────────────────────────────────
~~~~~~~~~
```

### Key-value pairs: `kv`
```scala
kv("name" -> "Alice", "role" -> "admin")
```
```
name : Alice
role : admin
```

### Table: `table`
```scala
table(
  headers = Seq("Name", "Status"),
  rows = Seq(Seq("Alice", "Online"), Seq("Bob", "Away"))
)
```
```
┌───────┬────────┐
│ Name  │ Status │
├───────┼────────┤
│ Alice │ Online │
│ Bob   │ Away   │
└───────┴────────┘
```



### Ordered Lists: `ol`
Automatically numbered lists
```scala
ol("First step", "Second step", "Third step")
```
```
1. First step
2. Second step
3. Third step
```

Hierarchical nested numbering
```scala
ol("Setup",
  ol("Install tools", "Configure IDE"),
  "Development", 
  ol("Write code", ol("Unit tests", "Integration tests")),
  "Deploy")
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

Mix with other elements
```scala
ol("Initialize project",
  ul("Create repo", "Setup CI/CD"),
  inlineBar("Progress", 0.6),
  "Ready to code!")
```
```
1. Initialize project
2. • Create repo
   • Setup CI/CD
3. Progress [████████████────────] 60%
4. Ready to code!
```

### Unordered Lists: `ul`
Clean unordered lists with custom bullets
```scala
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
```scala
ul("Backend",
  ul("API", "Database"),
  "Frontend", 
  ul("Components", ul("Header", "Footer")))
```
```
• Backend
  ◦ API
  ◦ Database
• Frontend
  ◦ Components
    ▪ Header
    ▪ Footer
```

Mix with other elements
```scala
ul("System Status",
  "CPU: 45%",
  inlineBar("Memory", 0.78),
  statusCard("Health", "OK"))
```
```
• System Status
• CPU: 45%
• Memory [███████████████▌─────] 78%
• ┌─────────┐
  │ Health  │
  │ OK      │
  └─────────┘
```

### Underline: `underline`
Add underlines to any element
```scala
underline("Important Title")
underline("Custom", "=")
```
```
Important Title
───────────────

Custom
══════
```

### Box: `box`
With title:
```scala
box("Summary")(kv("total" -> "42"))
```
```
┌──Summary───┐
│ total : 42 │
└────────────┘
```

Without title:
```scala
box(kv("total" -> "42"))
```
```
┌────────────┐
│ total : 42 │
└────────────┘
```

### Status card: `statusCard`
```scala
statusCard("CPU", "45%")
```
```
┌───────┐
│ CPU   │
│ 45%   │
└───────┘
```

### Progress bar: `inlineBar`
```scala
inlineBar("Download", 0.75)
```
```
Download [███████████████─────] 75%
```

### Spinner: `spinner`
```scala
spinner("Loading...", frame = 3)
spinner("Processing", frame = 0, SpinnerStyle.Line)
```
```
⠸ Loading...
|| Processing
```
Styles: `Dots` (default), `Line`, `Clock`, `Bounce`

### Diff block: `diffBlock`
```scala
diffBlock(
  added = Seq("new feature"),
  removed = Seq("old code")
)
```
```
Changes:
- old code
+ new feature
```

### Tree: `tree`/`branch`/`leaf`
```scala
tree("Project")(
  branch("src",
    branch("main", leaf("App.scala")),
    branch("test", leaf("AppSpec.scala"))
  )
)
```
```
Project
└── src/
    ├── main/
    │   └── App.scala
    └── test/
        └── AppSpec.scala
```

### Banner: `banner`
```scala
banner("System Dashboard", BorderStyle.Double)
```
```
╔═══════════════════╗
║ System Dashboard  ║
╚═══════════════════╝
```

### Chart: `chart`
```scala
chart(
  "Web" -> 10,
  "Mobile" -> 20,
  "API" -> 15
)
```
```
Web            │████████████████ 10.0
Mobile         │████████████████████████████████ 20.0
API            │███████████████████████████ 15.0
```

## Text Formatting & Layout

### Alignment: `center`/`leftAlign`/`rightAlign`
Align text within a specified width
```scala
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
```scala
center("Line 1\nLine 2", 15)
```
```
   Line 1   
   Line 2   
```

### Text Wrapping: `wrap`
Wrap long text at word boundaries
```scala
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
```scala
justify("This text will be justified to fit exactly", 25)
justifyAll("All lines\neven the last", 15)
```
```
This text will be justified to fit exactly

All lines even
even  the  last
```

### Border Styles
Elements like `box`, `table`, and `banner` support different `BorderStyle` options:

**Single** (default):
```scala
box("Title", BorderStyle.Single)("")
```
```
┌─Title─┐
│       │
└───────┘
```

**Double**:
```scala
banner("Welcome", BorderStyle.Double)
```
```
╔═════════╗
║ Welcome ║
╚═════════╝
```

**Thick**:
```scala
table(headers, rows, BorderStyle.Thick)
```
```
┏━━━━━━━┳━━━━━━━━┓
┃ Name  ┃ Status ┃
┣━━━━━━━╋━━━━━━━━┫
┃ Alice ┃ Online ┃
┗━━━━━━━┻━━━━━━━━┛
```

**Round**:
```scala
box("Info", BorderStyle.Round)("")
```
```
╭─Info─╮
│      │
╰──────╯
```

**Custom**:
```scala
box("Alert", BorderStyle.Custom(
  corner = "+", 
  horizontal = "=", 
  vertical = "|"
))("")
```
```
+=Alert=+
|       |
+=======+
```

## Working with collections
The full power of Scala functional collections is at your fingertips to render your strings with **layoutz**
```scala
case class User(name: String, role: String)
val users = Seq(User("Alice", "Admin"), User("Bob", "User"), User("Tom", "User"))

val usersByRole = users.groupBy(_.role)
section("Users by Role")(
  layout(
    usersByRole.map { case (role, roleUsers) =>
      box(role)(
        ul(roleUsers.map(_.name): _*)
      )
    }.toSeq: _*
  )
)
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

## Interactive Apps
Build **Elm-style terminal applications** with the `LayoutzApp` architecture.

### `LayoutzApp[State, Message]`
The type parameters define your app's contract:
- **State**: Your application's state type (primitive like `Int`, case class like `TodoState`, etc.)  
- **Message**: Union type of all possible actions your app can handle (often sealed trait or simple strings)

You implement four methods:
- `init: State` - Initial state when app starts
- `view(state: State): Element` - Render current state to UI elements  
- `onKey(key: Key): Option[Message]` - Convert keyboard input to optional messages
- `update(message: Message, state: State): State` - Apply message to state, return new state

`LayoutzRuntime.run()` handles the event loop, terminal management, and threading automatically.

```scala
object CounterApp extends LayoutzApp[Int, String] {
  def init = 0
  def update(msg: String, count: Int) = msg match {
    case "inc" => count + 1
    case "dec" => count - 1
    case _ => count
  }
  def onKey(k: Key) = k match {
    case CharKey('+') => Some("inc")
    case CharKey('-') => Some("dec") 
    case _ => None
  }
  def view(count: Int) = 
    section("Counter")(s"Count: $count")
}

LayoutzRuntime.run(CounterApp)
```

## Key System
**Key types**: `CharKey(c)`, `EnterKey`, `ArrowUpKey`, `SpecialKey("Ctrl+S")`, auto-generated `SpinnerTickKey`/`ProgressTickKey`

### Input Handling Pattern
```scala
def onKey(k: Key): Option[Message] = k match {
  case CharKey(c) if c.isPrintable => Some(AddChar(c))
  case BackspaceKey => Some(DeleteChar)
  case ArrowLeftKey => Some(MoveLeft)
  case SpecialKey("Ctrl+S") => Some(Save)
  case SpinnerTickKey => Some(UpdateAnimation)
  case _ => None
}
```

## Inspiration
- [ScalaTags](https://github.com/com-lihaoyi/scalatags) by Li Haoyi
- Countless templating libraries via osmosis ...
