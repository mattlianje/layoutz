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

val dashboard = layout(
  center(underline(Text("📊 SYSTEM DASHBOARD")), 50),
  
  section("System Status")(
    row(
      statusCard("CPU", "45%"),
      statusCard("Memory", "78%"), 
      statusCard("Disk", "23%")
    )
  ),
  
  row(
    box("Recent Activity")(
      ul("User alice logged in", "Database backup completed", "3 new deployments")
    ),
    box("Next Steps")(
      ol("Review logs", "Update configs", "Deploy v2.1")
    )
  )
).render
```
```
               📊 SYSTEM DASHBOARD               
               ────────────────────               

=== System Status ===
┌───────┐ ┌──────────┐ ┌────────┐
│ CPU   │ │ Memory   │ │ Disk   │
│ 45%   │ │ 78%      │ │ 23%    │
└───────┘ └──────────┘ └────────┘

┌───────Recent Activity───────┐ ┌────────Next Steps────────┐
│ • User alice logged in      │ │ 1. Review logs            │
│ • Database backup completed │ │ 2. Update configs         │
│ • 3 new deployments         │ │ 3. Deploy v2.1            │
└─────────────────────────────┘ └───────────────────────────┘
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
layout(elem1, elem2, elem3)  /* Joins with "\n\n" */
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

### Bullets: `bullets`/`bullet`
Simple bullet list
```scala
bullets("Task 1", "Task 2", "Task 3")
```
```
• Task 1
• Task 2
• Task 3
```
Single bullet with nested children
```scala
bullet("Backend", 
  bullet("API"),
  bullet("Database")
)
```
```
• Backend
  • API
  • Database
```
Complex nesting
```scala
bullets(
  bullet("Frontend",
    bullet("Components",
      bullet("Header"),
      bullet("Footer")
    ),
    bullet("Styles")
  ),
  bullet("Backend",
    bullet("API"),
    bullet("Database")
  )
)
```
```
• Frontend
  • Components
    • Header
    • Footer
  • Styles
• Backend
  • API
  • Database
```
Mix bullets with other elements
```scala
bullet("Status",
  "System online",
  inlineBar("Health", 0.95),
  "All services running"
)
```
```
• Status
  • System online
  • Health [███████████████████─] 95%
  • All services running
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
With nested elements
```scala
ol(
  "Setup project",
  ul("Install dependencies", "Configure settings"),
  "Run tests"
)
```
```
1. Setup project
2. • Install dependencies
   • Configure settings
3. Run tests
```

### Unordered Lists: `ul`
Clean unordered lists with custom bullets
```scala
ul("Feature A", "Feature B", "Feature C")
ul("Item 1", "Item 2", bullet = "→")
```
```
• Feature A
• Feature B
• Feature C

→ Item 1
→ Item 2
```

### Underline: `underline`
Add underlines to any element
```scala
underline(Text("Important Title"))
underline(Text("Custom"), "=")
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
center(Text("TITLE"), 20)
leftAlign(Text("Left side"), 20)
rightAlign(Text("Right side"), 20)
```
```
       TITLE        
Left side           
          Right side
```

Works with multiline text:
```scala
center(Text("Line 1\nLine 2"), 15)
```
```
   Line 1   
   Line 2   
```

### Text Wrapping: `wrap`
Wrap long text at word boundaries
```scala
wrap(Text("This is a very long line that should be wrapped at word boundaries"), 20)
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
justify(Text("This text will be justified to fit exactly"), 25)
justifyAll(Text("All lines\neven the last"), 15)
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
        bullets(roleUsers.map(_.name): _*)
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
    section("Counter")(Text(s"Count: $count"))
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
