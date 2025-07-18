<p align="center">
  <img src="pix/layoutz-demo.png" width="700">
</p>

# <img src="pix/layoutz.png" width="60"> layoutz
**Simple, beautiful CLI output 🪶✨**

Build declarative and composable sections, trees, tables and dashboards for your consoles. Part of [d4](https://github.com/mattlianje/d4)

## Features
- Zero dependencies, use Layoutz.scala like a header-file
- Everything renders to `Element` = everything composes
- Immutable, thread-safe, purely functional rendering
- Scala 2.12/2.13/3 compatible


## Quickstart
```scala
import layout._

val dashboard = layout(
  section("System Status")(
    row(
      statusCard("CPU", "45%"),
      statusCard("Memory", "78%"), 
      statusCard("Disk", "23%")
    )
  ),
  box("Recent Activity")(
    bullets(
      "User alice logged in",
      "Database backup completed", 
      "3 new deployments"
    )
  )
)

println(dashboard.render)
```
yields:
```
=== System Status ===
┌───────┐ ┌──────────┐ ┌────────┐
│ CPU   │ │ Memory   │ │ Disk   │
│ 45%   │ │ 78%      │ │ 23%    │
└───────┘ └──────────┘ └────────┘

┌───────Recent Activity───────┐
│ • User alice logged in      │
│ • Database backup completed │
│ • 3 new deployments         │
└─────────────────────────────┘
```

## Of note...
- With LLM's, boilerplate code that formats & "pretty-prints" is **_cheaper than ever_**...
- This is why, **_more than ever_**, "string formatting code" is spawning and polluting domain logic
- **layoutz** is just a tiny, declarative DSL to combat this

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

### Text
**layoutz** implicitly convert Strings to `Text` element
```scala
"Simple text" // <- valid Element
```
this lets you splice strings into layouts as you build them with var-arg shorthand

### Line Break: `br`
Add extra line-break "\n" with `br`:
```scala
layout("Line 1", br, "Line 2")
```

### Section: `section`
```scala
section("Config")(kv("env" -> "prod"))
```
```
=== Config ===
env : prod
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

### Box: `box`
```scala
box("Summary")(kv("total" -> "42"))
```
```
┌──Summary───┐
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

## Inspiration
- [ScalaTags](https://github.com/com-lihaoyi/scalatags) by Mr. Li Haoyi
- Countless templating libraries via osmosis ...
