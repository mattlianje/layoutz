<p align="center">
  <img src="pix/layoutz-demo.png" width="700">
</p>

# <img src="pix/layoutz.png" width="60"> layout
**Composable layouts for beautiful CLI output 🖋️✨**

Build declarative and composable sections, trees, tables and dashboards for your consoles. Part of [d4](https://github.com/mattlianje/d4)

## Of note...
- With LLM's, boilerplate code that "spaces", "pads", "formats" and "pretty-prints" is **_cheaper than ever_**...
- Which is why, **_more than ever_**, "string formatting code" is spawning, duplicating and becoming muddled with domain logic
- **layout** is just a tiny, declarative DSL to combat this

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

## Core concepts
You just need to know two things

**Element**
- Every piece of content is an `Element`
- Elements are **immutable** and **composable** - you build complex layouts by combining simple elements.

**Layout**
- A `layout` is just a special element that arranges other elements **vertically** with consistent spacing:
```scala
layout(elem1, elem2, elem3)  /* Joins with "\n\n" */
```
The power comes from **uniform composition**, since everything is an `Element`, everything can be combined with everything else.

## Elements
All components implementing the Element interface you can use in your layouts...

### Text
**layout** implicitly convert Strings to `Text` element
```scala
"Simple text" // <- valid Element
```
this lets you splice strings into layouts as you build them with var-arg shorthand

### Line Break
Add extra line-break "\n" with `br`:
```scala
layout("Line 1", br, "Line 2")
```

### Section
```scala
section("Config")(kv("env" -> "prod"))
```
```
=== Config ===
env : prod
```

### Layout (vertical)
```scala
layout("First", "Second", "Third")
```
```
First

Second

Third
```

### Row (horizontal)
```scala
row("Left", "Middle", "Right")
```
```
Left Middle Right
```

### Key-value pairs
```scala
kv("name" -> "Alice", "role" -> "admin")
```
```
name : Alice
role : admin
```

### Table
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

### Bullets
```scala
bullets("Task 1", "Task 2", "Task 3")
```
```
• Task 1
• Task 2
• Task 3
```

### Box
```scala
box("Summary")(kv("total" -> "42"))
```
```
┌──Summary───┐
│ total : 42 │
└────────────┘
```

### Status card
```scala
statusCard("CPU", "45%")
```
```
┌───────┐
│ CPU   │
│ 45%   │
└───────┘
```

### Progress bar
```scala
inlineBar("Download", 0.75)
```
```
Download [███████████████─────] 75%
```

### Diff block
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

### Tree
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
The full power of Scala functional collections is at your fingertips to render your strings with **layout**
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
- [ScalaTags](https://github.com/com-lihaoyi/scalatags) and countless templating libraries via osmosis ...

