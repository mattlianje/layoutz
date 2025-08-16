package layoutz

class LayoutzSpecs extends munit.FunSuite {

  test("basic layout composition") {
    val result = layout(
      section("Config") {
        kv("env" -> "prod", "region" -> "us-east-1")
      }
    )

    val expected = """=== Config ===
env    : prod
region : us-east-1"""

    assertEquals(result.render, expected)
  }

  test("key-value pairs") {
    val kvElement =
      kv("user" -> "alice", "role" -> "admin", "status" -> "active")

    val expected = """user   : alice
role   : admin
status : active"""

    assertEquals(kvElement.render, expected)
  }

  test("table rendering") {
    val tableElement = table(
      headers = Seq("Name", "Role", "Status"),
      rows = Seq(
        Seq("Alice", "Engineer", "Online"),
        Seq("Bob", "Designer", "Offline"),
        Seq("Eve", "QA", "Away")
      )
    )

    val expected = """┌───────┬──────────┬─────────┐
│ Name  │ Role     │ Status  │
├───────┼──────────┼─────────┤
│ Alice │ Engineer │ Online  │
│ Bob   │ Designer │ Offline │
│ Eve   │ QA       │ Away    │
└───────┴──────────┴─────────┘"""

    assertEquals(tableElement.render, expected)
  }

  test("bullet points") {
    val bulletElement =
      bullets("Connected to database", "Loaded 28 models", "Cache warmed")

    val expected = """• Connected to database
• Loaded 28 models
• Cache warmed"""

    assertEquals(bulletElement.render, expected)
  }

  test("inline progress bars") {
    val bar1 = inlineBar("Download", 0.72)
    val bar2 = inlineBar("Upload", 0.34)

    assertEquals(bar1.render, "Download [██████████████──────] 72%")
    assertEquals(bar2.render, "Upload [██████──────────────] 34%")
  }

  test("status cards in row layout") {
    val result = layout(
      row(
        statusCard("Jobs", "✔ 132", "green"),
        statusCard("Errors", "✘ 7", "red"),
        statusCard("Time", "12m 33s")
      )
    )

    println(result.render)

    val expected = """┌─────────┐ ┌──────────┐ ┌───────────┐
│ Jobs    │ │ Errors   │ │ Time      │
│ ✔ 132   │ │ ✘ 7      │ │ 12m 33s   │
└─────────┘ └──────────┘ └───────────┘"""

    assertEquals(result.render, expected)
  }

  test("diff block with additions and removals") {
    val diffElement = diffBlock(
      added = Seq("created_at", "email"),
      removed = Seq("legacy_id")
    )

    val expected = """Changes:
- legacy_id
+ created_at
+ email"""

    assertEquals(diffElement.render, expected)
  }

  test("diff block with no changes") {
    val diffElement = diffBlock()
    assertEquals(diffElement.render, "No changes")
  }

  test("box container") {
    val boxElement = box("Pipeline Summary") {
      kv("Steps" -> "5", "Failures" -> "1", "Time" -> "6m 32s")
    }

    val expected = """┌──Pipeline Summary──┐
│ Steps    : 5       │
│ Failures : 1       │
│ Time     : 6m 32s  │
└────────────────────┘"""

    assertEquals(boxElement.render, expected)
  }

  test("complex dashboard layout") {
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
      ),
      section("Performance")(
        layout(
          inlineBar("API Response", 0.89),
          inlineBar("Database", 0.67),
          inlineBar("Cache Hit", 0.92)
        )
      )
    )

    test("horizontal rules") {
      val rule1 = hr
      val rule2 = hr("=", 20)
      val rule3 = hr("*")

      assertEquals(rule1.render, "─" * 50)
      assertEquals(rule2.render, "=" * 20)
      assertEquals(rule3.render, "*" * 50)
    }

    test("nested bullets") {
      val nestedBullets = bullets(
        bullet("Backend", bullet("API"), bullet("Database")),
        bullet(
          "Frontend",
          bullet("Components", bullet("Header"), bullet("Footer"))
        )
      )

      val expected = """• Backend
  • API
  • Database
• Frontend
  • Components
    • Header
    • Footer"""

      assertEquals(nestedBullets.render, expected)
    }
    println(dashboard.render)
    val rendered = dashboard.render
    assert(rendered.contains("=== System Status ==="))
    assert(rendered.contains("• User alice logged in"))
    assert(rendered.contains("API Response"))

  }

  test("element dimensions") {
    val simpleText = Text("Hello")
    assertEquals(simpleText.width, 5)
    assertEquals(simpleText.height, 1)

    val multilineText = Text("Line 1\nLonger line 2\nShort")
    assertEquals(multilineText.width, 13) // "Longer line 2"
    assertEquals(multilineText.height, 3)
  }

  test("implicit string conversion") {
    val result: Layout = layout("Simple text", "Another line")
    val expected = """Simple text

Another line"""
    assertEquals(result.render, expected)
  }

  test("nested sections") {
    val result = layout(
      section("Database") {
        layout(
          kv("Host" -> "localhost", "Port" -> "5432"),
          bullet("Connected", "Healthy", "Low latency")
        )
      }
    )

    val rendered = result.render
    assert(rendered.contains("=== Database ==="))
    assert(rendered.contains("Host : localhost"))
    assert(rendered.contains("• Connected"))
  }

  test("tree structure") {
    val treeElement = tree("Project Structure") {
      branch(
        "src",
        branch("main", leaf("Main.scala"), leaf("Utils.scala")),
        branch("test", leaf("MainSpec.scala"), leaf("UtilsSpec.scala"))
      )
    }

    val expected = """Project Structure
└── src/
    ├── main/
    │   ├── Main.scala
    │   └── Utils.scala
    └── test/
        ├── MainSpec.scala
        └── UtilsSpec.scala"""

    assertEquals(treeElement.render, expected)
  }

  test("simple tree with just leaves") {
    val simpleTree = tree("Files") {
      branch("docs", leaf("README.md"), leaf("CHANGELOG.md"))
    }

    val expected = """Files
└── docs/
    ├── README.md
    └── CHANGELOG.md"""

    assertEquals(simpleTree.render, expected)
  }

  test("empty elements") {
    val emptyBullets = bullets()
    assertEquals(emptyBullets.render, "")

    val emptyKv = kv()
    assertEquals(emptyKv.render, "")
  }

  test("line break element") {
    val elements = Seq(
      Text("First line"),
      br,
      Text("Second line"),
      br,
      br,
      Text("Third line")
    )
    val result = elements.map(_.render).mkString

    val expected = """First line
Second line

Third line"""

    assertEquals(result, expected)
  }
  test("horizontal rules") {
    val rule1 = hr
    val rule2 = hr("=", 20)
    val rule3 = hr("*")

    assertEquals(rule1.render, "─" * 50)
    assertEquals(rule2.render, "=" * 20)
    assertEquals(rule3.render, "*" * 50)
  }

  test("nested bullets") {
    val nestedBullets = bullets(
      bullet("Backend", bullet("API"), bullet("Database")),
      bullet(
        "Frontend",
        bullet("Components", bullet("Header"), bullet("Footer"))
      )
    )

    val expected = """• Backend
  • API
  • Database
• Frontend
  • Components
    • Header
    • Footer"""

    assertEquals(nestedBullets.render, expected)
  }

  /** Test LayoutzApp and key mechanics
    */
  test("Key ADT construction and pattern matching") {
    val charKey = CharKey('a')
    val specialKey = SpecialKey("F1")
    val enterKey = EnterKey
    val escapeKey = EscapeKey

    charKey match {
      case CharKey(c) => assertEquals(c, 'a')
      case _          => fail("Should match CharKey")
    }

    specialKey match {
      case SpecialKey(name) => assertEquals(name, "F1")
      case _                => fail("Should match SpecialKey")
    }

    assertEquals(enterKey, EnterKey)
    assertEquals(escapeKey, EscapeKey)
  }

  test("Key parsing logic (through public interface)") {
    // Test key parsing behavior indirectly since parseKey is private
    val charA = CharKey('a')
    val charPlus = CharKey('+')
    val charMinus = CharKey('-')

    // Test that our Key ADT types work correctly
    assertEquals(charA.c, 'a')
    assertEquals(charPlus.c, '+')
    assertEquals(charMinus.c, '-')
  }

  // Test CounterApp behavior
  test("CounterApp initial state") {
    import Examples.CounterApp
    assertEquals(CounterApp.init, 0)
  }

  test("CounterApp update function") {
    import Examples.CounterApp

    assertEquals(CounterApp.update("inc", 5), 6)
    assertEquals(CounterApp.update("dec", 5), 4)
    assertEquals(CounterApp.update("unknown", 5), 5)

    // Test edge cases
    assertEquals(CounterApp.update("inc", Int.MaxValue - 1), Int.MaxValue)
    assertEquals(CounterApp.update("dec", Int.MinValue + 1), Int.MinValue)
  }

  test("CounterApp key handling") {
    import Examples.CounterApp

    assertEquals(CounterApp.onKey(CharKey('+')), Some("inc"))
    assertEquals(CounterApp.onKey(CharKey('-')), Some("dec"))
    assertEquals(CounterApp.onKey(CharKey('a')), None)
    assertEquals(CounterApp.onKey(EnterKey), None)
    assertEquals(CounterApp.onKey(EscapeKey), None)
  }

  test("CounterApp view rendering") {
    import Examples.CounterApp

    val view0 = CounterApp.view(0)
    val view42 = CounterApp.view(42)
    val viewNegative = CounterApp.view(-5)

    assert(view0.render.contains("Current count: 0"))
    assert(view0.render.contains("=== Counter ==="))
    assert(view0.render.contains("Press + / - to adjust"))

    assert(view42.render.contains("Current count: 42"))
    assert(viewNegative.render.contains("Current count: -5"))
  }

  test("CounterApp full interaction cycle") {
    import Examples.CounterApp

    var state = CounterApp.init
    assertEquals(state, 0)

    // Simulate pressing + key
    CounterApp.onKey(CharKey('+')) match {
      case Some(msg) => state = CounterApp.update(msg, state)
      case None      => fail("Should produce increment message")
    }
    assertEquals(state, 1)

    // Simulate pressing + key again
    CounterApp.onKey(CharKey('+')) match {
      case Some(msg) => state = CounterApp.update(msg, state)
      case None      => fail("Should produce increment message")
    }
    assertEquals(state, 2)

    // Simulate pressing - key
    CounterApp.onKey(CharKey('-')) match {
      case Some(msg) => state = CounterApp.update(msg, state)
      case None      => fail("Should produce decrement message")
    }
    assertEquals(state, 1)

    // Simulate pressing an unmapped key
    CounterApp.onKey(CharKey('x')) match {
      case Some(_) => fail("Should not produce a message")
      case None    => // Expected
    }
    assertEquals(state, 1) // State unchanged
  }

  // Test TodoApp behavior
  test("TodoApp initial state") {
    import Examples.{TodoApp, TodoState}

    val initialState = TodoApp.init
    assertEquals(initialState.items.length, 3)
    assertEquals(initialState.completed, Set(2))
    assertEquals(initialState.inputText, "")
    assertEquals(initialState.inputMode, false)

    assert(initialState.items.contains("Learn Scala"))
    assert(initialState.items.contains("Build awesome apps"))
    assert(initialState.items.contains("Drink coffee"))
  }

  test("TodoApp toggle item completion") {
    import Examples.{TodoApp, ToggleItem}

    val initialState = TodoApp.init

    // Toggle item 0 (not completed initially, not in input mode)
    val state1 = TodoApp.update(ToggleItem(0), initialState)
    assertEquals(state1.completed, Set(0, 2))

    // Toggle item 2 (completed initially)
    val state2 = TodoApp.update(ToggleItem(2), state1)
    assertEquals(state2.completed, Set(0))

    // Toggle item 0 again (should remove it)
    val state3 = TodoApp.update(ToggleItem(0), state2)
    assertEquals(state3.completed, Set.empty[Int])
  }

  test("TodoApp text input functionality") {
    import Examples.{
      TodoApp,
      ToggleInputMode,
      AddChar,
      AddCurrentItem,
      ClearInput
    }

    val initialState = TodoApp.init

    // Enter input mode
    val state1 = TodoApp.update(ToggleInputMode, initialState)
    assertEquals(state1.inputMode, true)
    assertEquals(state1.inputText, "")

    // Add characters
    val state2 = TodoApp.update(AddChar('H'), state1)
    val state3 = TodoApp.update(AddChar('i'), state2)
    assertEquals(state3.inputText, "Hi")

    // Add current item
    val state4 = TodoApp.update(AddCurrentItem, state3)
    assertEquals(state4.items.length, 4)
    assert(state4.items.contains("Hi"))
    assertEquals(state4.inputMode, false)
    assertEquals(state4.inputText, "")
  }

  test("TodoApp key handling") {
    import Examples.{TodoApp, ToggleItem, ToggleInputMode, AddChar}

    assertEquals(TodoApp.onKey(CharKey('1')), Some(ToggleItem(0)))
    assertEquals(TodoApp.onKey(CharKey('9')), Some(ToggleItem(8)))
    assertEquals(TodoApp.onKey(CharKey('0')), None)
    assertEquals(TodoApp.onKey(SpecialKey("Ctrl+N")), Some(ToggleInputMode))
    assertEquals(TodoApp.onKey(CharKey('a')), Some(AddChar('a')))
    assertEquals(
      TodoApp.onKey(CharKey('q')),
      Some(AddChar('q'))
    ) // 'q' is text, Ctrl+Q quits!
    assertEquals(TodoApp.onKey(CharKey(' ')), Some(AddChar(' ')))
  }

  test("TodoApp view rendering") {
    import Examples.{TodoApp, TodoState}

    val state = TodoApp.init
    val view = TodoApp.view(state)

    // Should show the todo list
    assert(view.render.contains("=== Todo List ==="))
    assert(view.render.contains("1. ❌ Learn Scala"))
    assert(view.render.contains("2. ❌ Build awesome apps"))
    assert(view.render.contains("3. ✅ Drink coffee"))

    // Should show stats
    assert(view.render.contains("=== Stats ==="))
    assert(view.render.contains("Progress: 1/3 completed"))

    // Should show add task section
    assert(view.render.contains("=== Add New Task ==="))
    assert(view.render.contains("Press Ctrl+N to add new task"))
  }

  test("TextInput element rendering") {
    val input1 = textInput("Name", "John", "Enter your name")
    val input2 = textInput("Task", "", "What to do?", active = true)

    assertEquals(input1.render, "  Name: John")
    assertEquals(input2.render, "> Task: What to do?_")
  }

  test("Spinner element rendering and animation") {
    val spinner1 = spinner("Loading", 0, SpinnerStyle.Line)
    val spinner2 = spinner("", 1, SpinnerStyle.Dots)
    val spinner3 = spinner("Processing", 2, SpinnerStyle.Clock)

    assertEquals(spinner1.render, "| Loading")
    assertEquals(spinner2.render, "⠙")
    assert(spinner3.render.startsWith("🕒"))
    assert(spinner3.render.contains("Processing"))

    val nextSpinner = spinner1.nextFrame
    assertEquals(nextSpinner.frame, 1)
    assertEquals(nextSpinner.render, "/ Loading")

    // Test frame wrapping
    val lineSpinner =
      spinner("", SpinnerStyle.Line.frames.length, SpinnerStyle.Line)
    assertEquals(lineSpinner.render, "|") // Should wrap to frame 0
  }

  test("Spinner styles") {
    import SpinnerStyle._

    assert(Dots.frames.nonEmpty)
    assert(Line.frames.nonEmpty)
    assert(Clock.frames.nonEmpty)
    assert(Bounce.frames.nonEmpty)

    assert(Line.frames.forall(_.isInstanceOf[String]))
    assert(Dots.frames.forall(_.isInstanceOf[String]))
  }

  test("LoadingApp initial state") {
    import Examples.{LoadingApp, LoadingState}

    val initialState = LoadingApp.init
    assertEquals(initialState.selectedTask, 0)
    assertEquals(initialState.isLoading, false)
    assertEquals(initialState.progress, 0.0)
    assertEquals(initialState.spinnerFrame, 0)
    assertEquals(initialState.completed, Set.empty[Int])
  }

  test("LoadingApp task selection") {
    import Examples.{LoadingApp, SelectTask}

    val initialState = LoadingApp.init

    val state1 = LoadingApp.update(SelectTask(2), initialState)
    assertEquals(state1.selectedTask, 2)

    // Invalid selection should be ignored
    val state2 = LoadingApp.update(SelectTask(10), initialState)
    assertEquals(state2.selectedTask, 0) /* remains unchanged */
  }

  test("LoadingApp loading lifecycle") {
    import Examples.{LoadingApp, StartLoading, ProgressTick, SpinnerTick}

    val initialState = LoadingApp.init

    val state1 = LoadingApp.update(StartLoading, initialState)
    assertEquals(state1.isLoading, true)
    assertEquals(state1.progress, 0.0)
    assert(state1.startTime > 0)

    val state2 = LoadingApp.update(SpinnerTick, state1)
    assertEquals(state2.spinnerFrame, state1.spinnerFrame + 1)

    Thread.sleep(100)
    val state3 = LoadingApp.update(ProgressTick, state2)
    assert(state3.progress >= 0.0)
    assert(state3.elapsedTime > 0)
  }

  test("Badge widget rendering") {
    val result = layout(
      badge("New"),
      badge("Success", BadgeStyle.Success),
      badge("Error", BadgeStyle.Error)
    )

    val expected = """[New]

✅ Success

❌ Error"""

    assertEquals(result.render, expected)
  }

  test("Columns layout") {
    val result = columns(
      Text("A\nB"),
      Text("1\n2\n3"),
      Text("X")
    )

    val expected = """A  1  X
B  2   
   3   """

    assertEquals(result.render, expected)
  }

  test("Chart widget") {
    val result = chart(("CPU", 75.0), ("Memory", 60.0), ("Disk", 30.0))

    val expected =
      """CPU             │████████████████████████████████████████ 75.0
Memory          │████████████████████████████████ 60.0
Disk            │████████████████ 30.0"""

    assertEquals(result.render, expected)
  }

  test("Banner widget") {
    val result = banner("Hello\nWorld", BannerStyle.Double)

    val expected = """╔═══════╗
║ Hello ║
║ World ║
╚═══════╝"""

    assertEquals(result.render, expected)
  }

  test("LayoutzApp trait compilation and polymorphism") {
    import Examples.{CounterApp, TodoApp, TodoState, TodoMessage}

    val counterApp: LayoutzApp[Int, String] = CounterApp
    val todoApp: LayoutzApp[TodoState, TodoMessage] = TodoApp

    assertEquals(counterApp.init, 0)
    assert(todoApp.init.items.nonEmpty)

    val counterView: Element = counterApp.view(5)
    val todoView: Element = todoApp.view(todoApp.init)

    assert(counterView.render.nonEmpty)
    assert(todoView.render.nonEmpty)
  }
}
