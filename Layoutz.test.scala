package layoutz

class LayoutzSpecs extends munit.FunSuite {

  test("basic layout composition") {
    val result = layout(
      section("Config") {
        kv("env" -> "prod", "region" -> "us-east-1")
      }
    )

    val expected = """=== Config ===
env:    prod
region: us-east-1"""

    assertEquals(result.render, expected)
  }

  test("key-value pairs") {
    val kvElement =
      kv("user" -> "alice", "role" -> "admin", "status" -> "active")

    val expected = """user:   alice
role:   admin
status: active"""

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
      ul("Connected to database", "Loaded 28 models", "Cache warmed")

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
        statusCard("Jobs", "✔ 132"),
        statusCard("Errors", "✘ 7"),
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
│ Steps:    5        │
│ Failures: 1        │
│ Time:     6m 32s   │
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
        ul(
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
      val rule1 = hr()
      val rule2 = hr("=")(20)
      val rule3 = hr("*")()

      assertEquals(rule1.render, "─" * 50)
      assertEquals(rule2.render, "=" * 20)
      assertEquals(rule3.render, "*" * 50)
    }

    test("nested bullets") {
      val nestedBullets = ul(
        ul("Backend", ul("API"), ul("Database")),
        ul(
          "Frontend",
          ul("Components", ul("Header"), ul("Footer"))
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
    val simpleText = "Hello"
    assertEquals(simpleText.width, 5)
    assertEquals(simpleText.height, 1)

    val multilineText = "Line 1\nLonger line 2\nShort"
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
          ul("Connected", "Healthy", "Low latency")
        )
      }
    )

    val rendered = result.render
    assert(rendered.contains("=== Database ==="))
    assert(rendered.contains("Host: localhost"))
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
    val emptyBullets = ul()
    assertEquals(emptyBullets.render, "")

    val emptyKv = kv()
    assertEquals(emptyKv.render, "")
  }

  test("line break element") {
    val elements: Seq[Element] = Seq(
      "First line",
      br,
      "Second line",
      br,
      br,
      "Third line"
    )
    val result = elements.map(_.render).mkString

    val expected = """First line
Second line

Third line"""

    assertEquals(result, expected)

    // Test basic properties
    assertEquals(br.render, "\n")
    assertEquals(br.width, 0)
    assertEquals(br.height, 2)

    // Test function call syntax
    val tripleBreak = br(3)
    // br(3) creates Layout(List.fill(3)(LineBreak)) which renders as "\n\n\n\n"
    // br(3) renders as 5 newlines
    assertEquals(tripleBreak.render.length, 5)
    assertEquals(tripleBreak.render, "\n\n\n\n\n")
    assertEquals(tripleBreak.height, 6)

    // Test edge cases
    assertEquals(br(0).render, "")
    assertEquals(br(1).render, "\n")
  }
  test("horizontal rules") {
    val rule1 = hr()
    val rule2 = hr("=")(20)
    val rule3 = hr("*")()

    assertEquals(rule1.render, "─" * 50)
    assertEquals(rule2.render, "=" * 20)
    assertEquals(rule3.render, "*" * 50)
  }

  test("space element") {
    assertEquals(space.render, " ")
    assertEquals(space.width, 1)
    assertEquals(space.height, 1)

    // Test function call syntax
    val fiveSpaces = space(5)
    assertEquals(fiveSpaces.render, "     ")
    assertEquals(fiveSpaces.width, 5)

    // Test edge cases
    assertEquals(space(0).render, "")
    assertEquals(space(1).render, " ")
  }

  test("multiplied elements in layouts") {
    val spaced = layout(
      "First line",
      br(2),
      "After double break",
      space(3),
      "With spaces"
    )

    val rendered = spaced.render
    val lines = rendered.split("\n", -1) // -1 to preserve empty strings

    assertEquals(lines.length, 8) // Each element gets its own line in Layout
    assertEquals(lines(0), "First line")
    assertEquals(lines(1), "")
    assertEquals(lines(2), "")
    assertEquals(lines(3), "")
    assertEquals(lines(4), "")
    assertEquals(lines(5), "After double break")
    assertEquals(lines(6), "   ") // space(3) on its own line
    assertEquals(lines(7), "With spaces")
  }

  test("nested unordered lists") {
    val nestedLists = ul(
      ul("Backend", ul("API"), ul("Database")),
      ul(
        "Frontend",
        ul("Components", ul("Header"), ul("Footer"))
      )
    )

    val expected = """  ◦ Backend
    ▪ API
    ▪ Database
  ◦ Frontend
    ▪ Components
      • Header
      • Footer"""

    assertEquals(nestedLists.render, expected)
  }

  test("Key ADT construction and pattern matching") {
    val charKey = CharKey('a')
    val specialKey = SpecialKey("F1")
    val enterKey = EnterKey
    val escapeKey = EscapeKey

    charKey match {
      case CharKey(c) => assertEquals(c, 'a')
    }

    specialKey match {
      case SpecialKey(name) => assertEquals(name, "F1")
    }

    assertEquals(enterKey, EnterKey)
    assertEquals(escapeKey, EscapeKey)
  }

  test("Key parsing logic (through public interface)") {
    val charA = CharKey('a')
    val charPlus = CharKey('+')
    val charMinus = CharKey('-')

    assertEquals(charA.c, 'a')
    assertEquals(charPlus.c, '+')
    assertEquals(charMinus.c, '-')
  }

  test("LayoutzApp trait basic functionality") {
    // Simple counter app implementation for testing
    object TestCounterApp extends LayoutzApp[Int, String] {
      def init: Int = 0
      def update(msg: String, state: Int): Int = msg match {
        case "inc" => state + 1
        case "dec" => state - 1
        case _     => state
      }
      def onKey(k: Key): Option[String] = k match {
        case CharKey('+') => Some("inc")
        case CharKey('-') => Some("dec")
        case _            => None
      }
      def view(state: Int): Element = layout(
        s"Count: $state",
        "Press +/- to change"
      )
    }

    // Test basic functionality
    assertEquals(TestCounterApp.init, 0)
    assertEquals(TestCounterApp.update("inc", 5), 6)
    assertEquals(TestCounterApp.update("dec", 5), 4)
    assertEquals(TestCounterApp.update("unknown", 5), 5)

    assertEquals(TestCounterApp.onKey(CharKey('+')), Some("inc"))
    assertEquals(TestCounterApp.onKey(CharKey('-')), Some("dec"))
    assertEquals(TestCounterApp.onKey(CharKey('x')), None)

    val view = TestCounterApp.view(42)
    assert(view.render.contains("Count: 42"))
    assert(view.render.contains("Press +/- to change"))
  }

  test("LayoutzApp polymorphism") {
    // Test that LayoutzApp can be used polymorphically
    object SimpleApp extends LayoutzApp[String, Char] {
      def init: String = "hello"
      def update(msg: Char, state: String): String = state + msg
      def onKey(k: Key): Option[Char] = k match {
        case CharKey(c) if c.isLetter => Some(c)
        case _                        => None
      }
      def view(state: String): Element = s"Text: $state"
    }

    val app: LayoutzApp[String, Char] = SimpleApp
    assertEquals(app.init, "hello")
    assertEquals(app.update('!', "test"), "test!")
    assertEquals(app.onKey(CharKey('a')), Some('a'))
    assertEquals(app.onKey(CharKey('1')), None)

    val view = app.view("world")
    assertEquals(view.render, "Text: world")
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
    assert(spinner3.render.contains("Processing"))

    val nextSpinner = spinner1.nextFrame
    assertEquals(nextSpinner.frame, 1)
    assertEquals(nextSpinner.render, "/ Loading")

    val lineSpinner =
      spinner("", SpinnerStyle.Line.frames.length, SpinnerStyle.Line)
    assertEquals(lineSpinner.render, "|")
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

  test("Columns layout") {
    val result = columns(
      "A\nB",
      "1\n2\n3",
      "X"
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
    val result = banner(BannerStyle.Double)("Hello\nWorld")

    val expected = """╔═══════╗
║ Hello ║
║ World ║
╚═══════╝"""

    assertEquals(result.render, expected)
  }

  test("underline element") {
    val simpleText = "Hello"
    val underlined = underline()(simpleText)
    val expected = """Hello
─────"""
    assertEquals(underlined.render, expected)

    val customUnderlined = underline("=")(simpleText)
    val expectedCustom = """Hello
====="""
    assertEquals(customUnderlined.render, expectedCustom)

    val longPattern = underline("─═─═─═─═")(simpleText)
    val expectedTruncated = """Hello
─═─═─"""
    assertEquals(longPattern.render, expectedTruncated)

    val multilineText = "Line 1\nLonger line 2"
    val underlinedMulti = underline()(multilineText)
    val expectedMulti = """Line 1
Longer line 2
─────────────"""
    assertEquals(underlinedMulti.render, expectedMulti)
  }

  test("ordered list") {
    val list = ol(
      "First item",
      "Second item",
      "Third item"
    )

    val expected = """1. First item
2. Second item
3. Third item"""

    assertEquals(list.render, expected)

    val multilineList = ol(
      "First item\nwith continuation",
      "Second item"
    )

    val expectedMultiline = """1. First item
   with continuation
2. Second item"""

    assertEquals(multilineList.render, expectedMultiline)

    val emptyList = ol()
    assertEquals(emptyList.render, "")
  }

  test("nested ordered list") {
    val nestedList = ol(
      "Top level item",
      "Another top item",
      ol(
        "Nested item a",
        "Nested item b",
        ol("Deep nested i", "Deep nested ii")
      ),
      "Back to top level"
    )

    val expected = """1. Top level item
2. Another top item
  a. Nested item a
  b. Nested item b
    i. Deep nested i
    ii. Deep nested ii
3. Back to top level"""

    assertEquals(nestedList.render, expected)
  }

  test("unordered list") {
    val list = ul(
      "First item",
      "Second item",
      "Third item"
    )

    val expected = """• First item
• Second item
• Third item"""

    assertEquals(list.render, expected)

    val customBullet = UnorderedList(Seq("Item 1", "Item 2"), "★")
    val expectedCustom = """★ Item 1
★ Item 2"""

    assertEquals(customBullet.render, expectedCustom)

    val multilineList = ul(
      "First item\nwith continuation",
      "Second item"
    )

    val expectedMultiline = """• First item
  with continuation
• Second item"""

    assertEquals(multilineList.render, expectedMultiline)

    val emptyList = ul()
    assertEquals(emptyList.render, "")
  }

  test("complex list combinations") {
    val complexLayout = layout(
      underline("=")("My Lists"),
      ol(
        "Ordered item 1",
        ul("Nested unordered", "Another nested"),
        "Back to ordered"
      )
    )

    val rendered = complexLayout.render
    assert(rendered.contains("My Lists"))
    assert(rendered.contains("========"))
    assert(rendered.contains("1. Ordered item 1"))
    assert(rendered.contains("• Nested unordered"))
    assert(rendered.contains("3. Back to ordered"))
  }

  test("centered alignment") {
    val centered = center("Hello", 11)
    assertEquals(centered.render, "   Hello   ")

    val centeredOdd = center("Hello", 10)
    assertEquals(
      centeredOdd.render,
      "   Hello  "
    )

    val multiline = center("Line 1\nLine 2", 10)
    assertEquals(multiline.render, "  Line 1  \n  Line 2  ")
  }

  test("left alignment") {
    val leftAligned = leftAlign("Hello", 10)
    assertEquals(leftAligned.render, "Hello     ")

    val multiline = leftAlign("Hi\nBye", 8)
    assertEquals(multiline.render, "Hi      \nBye     ")
  }

  test("right alignment") {
    val rightAligned = rightAlign("Hello", 10)
    assertEquals(rightAligned.render, "     Hello")

    val multiline = rightAlign("Hi\nBye", 8)
    assertEquals(multiline.render, "      Hi\n     Bye")
  }

  test("alignment combinations") {
    val demo = layout(
      center("TITLE", 20),
      leftAlign("Left side", 20),
      rightAlign("Right side", 20)
    )

    val rendered = demo.render
    assertEquals(rendered.contains("        TITLE       "), true)
    assertEquals(rendered.contains("Left side           "), true)
    assertEquals(rendered.contains("          Right side"), true)
  }

  test("alignment with other elements") {
    val boxed = box("Centered Content")(center("Important Message", 15))
    assertEquals(boxed.render.contains("Important Message"), true)
  }

  test("text wrapping") {
    val wrapped = wrap(
      "This is a very long line that should be wrapped at word boundaries",
      20
    )
    val lines = wrapped.render.split('\n')

    assertEquals(lines.length > 1, true)
    lines.foreach(line => assertEquals(line.length <= 20, true))
  }

  test("text wrapping edge cases") {
    assertEquals(wrap("Short", 20).render, "Short")
    assertEquals(
      wrap("supercalifragilisticexpialidocious", 10).render,
      "supercalifragilisticexpialidocious"
    )

    val multiline = wrap("Line one\nLine two has more words than fit", 15)
    assertEquals(multiline.render.split('\n').length > 2, true)
  }

  test("text wrapping with spaces") {
    val wrapped = wrap("Word  with   multiple    spaces", 15)
    assertEquals(wrapped.render.contains("Word  with"), true)
  }

  test("text wrapping in complex layouts") {
    val article = layout(
      center("ARTICLE TITLE", 40),
      hr("=")(40),
      section("Introduction")(
        wrap(
          "This is a long introduction paragraph that needs to be wrapped to fit within a reasonable column width for easy reading.",
          35
        )
      ),
      section("Content")(
        wrap(
          "Here is the main content of the article. It contains many words and should flow nicely when wrapped at word boundaries.",
          35
        )
      )
    )

    val rendered = article.render
    assert(rendered.contains("ARTICLE TITLE"))
    assert(rendered.contains("Introduction"))
    assert(rendered.contains("Content"))

    // Check that lines aren't too long
    val lines = rendered.split('\n')
    val contentLines = lines.filter(line =>
      !line.contains("=") &&
        !line.contains("Article") &&
        !line.contains("Introduction") &&
        !line.contains("Content") &&
        line.trim.nonEmpty
    )

    contentLines.foreach { line =>
      assert(
        line.length <= 50,
        s"Content line too long: '$line' (${line.length} chars)"
      )
    }
  }

  test("text justification") {
    // Basic justification
    val text = "This is a test"
    val justified = justify(text, 20)
    val result = justified.render

    assertEquals(result.length, 20)
    assert(result.startsWith("This"))
    assert(result.endsWith("test"))
    assert(result.contains("  "))

    val shortText = "One Two Three"
    val justifiedShort = justify(shortText, 18)
    val resultShort = justifiedShort.render

    assertEquals(resultShort.length, 18)
    assert(
      resultShort.contains("One") && resultShort.contains("Two") && resultShort
        .contains("Three")
    )
  }

  test("text justification edge cases") {
    val singleWord = "Hello"
    val justifiedSingle = justify(singleWord, 15)
    assertEquals(justifiedSingle.render, "Hello          ")

    val exactWidth = "Exactly twenty chars"
    val justifiedExact = justify(exactWidth, 20)
    assertEquals(justifiedExact.render, "Exactly twenty chars")

    val tooLong = "This text is definitely longer than target"
    val justifiedLong = justify(tooLong, 20)
    val result = justifiedLong.render
    val lines = result.split('\n')

    assert(lines.length >= 2, "Long text should be wrapped into multiple lines")
    lines.dropRight(1).foreach { line =>
      assertEquals(line.length, 20, s"Line '$line' should be exactly 20 chars")
    }

    val empty = ""
    val justifiedEmpty = justify(empty, 10)
    assertEquals(justifiedEmpty.render, "          ")
  }

  test("text justification multiline") {
    val multiline = "First line here\nSecond line\nThird"
    val justified = justify(multiline, 20)
    val result = justified.render
    val lines = result.split('\n')

    assertEquals(lines.length, 3)

    assertEquals(lines(0).length, 20)
    assertEquals(lines(1).length, 20)

    assertEquals(lines(2), "Third")
  }

  test("text justification with justifyAll flag") {
    val multiline = "First line\nLast line"
    val justified = justifyAll(multiline, 15)
    val result = justified.render
    val lines = result.split('\n')

    assertEquals(lines.length, 2)

    assertEquals(lines(0).length, 15)
    assertEquals(lines(1).length, 15)

    assert(lines(0).contains("First") && lines(0).contains("line"))
    assert(lines(1).contains("Last") && lines(1).contains("line"))
  }

  test("justification in complex layouts") {
    val document = layout(
      center("JUSTIFIED DOCUMENT", 40),
      hr("═")(40),
      section("Paragraph 1")(
        justify(
          "This paragraph demonstrates text justification where each line fits snugly within the specified width by distributing spaces between words evenly.",
          35
        )
      ),
      section("Paragraph 2")(
        Layout(
          Seq(
            justify(
              "Another paragraph showing how justification works with multiple lines of text content.",
              35
            ),
            br,
            justify(
              "Each line becomes exactly the target width by adding extra spaces between words.",
              35
            )
          )
        )
      )
    )

    val rendered = document.render
    assert(rendered.contains("JUSTIFIED DOCUMENT"))
    assert(rendered.contains("Paragraph 1"))
    assert(rendered.contains("Paragraph 2"))

    val lines = rendered.split('\n')
    val justifiedLines = lines.filter(line =>
      line.trim.nonEmpty &&
        !line.contains("═") &&
        !line.contains("JUSTIFIED DOCUMENT") &&
        !line.contains("Paragraph") &&
        line.contains(" ") &&
        line.split("\\s+").length > 1
    )

    justifiedLines.foreach { line =>
      if (!line.trim.split("\\s+").exists(_.length > 30)) {
        assert(
          line.length <= 40,
          s"Justified line too long: '$line' (${line.length} chars)"
        )
      }
    }
  }

  test("ansi width calculations") {
    val coloredText = "\u001b[31mRed text\u001b[0m"
    assertEquals(coloredText.width, 8)

    val boxWithColor = box("Status")(coloredText)
    assert(boxWithColor.render.contains("Red text"))
    assert(boxWithColor.width > 8)
  }

  test("basic margin with custom prefix") {
    val simpleMargin = margin("[LOG]")("Hello World")
    assertEquals(simpleMargin.render, "[LOG] Hello World")

    val customPrefix = margin(">>>")("Indented message")
    assertEquals(customPrefix.render, ">>> Indented message")
  }

  test("margin with multiple elements") {
    val multiElementMargin = margin("[INFO]")(
      "Line 1",
      "Line 2",
      "Line 3"
    )

    val expected = """[INFO] Line 1
[INFO] Line 2
[INFO] Line 3"""

    assertEquals(multiElementMargin.render, expected)
  }

  test("margin with single element vs layout") {
    val singleElement = margin("[SINGLE]")("One line")
    assertEquals(singleElement.render, "[SINGLE] One line")

    val layoutElement = margin("[LAYOUT]")(
      layout(
        "First line",
        "Second line"
      )
    )

    val expectedLayout = """[LAYOUT] First line
[LAYOUT] Second line"""

    assertEquals(layoutElement.render, expectedLayout)
  }

  test("status margins - error") {
    val errorMargin = margin.error("Connection failed")
    val rendered = errorMargin.render

    // Red
    assert(rendered.contains("\u001b[31m"))
    assert(rendered.contains("error"))
    assert(rendered.contains("Connection failed"))
    // Reset
    assert(rendered.contains("\u001b[0m"))
  }

  test("status margins - warn") {
    val warnMargin = margin.warn("Performance issue")
    val rendered = warnMargin.render

    assert(rendered.contains("\u001b[33m"))
    assert(rendered.contains("warn"))
    assert(rendered.contains("Performance issue"))
    assert(rendered.contains("\u001b[0m"))
  }

  test("status margins - success") {
    val successMargin = margin.success("All systems operational")
    val rendered = successMargin.render

    assert(rendered.contains("\u001b[32m"))
    assert(rendered.contains("success"))
    assert(rendered.contains("All systems operational"))
    assert(rendered.contains("\u001b[0m"))
  }

  test("status margins - info") {
    val infoMargin = margin.info("System information")
    val rendered = infoMargin.render

    assert(rendered.contains("\u001b[36m"))
    assert(rendered.contains("info"))
    assert(rendered.contains("System information"))
    assert(rendered.contains("\u001b[0m"))
  }

  test("margin with complex nested elements") {
    val complexMargin = margin.error(
      row(
        statusCard("API", "DOWN"),
        statusCard("DB", "SLOW")
      )
    )

    val rendered = complexMargin.render
    assert(rendered.contains("error"))
    assert(rendered.contains("API"))
    assert(rendered.contains("DOWN"))
    assert(rendered.contains("DB"))
    assert(rendered.contains("SLOW"))

    val lines = rendered.split('\n')
    assert(lines.forall(_.contains("error")))
  }

  test("margin preserves element structure") {
    val boxedContent = box("Status")(
      "Service is running",
      "Memory usage: 45%"
    )

    val marginedBox = margin.info(boxedContent)
    val rendered = marginedBox.render

    assert(rendered.contains("┌"))
    assert(rendered.contains("│"))
    assert(rendered.contains("└"))
    assert(rendered.contains("Status"))
    assert(rendered.contains("Service is running"))
    assert(rendered.contains("Memory usage: 45%"))

    val lines = rendered.split('\n')
    assert(lines.forall(_.contains("info")))
  }

  test("user example - nested margins work correctly") {
    val userExample = margin.info(
      row("yo", "man", "what"),
      layout(
        margin.error(
          row(
            statusCard(Border.Double)("API", "LIVE"),
            statusCard("DB", "99.9%"),
            statusCard(Border.Thick)("Cache", "READY")
          )
        )
      )
    )

    val rendered = userExample.render

    assert(rendered.contains("info"))

    assert(rendered.contains("yo"))
    assert(rendered.contains("man"))
    assert(rendered.contains("what"))

    assert(rendered.contains("error"))

    assert(rendered.contains("API"))
    assert(rendered.contains("LIVE"))
    assert(rendered.contains("DB"))
    assert(rendered.contains("99.9%"))
    assert(rendered.contains("Cache"))
    assert(rendered.contains("READY"))

    val lines = rendered.split('\n')
    val infoLines = lines.filter(_.contains("info"))
    val errorLines = lines.filter(_.contains("error"))

    assert(infoLines.nonEmpty, "Should have info prefix lines")
    assert(errorLines.nonEmpty, "Should have error prefix lines")
  }

  test("margin with empty content") {
    val emptyMargin = margin("[EMPTY]")("")
    assertEquals(emptyMargin.render, "[EMPTY] ")
  }

  test("margin ANSI codes don't affect element width calculations") {
    val coloredMargin = margin.error("Short message")
    val plainMargin = margin("[error]")("Short message")

    val coloredElement = coloredMargin
    val plainElement = plainMargin

    assert(coloredElement.width > 0)
    assert(plainElement.width > 0)

    val rendered = coloredMargin.render
    assert(rendered.contains("\u001b[31m"))
    assert(rendered.contains("Short message"))
  }

}
