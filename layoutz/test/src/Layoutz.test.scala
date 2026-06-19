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

    assert(result.render == expected)
  }

  test("key-value pairs") {
    val kvElement =
      kv("user" -> "alice", "role" -> "admin", "status" -> "active")

    val expected = """user:   alice
role:   admin
status: active"""

    assert(kvElement.render == expected)
  }

  test("key-value pairs with CJK characters") {
    val kvElement =
      kv("Kazushi" -> "Sakuraba", "Jet 李連杰" -> "Li", "Rory" -> "MacDonald")

    val rendered = kvElement.render

    assert(rendered.contains("Kazushi:"))
    assert(rendered.contains("Jet 李連杰:"))
    assert(rendered.contains("Rory:"))

    assert(rendered.contains("Sakuraba"))
    assert(rendered.contains("Li"))
    assert(rendered.contains("MacDonald"))
  }

  test("code point width: ascii, cjk, combining, emoji, astral") {
    assert("hello".width == 5)

    assert("李連杰".width == 6)
    assert("你好".width == 4)
    assert("ＡＢ".width == 4)

    assert("é".width == 1)

    assert("😀".width == 2)
    assert("🎉".width == 2)

    assert("𝐀".width == 1)
    assert("𝄀".width == 1)

    assert("a😀李".width == 5)

    assert("[31m😀[0m".width == 2)
  }

  private def bytes(s: String): Array[Byte] = s.map(_.toByte).toArray

  test("kitty base64 encodes RFC 4648 vectors") {
    import KittyProtocol.base64
    assert(base64(bytes("")) == "")
    assert(base64(bytes("f")) == "Zg==")
    assert(base64(bytes("fo")) == "Zm8=")
    assert(base64(bytes("foo")) == "Zm9v")
    assert(base64(bytes("foob")) == "Zm9vYg==")
    assert(base64(bytes("fooba")) == "Zm9vYmE=")
    assert(base64(bytes("foobar")) == "Zm9vYmFy")
  }

  test("kitty image measures exactly cols x rows") {
    val img = kittyImage(bytes("PNGDATA"), cols = 4, rows = 2)
    assertEquals(img.width, 4)
    assertEquals(img.height, 2)

    val tall = kittyImage(bytes("x"), cols = 10, rows = 7)
    assertEquals(tall.width, 10)
    assertEquals(tall.height, 7)
  }

  test("kitty image composes inside containers like a plain block") {
    val img = kittyImage(bytes("PNGDATA"), cols = 6, rows = 3)
    val plain = Text(Seq.fill(3)("######").mkString("\n"))

    assertEquals(box("img")(img).width, box("img")(plain).width)
    assertEquals(box("img")(img).height, box("img")(plain).height)
    assertEquals(row(img, img).width, row(plain, plain).width)
  }

  test("kitty transmit block adds no visible width or height") {
    val img = kittyImage(bytes("PNGDATA"), cols = 4, rows = 2)
    val t = KittyProtocol.transmit(img)
    assertEquals(realLength(t), 0)
    assert(!t.contains("\n"))
  }

  test("kitty placement is rows lines each cols cells wide") {
    val img = kittyImage(bytes("PNGDATA"), cols = 5, rows = 3)
    val lines = KittyProtocol.placement(img).split('\n')
    assertEquals(lines.length, 3)
    lines.foreach(l => assertEquals(realLength(l), 5))
  }

  test("kitty payload chunks at 4096 base64 chars") {
    val big = kittyImage(new Array[Byte](3 * 4096 + 99), cols = 2, rows = 2)
    val t = KittyProtocol.transmit(big)
    val chunkCount = "_G".r.findAllIn(t).length
    assert(chunkCount >= 2, s"expected multiple chunks, got $chunkCount")
    assert(t.contains("m=1"))
    assert(t.contains("m=0"))
  }

  test("kitty image id is a deterministic non-zero 24-bit content hash") {
    val a1 = kittyImage(bytes("alpha"), 2, 2).id
    val a2 = kittyImage(bytes("alpha"), 8, 4).id
    val b = kittyImage(bytes("bravo"), 2, 2).id
    assertEquals(a1, a2)
    assertNotEquals(a1, b)
    assert(a1 > 0 && a1 <= 0xffffff)

    assertEquals(
      kittyImage(bytes("alpha"), 2, 2).copy(idOverride = Some(0x123456)).id,
      0x123456
    )
  }

  test("kitty raw RGB/RGBA require pixel dimensions") {
    intercept[IllegalArgumentException](
      kittyRGB(bytes("x"), pxW = 0, pxH = 4, cols = 2, rows = 2)
    )
    val ok = kittyRGBA(bytes("x"), pxW = 8, pxH = 8, cols = 2, rows = 2)
    assert(KittyProtocol.transmit(ok).contains("s=8,v=8"))
    assert(KittyProtocol.transmit(ok).contains("f=32"))
  }

  test("kitty image rejects out-of-range footprints") {
    intercept[IllegalArgumentException](kittyImage(bytes("x"), cols = 0, rows = 2))
    intercept[IllegalArgumentException](kittyImage(bytes("x"), cols = 2, rows = 999))
  }

  test("kitty multi-chunk transmit puts image id on every chunk") {
    val big = kittyImage(new Array[Byte](3 * 4096 + 99), cols = 2, rows = 2)
    val t = KittyProtocol.transmit(big)
    val headers = "_G[^;]*;".r.findAllIn(t).toList
    assert(headers.length >= 2, s"expected >=2 chunks, got ${headers.length}")
    val tag = s"i=${big.id}"
    headers.foreach(h => assert(h.contains(tag), s"chunk header missing $tag: $h"))
  }

  test("kitty dedupe drops transmits whose id is already cached") {
    val img = kittyImage(bytes("PNGDATA"), cols = 4, rows = 2)
    val rendered = img.render
    val cache = scala.collection.mutable.Set.empty[Int]

    val first = KittyProtocol.dedupeTransmits(rendered, cache)
    assert(first.contains("_G"), "first pass should keep the transmit")
    assert(cache.contains(img.id), "first pass should record the id")

    val second = KittyProtocol.dedupeTransmits(rendered, cache)
    assert(!second.contains("_G"), "second pass should drop the transmit")
    val firstLine = second.split('\n')(0)
    assertEquals(realLength(firstLine), 4)
    assertEquals(second.count(_ == '\n'), 1)
  }

  test("kitty dedupe keeps blocks for new ids and adds them to the cache") {
    val a = kittyImage(bytes("alpha"), 2, 2)
    val b = kittyImage(bytes("bravo"), 2, 2)
    val cache = scala.collection.mutable.Set(a.id)
    val mixed = a.render + b.render

    val out = KittyProtocol.dedupeTransmits(mixed, cache)
    val blockCount = "_G".r.findAllIn(out).length
    assertEquals(blockCount, 1, "only the unseen image's transmit should remain")
    assert(out.contains(s"i=${b.id}"))
    assert(cache.contains(a.id) && cache.contains(b.id))
  }

  test("kitty dedupe drops every chunk of a multi-chunk transmit") {
    val big = kittyImage(new Array[Byte](3 * 4096 + 99), cols = 2, rows = 2)
    val cache = scala.collection.mutable.Set(big.id)
    val out = KittyProtocol.dedupeTransmits(big.render, cache)
    assert(!out.contains("_G"), "no APC graphics block should survive")
  }

  test("kitty dedupe leaves non-graphics content untouched") {
    val s = "hello[31mworld[0m\n[38;2;1;2;3m󯻮x[0m"
    val cache = scala.collection.mutable.Set.empty[Int]
    assertEquals(KittyProtocol.dedupeTransmits(s, cache), s)
    assert(cache.isEmpty)
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

    assert(tableElement.render == expected)
  }

  test("table with new border styles") {
    val t = table(Seq("A", "B"), Seq(Seq("1", "2")))

    val ascii = t.border(Border.Ascii)
    assert(ascii.render.contains("+"))
    assert(ascii.render.contains("-"))
    assert(ascii.render.contains("|"))

    val dashed = t.border(Border.Dashed)
    assert(dashed.render.contains("╌"))

    val dotted = t.border(Border.Dotted)
    assert(dotted.render.contains("┈"))

    val block = t.border(Border.Block)
    assert(block.render.contains("█"))

    val markdown = t.border(Border.Markdown)
    assert(markdown.render.contains("|"))
    assert(markdown.render.contains("-"))

    val inner = t.border(Border.InnerHalfBlock)
    assert(inner.render.contains("▗"))
    assert(inner.render.contains("▘"))

    val outer = t.border(Border.OuterHalfBlock)
    assert(outer.render.contains("▛"))
    assert(outer.render.contains("▟"))
  }

  test("half-block border asymmetry") {
    val inner = box()("test").border(Border.InnerHalfBlock)
    val rendered = inner.render
    val lines = rendered.split('\n')
    assert(lines.head.contains("▄"))
    assert(lines.last.contains("▀"))
    val contentLine = lines(1)
    assert(contentLine.startsWith("▐"))
    assert(contentLine.endsWith("▌"))

    val outer = box()("test").border(Border.OuterHalfBlock)
    val rendered2 = outer.render
    val lines2 = rendered2.split('\n')
    assert(lines2.head.contains("▀"))
    assert(lines2.last.contains("▄"))
    val contentLine2 = lines2(1)
    assert(contentLine2.startsWith("▌"))
    assert(contentLine2.endsWith("▐"))
  }

  test("table row normalization") {
    val mixedTable = table(
      headers = Seq("Name", "Age", "City"),
      rows = Seq(
        Seq("Alice", "30", "New York"),
        Seq("Bob", "25"),
        Seq("Charlie", "35", "London", "Extra"),
        Seq("Diana"),
        Seq()
      )
    )

    val rendered = mixedTable.render
    val lines = rendered.split('\n')
    val dataRows = lines
      .drop(2)
      .dropRight(1)
      .filter(line => line.contains("│") && !line.contains("─"))

    dataRows.foreach { row =>
      assert(row.count(_ == '│') == 4)
    }

    assert(rendered.contains("Alice") && rendered.contains("Bob"))
    assert(rendered.contains("Charlie") && rendered.contains("Diana"))
    assert(!rendered.contains("Extra"))
  }

  test("bullet points") {
    val bulletElement =
      ul("Connected to database", "Loaded 28 models", "Cache warmed")

    val expected = """• Connected to database
• Loaded 28 models
• Cache warmed"""

    assert(bulletElement.render == expected)
  }

  test("inline progress bars") {
    val bar1 = inlineBar("Download", 0.72)
    val bar2 = inlineBar("Upload", 0.34)

    assert(bar1.render == "Download [██████████████──────] 72%")
    assert(bar2.render == "Upload [██████──────────────] 34%")
  }

  test("status cards in row layout") {
    val result = layout(
      row(
        statusCard("Jobs", "✔ 132"),
        statusCard("Errors", "✘ 7"),
        statusCard("Time", "12m 33s")
      )
    )

    val expected = """┌─────────┐ ┌──────────┐ ┌───────────┐
│ Jobs    │ │ Errors   │ │ Time      │
│ ✔ 132   │ │ ✘ 7      │ │ 12m 33s   │
└─────────┘ └──────────┘ └───────────┘"""

    assert(result.render == expected)
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

    assert(boxElement.render == expected)
  }

  test("box direct syntax") {
    val boxElement = box()(kv("total" -> "42"))

    val expected = """┌───────────┐
│ total: 42 │
└───────────┘"""

    assert(boxElement.render == expected)
  }

  test("box with string content") {
    val boxElement: Box = box()("heyya")

    val expected = """┌───────┐
│ heyya │
└───────┘"""

    assert(boxElement.render == expected)
  }

  test("box border styles") {
    val singleBox = box()("Single border")
    val doubleBox = box()("Double border").border(Border.Double)
    val thickBox = box()("Thick border").border(Border.Thick)
    val roundBox = box()("Round border").border(Border.Round)
    val customBox = box()("Custom border").border(Border.Custom("*", "=", "|"))

    val asciiBox = box()("hi").border(Border.Ascii)
    assert(asciiBox.render.contains("+----+"))
    assert(asciiBox.render.contains("| hi |"))

    val blockBox = box()("hi").border(Border.Block)
    assert(blockBox.render.contains("██████"))
    assert(blockBox.render.contains("█ hi █"))

    val dashedBox = box()("hi").border(Border.Dashed)
    assert(dashedBox.render.contains("╌"))
    assert(dashedBox.render.contains("╎"))

    val dottedBox = box()("hi").border(Border.Dotted)
    assert(dottedBox.render.contains("┈"))
    assert(dottedBox.render.contains("┊"))

    val markdownBox = box()("hi").border(Border.Markdown)
    assert(markdownBox.render.contains("|----"))
    assert(markdownBox.render.contains("| hi |"))

    val innerBox = box()("hi").border(Border.InnerHalfBlock)
    assert(innerBox.render.contains("▗"))
    assert(innerBox.render.contains("▖"))
    assert(innerBox.render.contains("▝"))
    assert(innerBox.render.contains("▘"))
    assert(innerBox.render.contains("▄"))
    assert(innerBox.render.contains("▀"))
    assert(innerBox.render.contains("▐"))
    assert(innerBox.render.contains("▌"))

    val outerBox = box()("hi").border(Border.OuterHalfBlock)
    assert(outerBox.render.contains("▛"))
    assert(outerBox.render.contains("▜"))
    assert(outerBox.render.contains("▙"))
    assert(outerBox.render.contains("▟"))
    assert(outerBox.render.contains("▀"))
    assert(outerBox.render.contains("▄"))
    assert(outerBox.render.contains("▌"))
    assert(outerBox.render.contains("▐"))
  }

  test("HasBorder typeclass") {
    import HasBorder._

    // Works with all bordered elements using typeclass method
    val myBox = box()("content").border(Border.Double)
    val myTable = table(Seq("A", "B"), Seq(Seq("1", "2"))).border(Border.Thick)
    val myCard = statusCard("Status", "OK").border(Border.Round)
    val myBanner = banner("Alert").border(Border.Single)

    // Method syntax
    val box2 = box()("test").border(Border.Double)
    val table2 = table(Seq("X"), Seq(Seq("Y"))).border(Border.None)

    // Generic function that works with any bordered element
    def makeThick[T: HasBorder](element: T): T =
      element.border(Border.Thick)

    val thickBox = makeThick(box()("content"))
    val thickTable = makeThick(table(Seq("X"), Seq(Seq("Y"))))

    assert(thickBox.borderStyle == Border.Thick)
    assert(thickTable.borderStyle == Border.Thick)
    assert(myBox.borderStyle == Border.Double)
    assert(box2.borderStyle == Border.Double)
    assert(table2.borderStyle == Border.None)
  }

  test("padding element") {
    val padded = pad(2)("content")
    val rendered = padded.render
    val lines = rendered.split('\n')

    // Should have 2 empty lines before, content line, 2 empty lines after
    assert(lines.length == 5)
    assert(lines(2) == "  content  ")
  }

  test("truncate element") {
    val long = "This is a very long text that should be truncated"
    val truncated = truncate(20)(long)

    val result = truncated.render
    assert(result.length == 20)
    assert(result.endsWith("..."))
  }

  test("empty element") {
    assert(empty.render == "")
    assert(empty.width == 0)
    assert(empty.height == 1)

    // Useful for conditional rendering
    val conditional: Element = if (false) "Hidden" else empty
    assert(conditional.render == "")
  }

  test("vertical separator") {
    val vSep = vr(3)
    val expected = "│\n│\n│"
    assert(vSep.render == expected)

    val customVSep = vr(2, "┃")
    assert(customVSep.render == "┃\n┃")
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

    val rendered = dashboard.render
    assert(rendered.contains("=== System Status ==="))
    assert(rendered.contains("• User alice logged in"))
    assert(rendered.contains("API Response"))
  }

  test("horizontal rules") {
    // Fluent API
    val rule1 = hr
    val rule2 = hr.width(20).char("=")
    val rule3 = hr.char("*")

    assert(rule1.render == "─" * 50) // DEFAULT_RULE_WIDTH
    assert(rule2.render == "=" * 20)
    assert(rule3.render == "*" * 50)

    // Test method chaining in different orders
    assert(hr.char("█").width(5).render == "█████")
    assert(hr.width(3).char("•").render == "•••")
  }

  test("nested bullets") {
    val nestedBullets = ul(
      "Backend",
      ul("API", "Database"),
      "Frontend",
      ul("Components", ul("Header", "Footer"))
    )

    val expected = """• Backend
  ◦ API
  ◦ Database
• Frontend
  ◦ Components
    ▪ Header
    ▪ Footer"""

    assert(nestedBullets.render == expected)
  }

  test("element dimensions") {
    val simpleText = "Hello"
    assert(simpleText.width == 5)
    assert(simpleText.height == 1)

    val multilineText = "Line 1\nLonger line 2\nShort"
    assert(multilineText.width == 13) // "Longer line 2"
    assert(multilineText.height == 3)
  }

  test("implicit string conversion") {
    val result: Layout = layout("Simple text", "Another line")
    val expected = """Simple text
Another line"""
    assert(result.render == expected)
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
    val treeElement = tree("Project Structure")(
      tree("src")(
        tree("main")(tree("Main.scala"), tree("Utils.scala")),
        tree("test")(tree("MainSpec.scala"), tree("UtilsSpec.scala"))
      )
    )

    val expected = """Project Structure
└── src/
    ├── main/
    │   ├── Main.scala
    │   └── Utils.scala
    └── test/
        ├── MainSpec.scala
        └── UtilsSpec.scala"""

    assert(treeElement.render == expected)
  }

  test("simple tree with just leaves") {
    val simpleTree = tree("Files")(
      tree("docs")(tree("README.md"), tree("CHANGELOG.md"))
    )

    val expected = """Files
└── docs/
    ├── README.md
    └── CHANGELOG.md"""

    assert(simpleTree.render == expected)
  }

  test("consistent tree syntax") {
    val projectTree = tree("Project")(
      tree("src")(
        tree("main")(tree("App.scala")),
        tree("test")(tree("AppSpec.scala"))
      )
    )

    val expected = """Project
└── src/
    ├── main/
    │   └── App.scala
    └── test/
        └── AppSpec.scala"""

    assert(projectTree.render == expected)
  }

  test("tree leaf without parentheses") {
    val leaf = tree("simple-file.txt")

    val expected = "simple-file.txt"

    assert(leaf.render == expected)
  }

  test("empty elements") {
    val emptyBullets = ul()
    assert(emptyBullets.render == "")

    val emptyKv = kv()
    assert(emptyKv.render == "")
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

    assert(result == expected)

    // Test basic properties
    assert(br.render == "\n")
    assert(br.width == 0)
    assert(br.height == 2)

    // Test function call syntax
    val tripleBreak = br(3)
    // br(3) creates Layout(List.fill(3)(LineBreak)) which renders as "\n\n\n\n"
    // br(3) renders as 5 newlines
    assert(tripleBreak.render.length == 5)
    assert(tripleBreak.render == "\n\n\n\n\n")
    assert(tripleBreak.height == 6)

    // Test edge cases
    assert(br(0).render == "")
    assert(br(1).render == "\n")
  }

  test("hr fluent API") {
    assert(hr.render == "─" * 50)
    assert(hr.width(15).render == "─" * 15)
    assert(hr.char("═").render == "═" * 50)
    assert(hr.width(8).char("━").render == "━━━━━━━━")
    assert(hr.char("*").width(5).render == "*****")
  }

  test("space element") {
    assert(space.render == " ")
    assert(space.width == 1)
    assert(space.height == 1)

    // Test function call syntax
    val fiveSpaces = space(5)
    assert(fiveSpaces.render == "     ")
    assert(fiveSpaces.width == 5)

    // Test edge cases
    assert(space(0).render == "")
    assert(space(1).render == " ")
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

    assert(lines.length == 8) // Each element gets its own line in Layout
    assert(lines(0) == "First line")
    assert(lines(1) == "")
    assert(lines(2) == "")
    assert(lines(3) == "")
    assert(lines(4) == "")
    assert(lines(5) == "After double break")
    assert(lines(6) == "   ") // space(3) on its own line
    assert(lines(7) == "With spaces")
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

    assert(nestedLists.render == expected)
  }

  test("TextInput element rendering") {
    val input1 = textInput("Name", "John", "Enter your name")
    val input2 = textInput("Task", "", "What to do?", active = true)

    assert(input1.render == "  Name: John")
    assert(input2.render == "> Task: What to do?█")
  }

  test("Spinner element rendering and animation") {
    val spinner1 = spinner("Loading", 0, SpinnerStyle.Line)
    val spinner2 = spinner("", 1, SpinnerStyle.Dots)
    val spinner3 = spinner("Processing", 2, SpinnerStyle.Clock)

    assert(spinner1.render == "| Loading")
    assert(spinner2.render == "⠙")
    assert(spinner3.render.contains("Processing"))

    val nextSpinner = spinner1.nextFrame
    assert(nextSpinner.frame == 1)
    assert(nextSpinner.render == "/ Loading")

    val lineSpinner =
      spinner("", SpinnerStyle.Line.frames.length, SpinnerStyle.Line)
    assert(lineSpinner.render == "|")
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

    assert(result.render == expected)
  }

  test("Chart widget") {
    val result = chart(("CPU", 75.0), ("Memory", 60.0), ("Disk", 30.0))
    val rendered = result.render

    assert(rendered.contains("CPU"))
    assert(rendered.contains("Memory"))
    assert(rendered.contains("Disk"))
    assert(rendered.contains("█"))
    assert(rendered.contains("│"))
    assert(rendered.contains("75"))
    assert(rendered.contains("60"))
    assert(rendered.contains("30"))
  }

  test("Banner widget") {
    val result = banner("Hello\nWorld").border(BannerStyle.Double)

    val expected = """╔═══════╗
║ Hello ║
║ World ║
╚═══════╝"""

    assert(result.render == expected)
  }

  test("underline element") {
    val simpleText = "Hello"
    val underlined = underline()(simpleText)
    val expected = """Hello
─────"""
    assert(underlined.render == expected)

    val customUnderlined = underline("=")(simpleText)
    val expectedCustom = """Hello
====="""
    assert(customUnderlined.render == expectedCustom)

    val longPattern = underline("─═─═─═─═")(simpleText)
    val expectedTruncated = """Hello
─═─═─"""
    assert(longPattern.render == expectedTruncated)

    val multilineText = "Line 1\nLonger line 2"
    val underlinedMulti = underline()(multilineText)
    val expectedMulti = """Line 1
Longer line 2
─────────────"""
    assert(underlinedMulti.render == expectedMulti)
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

    assert(list.render == expected)

    val multilineList = ol(
      "First item\nwith continuation",
      "Second item"
    )

    val expectedMultiline = """1. First item
   with continuation
2. Second item"""

    assert(multilineList.render == expectedMultiline)

    val emptyList = ol()
    assert(emptyList.render == "")
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

    assert(nestedList.render == expected)
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

    assert(list.render == expected)

    val customBullet = UnorderedList(Seq("Item 1", "Item 2"), "★")
    val expectedCustom = """★ Item 1
★ Item 2"""

    assert(customBullet.render == expectedCustom)

    val multilineList = ul(
      "First item\nwith continuation",
      "Second item"
    )

    val expectedMultiline = """• First item
  with continuation
• Second item"""

    assert(multilineList.render == expectedMultiline)

    val emptyList = ul()
    assert(emptyList.render == "")
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
    assert(centered.render == "   Hello   ")

    val centeredOdd = center("Hello", 10)
    assert(centeredOdd.render == "   Hello  ")

    val multiline = center("Line 1\nLine 2", 10)
    assert(multiline.render == "  Line 1  \n  Line 2  ")
  }

  test("left alignment") {
    val leftAligned = leftAlign("Hello", 10)
    assert(leftAligned.render == "Hello     ")

    val multiline = leftAlign("Hi\nBye", 8)
    assert(multiline.render == "Hi      \nBye     ")
  }

  test("right alignment") {
    val rightAligned = rightAlign("Hello", 10)
    assert(rightAligned.render == "     Hello")

    val multiline = rightAlign("Hi\nBye", 8)
    assert(multiline.render == "      Hi\n     Bye")
  }

  test("alignment combinations") {
    val demo = layout(
      center("TITLE", 20),
      leftAlign("Left side", 20),
      rightAlign("Right side", 20)
    )

    val rendered = demo.render
    assert(rendered.contains("        TITLE       ") == true)
    assert(rendered.contains("Left side           ") == true)
    assert(rendered.contains("          Right side") == true)
  }

  test("alignment with other elements") {
    val boxed = box("Centered Content")(center("Important Message", 15))
    assert(boxed.render.contains("Important Message") == true)
  }

  test("text wrapping") {
    val wrapped = wrap(
      "This is a very long line that should be wrapped at word boundaries",
      20
    )
    val lines = wrapped.render.split('\n')

    assert(lines.length > 1 == true)
    lines.foreach(line => assert(line.length <= 20 == true))
  }

  test("text wrapping edge cases") {
    assert(wrap("Short", 20).render == "Short")
    assert(
      wrap("supercalifragilisticexpialidocious", 10).render == "supercalifragilisticexpialidocious"
    )

    val multiline = wrap("Line one\nLine two has more words than fit", 15)
    assert(multiline.render.split('\n').length > 2 == true)
  }

  test("text wrapping with spaces") {
    val wrapped = wrap("Word  with   multiple    spaces", 15)
    assert(wrapped.render.contains("Word  with") == true)
  }

  test("text wrapping in complex layouts") {
    val article = layout(
      center("ARTICLE TITLE", 40),
      hr.width(40).char("="),
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
      assert(line.length <= 50)
    }
  }

  test("text justification") {
    // Basic justification
    val text = "This is a test"
    val justified = justify(text, 20)
    val result = justified.render

    assert(result.length == 20)
    assert(result.startsWith("This"))
    assert(result.endsWith("test"))
    assert(result.contains("  "))

    val shortText = "One Two Three"
    val justifiedShort = justify(shortText, 18)
    val resultShort = justifiedShort.render

    assert(resultShort.length == 18)
    assert(
      resultShort.contains("One") && resultShort.contains("Two") && resultShort
        .contains("Three")
    )
  }

  test("text justification edge cases") {
    val singleWord = "Hello"
    val justifiedSingle = justify(singleWord, 15)
    assert(justifiedSingle.render == "Hello          ")

    val exactWidth = "Exactly twenty chars"
    val justifiedExact = justify(exactWidth, 20)
    assert(justifiedExact.render == "Exactly twenty chars")

    val tooLong = "This text is definitely longer than target"
    val justifiedLong = justify(tooLong, 20)
    val result = justifiedLong.render
    val lines = result.split('\n')

    assert(lines.length >= 2)
    lines.dropRight(1).foreach { line =>
      assert(line.length == 20)
    }

    val empty = ""
    val justifiedEmpty = justify(empty, 10)
    assert(justifiedEmpty.render == "          ")
  }

  test("text justification multiline") {
    val multiline = "First line here\nSecond line\nThird"
    val justified = justify(multiline, 20)
    val result = justified.render
    val lines = result.split('\n')

    assert(lines.length == 3)

    assert(lines(0).length == 20)
    assert(lines(1).length == 20)

    assert(lines(2) == "Third")
  }

  test("text justification with justifyAll flag") {
    val multiline = "First line\nLast line"
    val justified = justifyAll(multiline, 15)
    val result = justified.render
    val lines = result.split('\n')

    assert(lines.length == 2)

    assert(lines(0).length == 15)
    assert(lines(1).length == 15)

    assert(lines(0).contains("First") && lines(0).contains("line"))
    assert(lines(1).contains("Last") && lines(1).contains("line"))
  }

  test("justification in complex layouts") {
    val document = layout(
      center("JUSTIFIED DOCUMENT", 40),
      hr.width(40).char("═"),
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
        assert(line.length <= 40)
      }
    }
  }

  test("ansi width calculations") {
    val coloredText = "\u001b[31mRed text\u001b[0m"
    assert(coloredText.width == 8)

    val boxWithColor = box("Status")(coloredText)
    assert(boxWithColor.render.contains("Red text"))
    assert(boxWithColor.width > 8)
  }

  test("basic margin with custom prefix") {
    val simpleMargin = margin("[LOG]")("Hello World")
    assert(simpleMargin.render == "[LOG] Hello World")

    val customPrefix = margin(">>>")("Indented message")
    assert(customPrefix.render == ">>> Indented message")
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

    assert(multiElementMargin.render == expected)
  }

  test("margin with single element vs layout") {
    val singleElement = margin("[SINGLE]")("One line")
    assert(singleElement.render == "[SINGLE] One line")

    val layoutElement = margin("[LAYOUT]")(
      layout(
        "First line",
        "Second line"
      )
    )

    val expectedLayout = """[LAYOUT] First line
[LAYOUT] Second line"""

    assert(layoutElement.render == expectedLayout)
  }

  test("margin with complex nested elements") {
    val complexMargin = margin("[STATUS]")(
      row(
        statusCard("API", "DOWN"),
        statusCard("DB", "SLOW")
      )
    )

    val rendered = complexMargin.render
    assert(rendered.contains("[STATUS]"))
    assert(rendered.contains("API"))
    assert(rendered.contains("DOWN"))
    assert(rendered.contains("DB"))
    assert(rendered.contains("SLOW"))

    val lines = rendered.split('\n')
    assert(lines.forall(_.contains("[STATUS]")))
  }

  test("margin preserves element structure") {
    val boxedContent = box("Status")(
      "Service is running",
      "Memory usage: 45%"
    )

    val marginedBox = margin("[INFO]")(boxedContent)
    val rendered = marginedBox.render

    assert(rendered.contains("┌"))
    assert(rendered.contains("│"))
    assert(rendered.contains("└"))
    assert(rendered.contains("Status"))
    assert(rendered.contains("Service is running"))
    assert(rendered.contains("Memory usage: 45%"))

    val lines = rendered.split('\n')
    assert(lines.forall(_.contains("[INFO]")))
  }

  test("user example - nested margins work correctly") {
    val userExample = margin("[INFO]")(
      row("yo", "man", "what"),
      layout(
        margin("[ERROR]")(
          row(
            statusCard("API", "LIVE").border(Border.Double),
            statusCard("DB", "99.9%"),
            statusCard("Cache", "READY").border(Border.Thick)
          )
        )
      )
    )

    val rendered = userExample.render

    assert(rendered.contains("[INFO]"))

    assert(rendered.contains("yo"))
    assert(rendered.contains("man"))
    assert(rendered.contains("what"))

    assert(rendered.contains("[ERROR]"))

    assert(rendered.contains("API"))
    assert(rendered.contains("LIVE"))
    assert(rendered.contains("DB"))
    assert(rendered.contains("99.9%"))
    assert(rendered.contains("Cache"))
    assert(rendered.contains("READY"))

    val lines = rendered.split('\n')
    val infoLines = lines.filter(_.contains("[INFO]"))
    val errorLines = lines.filter(_.contains("[ERROR]"))

    assert(infoLines.nonEmpty)
    assert(errorLines.nonEmpty)
  }

  test("margin with empty content") {
    val emptyMargin = margin("[EMPTY]")("")
    assert(emptyMargin.render == "[EMPTY] ")
  }

  test("margin with different prefixes") {
    val plainMargin = margin("[error]")("Short message")

    val plainElement = plainMargin

    assert(plainElement.width > 0)

    val rendered = plainMargin.render
    assert(rendered.contains("[error]"))
    assert(rendered.contains("Short message"))
  }

  test("fluent transformations") {
    assert("Hello".center(10).render.length == 10)
    assert("Hello".pad(1).render.contains(" Hello "))
    assert("Long text here".wrap(8).render.split('\n').length > 1)
    assert("Hello World".truncate(8).render == "Hello...")
    assert("Hello".underline().render.contains("─"))
    assert("Hello".underline("=").render.contains("="))
    assert("Hello".center(10).pad(1).render.contains("Hello"))
  }

  test("transformations on complex elements") {
    assert(statusCard("API", "UP").center(30).render.contains("API"))
    assert(
      table(Seq("Name"), Seq(Seq("Alice"))).pad(1).render.contains("Alice")
    )
  }

  test("margin transformations") {
    assert("Done".margin("[LOG]").render == "[LOG] Done")
  }

  test("colors") {
    assert(Color.Red("Error").render.contains("\u001b[31m"))
    assert("Success".color(Color.Green).render.contains("\u001b[32m"))
    assert("Plain".color(Color.NoColor).render == "Plain")

    Seq(
      Color.Black,
      Color.Red,
      Color.Green,
      Color.Yellow,
      Color.Blue,
      Color.Magenta,
      Color.Cyan,
      Color.White,
      Color.BrightBlack,
      Color.BrightRed,
      Color.BrightGreen,
      Color.BrightYellow,
      Color.BrightBlue,
      Color.BrightMagenta,
      Color.BrightCyan,
      Color.BrightWhite
    ).foreach { c =>
      assert(c("test").render.contains("\u001b["))
    }
  }

  test("colored underlines") {
    val rendered = underlineColored("=", Color.Red)("Error").render
    assert(rendered.contains("\u001b[31m"))
    assert(rendered.contains("="))

    assert(
      "Info"
        .underlineColored("─", Color.BrightCyan)
        .render
        .contains("\u001b[96m")
    )
  }

  test("colored margins") {
    val single = marginColor("[ERROR]", Color.Red)("Oops").render
    assert(single.contains("\u001b[31m"))
    assert(single.contains("[ERROR]"))

    val multi =
      marginColor("[INFO]", Color.BrightCyan)(layout("L1", "L2", "L3")).render
    multi
      .split('\n')
      .foreach(line =>
        assert(line.contains("[INFO]") && line.contains("\u001b[96m"))
      )
  }

  test("styles") {
    assert("Bold".style(Style.Bold).render.contains("\u001b[1m"))
    assert("Italic".style(Style.Italic).render.contains("\u001b[3m"))
    assert("Plain".style(Style.NoStyle).render == "Plain")

    Seq(
      Style.Bold,
      Style.Dim,
      Style.Italic,
      Style.Underline,
      Style.Blink,
      Style.Reverse,
      Style.Hidden,
      Style.Strikethrough
    ).foreach { s =>
      assert("text".style(s).render.contains("\u001b["))
    }
  }

  test("color and style combinations") {
    val combined = "Error!".color(Color.Red).style(Style.Bold).render
    assert(combined.contains("\u001b[31m"))
    assert(combined.contains("\u001b[1m"))

    val multi = "Notice".style(Style.Bold).style(Style.Italic).render
    assert(multi.contains("\u001b[1m"))
    assert(multi.contains("\u001b[3m"))
  }

  test("style concatenation with ++") {
    val concat = "Text".style(Style.Bold ++ Style.Italic).render
    assert(concat.contains("\u001b[1m"))
    assert(concat.contains("\u001b[3m"))

    val triple =
      "More".style(Style.Bold ++ Style.Italic ++ Style.Underline).render
    assert(triple.contains("\u001b[1m"))
    assert(triple.contains("\u001b[3m"))
    assert(triple.contains("\u001b[4m"))
  }

  test("extended colors") {
    val full = "Test".color(Color.Full(196)).render
    assert(full.contains("38;5;196"))

    val trueColor = "RGB".color(Color.True(255, 128, 0)).render
    assert(trueColor.contains("38;2;255;128;0"))
  }

  test("tightRow") {
    val tight = tightRow("A".color(Color.Red), "B".color(Color.Green), "C")
    assert(tight.render == "\u001b[31mA\u001b[0m\u001b[32mB\u001b[0mC")
  }

  test("border after style") {
    val t = table(Seq("A", "B"), Seq(Seq("1", "2")))
    val styled = t.style(Style.Bold).border(Border.Thick)
    assert(styled.render.contains("┏"))
    assert(styled.render.contains("\u001b[1m"))
  }

  test("plot renders braille characters") {
    val points = (0 to 10).map(x => (x.toDouble, x.toDouble))
    val p = plot(20, 5)(Series(points))
    val rendered = p.render
    assert(rendered.exists(c => c >= '\u2800' && c <= '\u28FF'))
    assert(rendered.contains("┤"))
    assert(rendered.contains("└"))
  }

  test("plot with sine wave") {
    val sinPoints = (0 to 360 by 10).map(x => (x.toDouble, math.sin(x.toDouble.toRadians)))
    val p = plot(30, 8)(Series(sinPoints))
    val rendered = p.render
    assert(rendered.contains("1"))
    assert(rendered.contains("-1"))
    assert(rendered.contains("0"))
    assert(rendered.contains("360"))
  }

  test("multi-series plot with legend") {
    val sin =
      Series((0 to 90 by 10).map(x => (x.toDouble, math.sin(x.toDouble.toRadians))), "sin")
    val cos =
      Series((0 to 90 by 10).map(x => (x.toDouble, math.cos(x.toDouble.toRadians))), "cos")
    val p = plot(30, 6)(sin, cos)
    val rendered = p.render
    assert(rendered.exists(c => c >= '\u2800' && c <= '\u28FF'))
    assert(rendered.contains("sin"))
    assert(rendered.contains("cos"))
  }

  test("plot without axes") {
    val points = Seq((0.0, 0.0), (1.0, 1.0), (2.0, 0.5))
    val p = plot(10, 5, showAxes = false)(Series(points))
    val rendered = p.render
    assert(!rendered.contains("┤"))
    assert(!rendered.contains("└"))
    assert(rendered.exists(c => c >= '\u2800' && c <= '\u28FF'))
  }

  test("plot empty data") {
    val p = plot(10, 5)()
    assert(p.render == "No data")
  }

  test("plot with colored series") {
    val sin = Series((0 to 90 by 1).map(x => (x.toDouble, math.sin(x.toDouble.toRadians))), "sin")
      .color(Color.Red)
    val cos = Series((0 to 90 by 1).map(x => (x.toDouble, math.cos(x.toDouble.toRadians))), "cos")
      .color(Color.Blue)
    val p = plot(30, 6)(sin, cos)
    val rendered = p.render
    assert(rendered.contains("\u001b[31m"))
    assert(rendered.contains("\u001b[34m"))
    assert(rendered.contains("sin"))
    assert(rendered.contains("cos"))
  }

  test("pie chart renders braille") {
    val p = pie(20, 10)(Slice(30, "A"), Slice(50, "B"), Slice(20, "C"))
    val rendered = p.render
    assert(rendered.exists(c => c >= '\u2800' && c <= '\u28FF'))
    assert(rendered.contains("A"))
    assert(rendered.contains("B"))
    assert(rendered.contains("C"))
  }

  test("pie chart with custom colors") {
    val p = pie(20, 10)(
      Slice(50, "Red").color(Color.Red),
      Slice(50, "Blue").color(Color.Blue)
    )
    val rendered = p.render
    assert(rendered.contains("\u001b[31m"))
    assert(rendered.contains("\u001b[34m"))
  }

  test("pie chart empty data") {
    val p = pie(10, 5)()
    assert(p.render == "No data")
  }

  test("bar chart renders blocks") {
    val b = bar(20, 5)(
      Bar(10, "A"),
      Bar(20, "B"),
      Bar(15, "C")
    )
    val rendered = b.render
    assert(rendered.contains("█"))
    assert(rendered.contains("A"))
    assert(rendered.contains("B"))
    assert(rendered.contains("C"))
  }

  test("bar chart with custom colors") {
    val b = bar(20, 5)(
      Bar(10, "X").color(Color.Red),
      Bar(20, "Y").color(Color.Green)
    )
    val rendered = b.render
    assert(rendered.contains("\u001b[31m"))
    assert(rendered.contains("\u001b[32m"))
  }

  test("bar chart empty data") {
    val b = bar(10, 5)()
    assert(b.render == "No data")
  }

  test("stacked bar chart") {
    val s = stackedBar(30, 8)(
      StackedBar(Seq(Bar(5, "Q1"), Bar(3, "Q2")), "2023"),
      StackedBar(Seq(Bar(7, "Q1"), Bar(4, "Q2")), "2024")
    )
    val rendered = s.render
    assert(rendered.contains("█"))
    assert(rendered.contains("2023"))
    assert(rendered.contains("2024"))
    assert(rendered.contains("Q1"))
    assert(rendered.contains("Q2"))
  }

  test("stacked bar chart empty data") {
    val s = stackedBar(10, 5)()
    assert(s.render == "No data")
  }

  test("sparkline renders") {
    val s = sparkline(Seq(1.0, 3.0, 2.0, 5.0, 4.0))
    val rendered = s.render
    assert(rendered.nonEmpty)
    assert(rendered.length == 5)
  }

  test("sparkline empty") {
    val s = sparkline(Seq.empty)
    assert(s.render == "")
  }

  test("sparkline single value") {
    val s = sparkline(Seq(5.0))
    assert(s.render.length == 1)
  }

  test("histogram renders") {
    val h = histogram(Seq(1.0, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0), bins = 5)
    val rendered = h.render
    assert(rendered.contains("█"))
    assert(rendered.contains("─"))
  }

  test("histogram empty") {
    val h = histogram(Seq.empty, bins = 5)
    assert(h.render == "No data")
  }

  test("histogram with color") {
    val h = histogram(Seq(1.0, 2.0, 3.0), bins = 3).color(Color.Green)
    val rendered = h.render
    assert(rendered.nonEmpty)
  }

  test("heatmap renders") {
    val h = heatmap(Seq(
      Seq(1.0, 2.0, 3.0),
      Seq(4.0, 5.0, 6.0),
      Seq(7.0, 8.0, 9.0)
    ))
    val rendered = h.render
    assert(rendered.contains("\u001b[48;5;"))
    assert(rendered.contains("1.0"))
    assert(rendered.contains("9.0"))
  }

  test("heatmap empty") {
    val h = heatmap(Seq.empty)
    assert(h.render == "No data")
  }

  test("heatmap with labels") {
    val data = HeatmapData(
      rows = Seq(Seq(1.0, 2.0), Seq(3.0, 4.0)),
      rowLabels = Seq("A", "B"),
      colLabels = Seq("X", "Y")
    )
    val h = heatmap(data)
    val rendered = h.render
    assert(rendered.contains("A"))
    assert(rendered.contains("B"))
    assert(rendered.contains("X"))
    assert(rendered.contains("Y"))
  }

  test("boxplot renders") {
    val bp = boxPlot()(
      BoxData("A", 1.0, 2.0, 3.0, 4.0, 5.0),
      BoxData("B", 2.0, 3.0, 4.0, 5.0, 6.0)
    )
    val rendered = bp.render
    assert(rendered.contains("A"))
    assert(rendered.contains("B"))
    assert(rendered.contains("─") || rendered.contains("┼") || rendered.contains("│"))
  }

  test("boxplot empty") {
    val bp = boxPlot()()
    assert(bp.render == "No data")
  }

  test("boxplot with color") {
    val bp = boxPlot()(
      BoxData("Test", 0.0, 1.0, 2.0, 3.0, 4.0).color(Color.Cyan)
    )
    val rendered = bp.render
    assert(rendered.contains("Test"))
  }

  test("background color bgCode derivation") {
    assert(Color.Red.bgCode == "41")
    assert(Color.Green.bgCode == "42")
    assert(Color.Blue.bgCode == "44")
    assert(Color.Black.bgCode == "40")
    assert(Color.White.bgCode == "47")
    assert(Color.BrightRed.bgCode == "101")
    assert(Color.BrightGreen.bgCode == "102")
    assert(Color.BrightBlue.bgCode == "104")
    assert(Color.NoColor.bgCode == "")
    assert(Color.Full(196).bgCode == "48;5;196")
    assert(Color.True(255, 128, 0).bgCode == "48;2;255;128;0")
  }

  test("background color via element .colorBg()") {
    val rendered = "Error".colorBg(Color.Red).render
    assert(rendered.contains("\u001b[41m"))
    assert(rendered.contains("Error"))
    assert(rendered.contains("\u001b[0m"))
  }

  test("background color via Color.bg()") {
    val rendered = Color.Red.bg("Error").render
    assert(rendered.contains("\u001b[41m"))
    assert(rendered.contains("Error"))
  }

  test("background color NoColor passthrough") {
    assert("Plain".colorBg(Color.NoColor).render == "Plain")
  }

  test("background color with foreground and style") {
    val rendered = "Alert".colorBg(Color.Red).color(Color.White).style(Style.Bold).render
    assert(rendered.contains("\u001b[41m"))
    assert(rendered.contains("\u001b[37m"))
    assert(rendered.contains("\u001b[1m"))
    assert(rendered.contains("Alert"))
  }

  test("background color multiline") {
    val rendered = "Line1\nLine2".colorBg(Color.Green).render
    val lines = rendered.split('\n')
    assert(lines.length == 2)
    lines.foreach { line =>
      assert(line.contains("\u001b[42m"))
      assert(line.contains("\u001b[0m"))
    }
  }

  test("background color on box") {
    val rendered = box()("content").colorBg(Color.Yellow).render
    assert(rendered.contains("\u001b[43m"))
    assert(rendered.contains("content"))
  }

  test("background color extended") {
    val full = "Test".colorBg(Color.Full(196)).render
    assert(full.contains("48;5;196"))

    val trueColor = "RGB".colorBg(Color.True(255, 128, 0)).render
    assert(trueColor.contains("48;2;255;128;0"))
  }

  test("Ask.renderInputFrame shows value with cursor when non-empty") {
    val frame = Ask.renderInputFrame("› ", "hello", "type something")
    assert(frame.startsWith("› hello"))
    assert(frame.contains("▌"))
    assert(!frame.contains("type something"))
  }

  test("Ask.renderInputFrame shows dim placeholder when value is empty") {
    val frame = Ask.renderInputFrame("› ", "", "type something")
    assert(frame.contains("type something"))
    assert(frame.contains("[2m"))
  }

  test("Ask.renderConfirmFrame highlights the active button with a bg color") {
    val yes = Ask.renderConfirmFrame("ok?", yes = true, "Yes", "No")
    assert(yes.contains("ok?"))
    assert(yes.contains("Yes"))
    assert(yes.contains("No"))
    assert(yes.contains("[42m"))
    assert(yes.contains("[2m"))

    val no = Ask.renderConfirmFrame("ok?", yes = false, "Yes", "No")
    assert(no.contains("[41m"))
    assert(yes != no)
  }

  test("Ask.renderWriteFrame shows placeholder when empty and value otherwise") {
    val empty = Ask.renderWriteFrame("Tell me", "", "type something", "ctrl+d to submit")
    assert(empty.contains("Tell me"))
    assert(empty.contains("type something"))
    assert(empty.contains("ctrl+d to submit"))
    assert(empty.contains("[2m"))

    val typed = Ask.renderWriteFrame("Tell me", "hello\nworld", "type something", "")
    assert(typed.contains("hello"))
    assert(typed.contains("world"))
    assert(!typed.contains("type something"))
  }

  test("Ask.fuzzyScore matches subsequence and skips non-matches") {
    assert(Ask.fuzzyScore("", "anything").isDefined)
    assert(Ask.fuzzyScore("ban", "Banana").isDefined)
    assert(Ask.fuzzyScore("xyz", "Banana").isEmpty)

    val tight = Ask.fuzzyScore("ban", "Banana").get
    val loose = Ask.fuzzyScore("bna", "Banana").get
    assert(tight < loose)
  }

  test("Ask.fuzzyMatches keeps matchers and drops non-matchers") {
    val items = Seq("Banana", "Strawberry", "Watermelon", "Mango")
    val matches = Ask.fuzzyMatches("wat", items, (s: String) => s)
    assert(matches.head == "Watermelon")
    assert(!matches.contains("Banana"))
    assert(!matches.contains("Mango"))
  }

  test("Ask.renderFilterFrame shows matches with selection marker") {
    val items = Seq("apple", "banana", "cherry")
    val matches = Ask.fuzzyMatches("a", items, (s: String) => s)
    val frame = Ask.renderFilterFrame("› ", "a", matches, 0, 10, (s: String) => s)
    assert(frame.contains("›"))
    assert(frame.contains("apple"))
    assert(frame.contains("banana"))
  }

  test("Ask.renderFilterFrame reports no matches") {
    val frame = Ask.renderFilterFrame("› ", "zz", Seq.empty[String], 0, 10, (s: String) => s)
    assert(frame.contains("(no matches)"))
  }

  test("Ask.renderFileFrame marks dirs and highlights selection") {
    import layoutz.Ask.FileEntry
    val entries = Seq(FileEntry("..", true), FileEntry("src", true), FileEntry("README.md", false))
    val frame = Ask.renderFileFrame("/some/path", entries, 1, 12)
    assert(frame.contains("/some/path"))
    assert(frame.contains("src/"))
    assert(frame.contains("README.md"))
    assert(frame.contains("›"))
  }

  test("Ask.renderPagerFrame shows numbered lines and a status bar") {
    val lines = (1 to 5).map(i => s"line $i").toArray
    val frame = Ask.renderPagerFrame(lines, 0, 3, 80, lineNumbers = true)
    val parts = frame.split('\n')
    assert(parts.head.contains("line 1"))
    assert(parts.head.contains("1 │"))
    assert(parts(2).contains("line 3"))
    assert(parts.last.contains("of 5"))
  }

  test("Ask.renderPagerFrame truncates lines wider than width") {
    val long = "x" * 200
    val frame = Ask.renderPagerFrame(Array(long), 0, 1, 30, lineNumbers = false)
    val firstLine = frame.split('\n').head
    assert(firstLine.endsWith("…"))
    assert(firstLine.length <= 30)
  }

  test("Ask.renderChooseFrame marks the selected row") {
    val frame = Ask.renderChooseFrame("Pick", Seq("a", "b", "c"), 1, (s: String) => s)
    val lines = frame.split('\n')
    assert(lines.head == "Pick")
    assert(lines(1).contains("a") && !lines(1).contains("›"))
    assert(lines(2).contains("›") && lines(2).contains("b"))
    assert(lines(3).contains("c") && !lines(3).contains("›"))
  }

  test("Ask.renderChooseFrame works with no prompt") {
    val frame = Ask.renderChooseFrame("", Seq("only"), 0, (s: String) => s)
    assert(!frame.contains("\n"))
    assert(frame.contains("›"))
    assert(frame.contains("only"))
  }

  test("Ask.renderChooseManyFrame shows checkbox marks and a selection count") {
    val items = Seq("a", "b", "c")
    val frame = Ask.renderChooseManyFrame(
      "Pick",
      items,
      idx = 1,
      selected = Set(0, 2),
      limit = 0,
      (s: String) => s
    )
    assert(frame.contains("Pick"))
    assert(frame.contains("[x]"))
    assert(frame.contains("[ ]"))
    assert(frame.contains("(2)"))
    val lines = frame.split('\n')
    assert(lines(2).contains("›"))
  }

  test("Ask.renderChooseManyFrame shows N/limit badge when limit is set") {
    val frame = Ask.renderChooseManyFrame(
      "Pick",
      Seq("a", "b", "c"),
      idx = 0,
      selected = Set(0),
      limit = 2,
      (s: String) => s
    )
    assert(frame.contains("(1/2)"))
  }

  test("Ask.renderChooseManyFrame hides badge when nothing selected and no limit") {
    val frame = Ask.renderChooseManyFrame(
      "Pick",
      Seq("a", "b"),
      idx = 0,
      selected = Set.empty,
      limit = 0,
      (s: String) => s
    )
    assert(!frame.contains("(0)"))
    assert(!frame.contains("/"))
  }

}
