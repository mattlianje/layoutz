import { describe, it, expect } from "vitest";
import {
  // Core elements
  layout,
  text,

  // Containers
  box,
  section,
  banner,

  // Layout
  row,
  tightRow,
  center,
  autoCenter,
  margin,
  columns,

  // Lists and data
  ul,
  ol,
  kv,
  table,
  tree,
  chart,

  // Widgets
  statusCard,
  inlineBar,
  hr,
  vr,
  underline,
  br,

  // Text formatting
  pad,
  truncate,
  wrap,
  justify,
  leftAlign,
  rightAlign,

  // Utilities
  empty,
  getWidth,
  getHeight,

  // Constants
  Border,
  BorderStyle,
  margins,

  // Colors and styles
  Color,
  Style,
  color,
  style,
  styles,
  colorFull,
  colorTrue,
  underlineColored,
  marginColored,

  // Test default import too
  default as layoutzDefault,
} from "./layoutz";

describe("layoutz", () => {
  // ═════════════════════════════════════════════════════════════════════════════
  // BASIC COMPONENTS
  // ═════════════════════════════════════════════════════════════════════════════

  it("should create basic text", () => {
    const t = text("Hello World");
    expect(t.render()).toBe("Hello World");
  });

  it("should support automatic string conversion", () => {
    const kvElement = kv(
      ["user", "alice"],
      ["role", "admin"],
      ["status", "active"]
    );

    const expected = `user:   alice
role:   admin
status: active`;

    expect(kvElement.render()).toBe(expected);
  });

  it("should create unordered lists with automatic string conversion", () => {
    const list = ul(
      "Connected to database",
      "Loaded 28 models",
      "Cache warmed"
    );

    const expected = `• Connected to database
• Loaded 28 models
• Cache warmed`;

    expect(list.render()).toBe(expected);
  });

  it("should create ordered lists", () => {
    const list = ol(
      text("First item"),
      text("Second item"),
      text("Third item")
    );

    const expected = `1. First item
2. Second item
3. Third item`;

    expect(list.render()).toBe(expected);
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // NESTED LISTS (FIXED)
  // ═════════════════════════════════════════════════════════════════════════════

  it("should handle nested unordered lists correctly", () => {
    const nested = ul("Gegard", ul("Mousasi", ul("was a BAD man")));

    const expected = `• Gegard
  ◦ Mousasi
    ▪ was a BAD man`;

    expect(nested.render()).toBe(expected);
  });

  it("should create nested lists with automatic bullet cycling", () => {
    const nested = ul("Backend", ul("API", "Database"), "Frontend");

    const rendered = nested.render();
    expect(rendered).toContain("• Backend");
    expect(rendered).toContain("  ◦ API");
    expect(rendered).toContain("  ◦ Database");
    expect(rendered).toContain("• Frontend");
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // CONTAINERS & LAYOUT
  // ═════════════════════════════════════════════════════════════════════════════

  it("should create boxes with automatic string conversion", () => {
    const boxElement = box("Config")(
      kv(["env", "prod"], ["region", "us-east-1"])
    );

    const rendered = boxElement.render();
    expect(rendered).toContain("Config");
    expect(rendered).toContain("env:");
    expect(rendered).toContain("prod");
    expect(rendered).toContain("┌");
    expect(rendered).toContain("└");
  });

  it("should create sections", () => {
    const sectionElement = section("Database")(
      kv(["host", "localhost"], ["port", "5432"])
    );

    const rendered = sectionElement.render();
    expect(rendered).toContain("=== Database ===");
    expect(rendered).toContain("host:");
    expect(rendered).toContain("localhost");
  });

  it("should create banners with fluent border API", () => {
    const bannerElement = banner("Welcome to Layoutz").border(Border.Thick);
    const rendered = bannerElement.render();

    expect(rendered).toContain("Welcome to Layoutz");
    expect(rendered).toContain("┏"); // Thick border characters
    expect(rendered).toContain("┗");
  });

  it("should create column layouts", () => {
    const columnsElement = columns(
      ul("Frontend", "React", "TypeScript"),
      ul("Backend", "Node.js", "Express")
    );

    const rendered = columnsElement.render();
    expect(rendered).toContain("Frontend");
    expect(rendered).toContain("Backend");
    expect(rendered).toContain("React");
    expect(rendered).toContain("Node.js");
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // WIDGETS & DATA DISPLAY
  // ═════════════════════════════════════════════════════════════════════════════

  it("should create progress bars with automatic string conversion", () => {
    const bar = inlineBar("Download", 0.72);
    expect(bar.render()).toBe("Download [██████████████──────] 72%");
  });

  it("should create status cards with fluent borders", () => {
    const card = statusCard("API", "UP").border(Border.Double);
    const rendered = card.render();

    expect(rendered).toContain("API");
    expect(rendered).toContain("UP");
    expect(rendered).toContain("╔"); // Double border characters
    expect(rendered).toContain("╚");
  });

  it("should create tables with fluent borders and automatic string conversion", () => {
    const tableElement = table(
      ["Name", "Role", "Status"],
      [
        ["Alice", "Engineer", "Online"],
        ["Bob", "Designer", "Offline"],
        ["Eve", "QA", "Away"],
      ]
    ).border(Border.Round);

    const rendered = tableElement.render();
    expect(rendered).toContain("Name");
    expect(rendered).toContain("Alice");
    expect(rendered).toContain("Engineer");
    expect(rendered).toContain("╭"); // Round border characters
    expect(rendered).toContain("╰");
  });

  it("should create charts", () => {
    const chartElement = chart(
      ["Sales", 85],
      ["Marketing", 65],
      ["Engineering", 95]
    );

    const rendered = chartElement.render();
    expect(rendered).toContain("Sales");
    expect(rendered).toContain("85");
    expect(rendered).toContain("█"); // Bar characters
    expect(rendered).toContain("│");
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // TEXT FORMATTING
  // ═════════════════════════════════════════════════════════════════════════════

  it("should add padding around elements", () => {
    const padded = pad(2)("Hello");
    const rendered = padded.render();

    expect(rendered).toContain("  Hello  ");
    const lines = rendered.split("\n");
    expect(lines.length).toBeGreaterThan(3); // Should have vertical padding
  });

  it("should truncate text with ellipsis", () => {
    const longText =
      "This is a very long line of text that should be truncated";
    const truncated = truncate(20, "...")(longText);

    expect(truncated.render()).toBe("This is a very lo...");
  });

  it("should wrap text at word boundaries", () => {
    const longText =
      "This is a long paragraph that should wrap at word boundaries";
    const wrapped = wrap(20)(longText);
    const rendered = wrapped.render();

    expect(rendered).toContain("This is a long");
    expect(rendered).toContain("paragraph that");
    expect(rendered.split("\n").length).toBeGreaterThan(1);
  });

  it("should justify text", () => {
    const justified = justify(30)("Short text");
    const rendered = justified.render();

    // Basic test - just make sure it doesn't crash and contains the text
    expect(rendered).toContain("Short");
    expect(rendered).toContain("text");
  });

  it("should left align text", () => {
    const aligned = leftAlign(20)("Left");
    const rendered = aligned.render();

    expect(rendered.length).toBe(20);
    expect(rendered.startsWith("Left")).toBe(true);
    expect(rendered.endsWith(" ")).toBe(true);
  });

  it("should right align text", () => {
    const aligned = rightAlign(20)("Right");
    const rendered = aligned.render();

    expect(rendered.length).toBe(20);
    expect(rendered.endsWith("Right")).toBe(true);
    expect(rendered.startsWith(" ")).toBe(true);
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // LAYOUT & SPACING
  // ═════════════════════════════════════════════════════════════════════════════

  it("should handle line breaks", () => {
    const withBreaks = layout(
      "First line",
      br(),
      "Second line",
      br(2),
      "Third line"
    );

    const rendered = withBreaks.render();
    const lines = rendered.split("\n");
    expect(lines).toContain("First line");
    expect(lines).toContain("Second line");
    expect(lines).toContain("Third line");
    expect(lines.length).toBeGreaterThan(5);
  });

  it("should create horizontal rules", () => {
    const rule1 = hr();
    const rule2 = hr("=", 20);

    expect(rule1.render()).toBe("─".repeat(50));
    expect(rule2.render()).toBe("=".repeat(20));
  });

  it("should create vertical rules", () => {
    const vrule = vr(3);
    const rendered = vrule.render();

    expect(rendered).toBe("│\n│\n│");
  });

  it("should center text", () => {
    const centered = center("Centered", 20);
    const rendered = centered.render();

    expect(rendered.length).toBe(20);
    expect(rendered).toContain("Centered");
    expect(rendered.trim()).toBe("Centered");
  });

  it("should create rows", () => {
    const rowElement = row(
      statusCard("CPU", "45%"),
      statusCard("Memory", "78%")
    );

    const rendered = rowElement.render();
    expect(rendered).toContain("CPU");
    expect(rendered).toContain("45%");
    expect(rendered).toContain("Memory");
    expect(rendered).toContain("78%");
  });

  it("should create tight rows without spacing", () => {
    const tightRowElement = tightRow(
      text("A"),
      text("B"),
      text("C")
    );

    const rendered = tightRowElement.render();
    expect(rendered).toBe("ABC");
  });

  it("should create tight rows with colored blocks", () => {
    const coloredBlocks = tightRow(
      text("█").color(Color.Red),
      text("█").color(Color.Green),
      text("█").color(Color.Blue)
    );

    const rendered = coloredBlocks.render();
    expect(rendered).toContain("█");
    expect(rendered).toContain("\x1b[31m"); // Red
    expect(rendered).toContain("\x1b[32m"); // Green
    expect(rendered).toContain("\x1b[34m"); // Blue
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // UNDERLINES & MARGINS
  // ═════════════════════════════════════════════════════════════════════════════

  it("should underline text with default character", () => {
    const underlined = underline()("Hello");
    const expected = `Hello
─────`;
    expect(underlined.render()).toBe(expected);
  });

  it("should underline text with custom character", () => {
    const underlined = underline("=")("Title");
    const expected = `Title
=====`;
    expect(underlined.render()).toBe(expected);
  });

  it("should create margins with custom prefix", () => {
    const marginElement = margin("[LOG]")("Hello World");
    expect(marginElement.render()).toBe("[LOG] Hello World");
  });

  it("should create error margins with ANSI colors", () => {
    const errorElement = margins.error("Connection failed");
    const rendered = errorElement.render();

    expect(rendered).toContain("\u001b[31m"); // Red color
    expect(rendered).toContain("error");
    expect(rendered).toContain("Connection failed");
    expect(rendered).toContain("\u001b[0m"); // Reset
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // TREE STRUCTURES
  // ═════════════════════════════════════════════════════════════════════════════

  it("should create simple tree leaf", () => {
    const leaf = tree("simple-file.txt");
    expect(leaf.render()).toBe("simple-file.txt");
  });

  it("should create tree with children", () => {
    const simpleTree = tree("Files")(
      tree("docs")(tree("README.md"), tree("CHANGELOG.md"))
    );

    const expected = `Files
└── docs/
    ├── README.md
    └── CHANGELOG.md`;

    expect(simpleTree.render()).toBe(expected);
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // UTILITIES
  // ═════════════════════════════════════════════════════════════════════════════

  it("should calculate element dimensions", () => {
    const simpleText = text("Hello");
    expect(getWidth(simpleText)).toBe(5);
    expect(getHeight(simpleText)).toBe(1);

    const multiline = text("Line 1\nLonger line 2\nShort");
    expect(getWidth(multiline)).toBe(13); // "Longer line 2"
    expect(getHeight(multiline)).toBe(3);
  });

  it("should handle empty elements", () => {
    const emptyElement = empty();
    expect(emptyElement.render()).toBe("");
  });

  it("should handle conditional rendering with empty", () => {
    const showOptional = false;
    const conditional = layout(
      "Always visible",
      showOptional ? "Optional content" : empty(),
      "Also always visible"
    );

    const rendered = conditional.render();
    expect(rendered).toContain("Always visible");
    expect(rendered).toContain("Also always visible");
    expect(rendered).not.toContain("Optional content");
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // IMPORT STYLES
  // ═════════════════════════════════════════════════════════════════════════════

  it("should work with default import", () => {
    const result = layoutzDefault.layout(
      layoutzDefault.text("Using default import")
    );
    expect(result.render()).toBe("Using default import");
  });

  it("should work with Border enum", () => {
    expect(Border.Single).toBe(BorderStyle.Single);
    expect(Border.Double).toBe(BorderStyle.Double);
    expect(Border.Thick).toBe(BorderStyle.Thick);
    expect(Border.Round).toBe(BorderStyle.Round);
  });

  // ═════════════════════════════════════════════════════════════════════════════
  // COMPLEX INTEGRATION TESTS
  // ═════════════════════════════════════════════════════════════════════════════

  it("should handle complex dashboard layout", () => {
    const dashboard = layout(
      banner("System Dashboard").border(Border.Double),
      "",
      row(
        statusCard("CPU", "67%"),
        statusCard("Memory", "45%").border(Border.Thick),
        statusCard("Disk", "23%")
      ),
      "",
      section("Performance")(
        chart(["API Calls", 1250], ["Database", 890], ["Cache Hits", 2100])
      ),
      "",
      columns(
        layout("🖥️ Hardware:", ul("CPU: Intel i9", "RAM: 32GB", "SSD: 1TB")),
        layout(
          "📊 Metrics:",
          kv(["Uptime", "99.9%"], ["Users", "1.2K"], ["Requests", "45K"])
        )
      )
    );

    const rendered = dashboard.render();
    expect(rendered).toContain("System Dashboard");
    expect(rendered).toContain("CPU");
    expect(rendered).toContain("67%");
    expect(rendered).toContain("Performance");
    expect(rendered).toContain("API Calls");
    expect(rendered).toContain("Hardware");
    expect(rendered).toContain("Intel i9");
    expect(rendered).toContain("Uptime");
  });

  it("should handle nested lists in table cells", () => {
    const tableWithNestedLists = table(
      ["Name", "Role", "Skills"],
      [
        ["Alice", "Engineer", ul("TypeScript", "React", "Node.js")],
        ["Bob", "Designer", ul("Figma", "Sketch", "Photoshop")],
      ]
    ).border(Border.Round);

    const rendered = tableWithNestedLists.render();
    expect(rendered).toContain("Alice");
    expect(rendered).toContain("Engineer");
    expect(rendered).toContain("• TypeScript");
    expect(rendered).toContain("• React");
    expect(rendered).toContain("Bob");
    expect(rendered).toContain("• Figma");
  });

  it("should handle the scala-style demo example", () => {
    const t = table(
      ["Name", "Role", "Status"],
      [
        ["Alice", "Engineer", "Online"],
        ["Eve", "QA", "Away"],
        [ul("Gegard", ul("Mousasi", ul("was a BAD man"))), "Fighter", "Nasty"],
      ]
    ).border(Border.Round);

    const rendered = t.render();
    expect(rendered).toContain("Alice");
    expect(rendered).toContain("Engineer");
    expect(rendered).toContain("• Gegard");
    expect(rendered).toContain("◦ Mousasi");
    expect(rendered).toContain("▪ was a BAD man");
    expect(rendered).toContain("Fighter");
    expect(rendered).toContain("Nasty");
    expect(rendered).toContain("╭"); // Round borders
    expect(rendered).toContain("╰");
  });
});

// ═════════════════════════════════════════════════════════════════════════════
// COLORS AND STYLES
// ═════════════════════════════════════════════════════════════════════════════

describe("Colors", () => {
  it("should apply color functionally", () => {
    const rendered = color(Color.Red)(text("Hello")).render();
    expect(rendered).toContain("\x1b[31m");
    expect(rendered).toContain("Hello");
  });

  it("should apply color fluently", () => {
    const rendered = text("Hello").color(Color.Green).render();
    expect(rendered).toContain("\x1b[32m");
  });

  it("should skip NoColor", () => {
    const rendered = text("Hello").color(Color.NoColor).render();
    expect(rendered).toBe("Hello");
  });

  it("should override chained colors", () => {
    const rendered = text("Hello").color(Color.Red).color(Color.Blue).render();
    expect(rendered).toContain("\x1b[34m");
    expect(rendered).not.toContain("\x1b[31m");
  });

  it("should color containers", () => {
    expect(
      box("Title")(text("Content")).color(Color.Yellow).render()
    ).toContain("\x1b[33m");
    expect(statusCard("API", "UP").color(Color.Green).render()).toContain(
      "\x1b[32m"
    );
    expect(tree("Root").color(Color.Cyan).render()).toContain("\x1b[36m");
  });
});

describe("Styles", () => {
  it("should apply style functionally", () => {
    const rendered = style(Style.Bold)(text("Hello")).render();
    expect(rendered).toContain("\x1b[1m");
  });

  it("should apply style fluently", () => {
    const rendered = text("Hello").style(Style.Italic).render();
    expect(rendered).toContain("\x1b[3m");
  });

  it("should skip NoStyle", () => {
    expect(text("Hello").style(Style.NoStyle).render()).toBe("Hello");
  });

  it("should chain styles", () => {
    const rendered = text("Hello")
      .style(Style.Bold)
      .style(Style.Underline)
      .render();
    expect(rendered).toContain("\x1b[4m");
  });

  it("should combine multiple styles with styles() function", () => {
    const rendered = text("Hello").style(styles(Style.Bold, Style.Italic)).render();
    expect(rendered).toContain("\x1b[1m"); // Bold
    expect(rendered).toContain("\x1b[3m"); // Italic
  });

  it("should combine multiple styles with .styles() method", () => {
    const rendered = text("Hello").styles(Style.Bold, Style.Italic).render();
    expect(rendered).toContain("\x1b[1m"); // Bold
    expect(rendered).toContain("\x1b[3m"); // Italic
  });

  it("should combine three styles with .styles()", () => {
    const rendered = text("Hello")
      .styles(Style.Bold, Style.Italic, Style.Underline)
      .render();
    expect(rendered).toContain("\x1b[1m"); // Bold
    expect(rendered).toContain("\x1b[3m"); // Italic
    expect(rendered).toContain("\x1b[4m"); // Underline
  });

  it("should apply styles functionally with styles()", () => {
    const rendered = style(styles(Style.Bold, Style.Reverse))(text("Test")).render();
    expect(rendered).toContain("\x1b[1m");
    expect(rendered).toContain("\x1b[7m");
  });

  it("should work with combined styles on containers", () => {
    const rendered = box("Title")(text("Content"))
      .styles(Style.Bold, Style.Italic)
      .render();
    expect(rendered).toContain("\x1b[1m");
    expect(rendered).toContain("\x1b[3m");
  });

  it("should combine color and style", () => {
    const rendered = text("Hello").color(Color.Red).style(Style.Bold).render();
    expect(rendered).toContain("\x1b[1m");
    expect(rendered).toContain("\x1b[31m");
  });

  it("should combine color with multiple styles", () => {
    const rendered = text("Hello")
      .color(Color.Blue)
      .styles(Style.Bold, Style.Underline)
      .render();
    expect(rendered).toContain("\x1b[1m");
    expect(rendered).toContain("\x1b[4m");
    expect(rendered).toContain("\x1b[34m");
  });

  it("should work like Scala's Style.Reverse ++ Style.Bold", () => {
    // This is the JS/TS equivalent of Scala's: Style.Reverse ++ Style.Bold
    const rendered = text("Scala-style").styles(Style.Reverse, Style.Bold).render();
    expect(rendered).toContain("\x1b[7m"); // Reverse
    expect(rendered).toContain("\x1b[1m"); // Bold
  });
});

describe("Colored underlines", () => {
  it("should color underline only", () => {
    const lines = underlineColored("=", Color.Blue)(text("Test"))
      .render()
      .split("\n");
    expect(lines[0]).toBe("Test");
    expect(lines[1]).toContain("\x1b[34m");
  });

  it("should handle CJK width correctly", () => {
    const lines = underlineColored("^", Color.Magenta)(text("李連杰"))
      .render()
      .split("\n");
    const underline = lines[1].replace(/\x1b\[[0-9;]*m/g, "");
    expect(underline).toBe("^^^^^^");
  });
});

describe("Margins", () => {
  it("should color margin prefix", () => {
    const rendered = marginColored("[ERROR]", Color.Red)(text("Oops")).render();
    expect(rendered).toContain("\x1b[31m");
    expect(rendered).toContain("[ERROR]");
  });

  it("should use margins shorthand", () => {
    const plain = margins
      .error(text("Failed"))
      .render()
      .replace(/\x1b\[[0-9;]*m/g, "");
    expect(plain).toContain("[error]");
  });
});

// ═════════════════════════════════════════════════════════════════════════════
// 256-COLOR PALETTE AND TRUE COLOR SUPPORT
// ═════════════════════════════════════════════════════════════════════════════

describe("256-color palette support", () => {
  it("should create 256-color palette color", () => {
    const color256 = colorFull(196); // Bright red in 256-color palette
    expect(color256).toEqual({ type: "full", colorCode: 196 });
  });

  it("should apply 256-color palette to text", () => {
    const rendered = text("Hello").color(colorFull(21)).render();
    expect(rendered).toContain("\x1b[38;5;21m"); // 256-color code format
    expect(rendered).toContain("Hello");
    expect(rendered).toContain("\x1b[0m");
  });

  it("should apply 256-color functionally", () => {
    const rendered = color(colorFull(220))(text("Warning")).render();
    expect(rendered).toContain("\x1b[38;5;220m");
    expect(rendered).toContain("Warning");
  });

  it("should apply 256-color to containers", () => {
    const rendered = box("Title")(text("Content"))
      .color(colorFull(99))
      .render();
    expect(rendered).toContain("\x1b[38;5;99m");
  });

  it("should handle 256-color in underlines", () => {
    const lines = underlineColored("=", colorFull(160))(text("Test"))
      .render()
      .split("\n");
    expect(lines[0]).toBe("Test");
    expect(lines[1]).toContain("\x1b[38;5;160m");
  });

  it("should handle 256-color in margins", () => {
    const rendered = marginColored("[LOG]", colorFull(27))(text("Info")).render();
    expect(rendered).toContain("\x1b[38;5;27m");
    expect(rendered).toContain("[LOG]");
  });

  it("should chain 256-color with style", () => {
    const rendered = text("Bold and colored")
      .color(colorFull(208))
      .style(Style.Bold)
      .render();
    expect(rendered).toContain("\x1b[38;5;208m");
    expect(rendered).toContain("\x1b[1m");
  });
});

describe("True color (24-bit RGB) support", () => {
  it("should create true color", () => {
    const rgb = colorTrue(255, 100, 50);
    expect(rgb).toEqual({ type: "true", r: 255, g: 100, b: 50 });
  });

  it("should apply true color to text", () => {
    const rendered = text("Hello").color(colorTrue(255, 0, 0)).render();
    expect(rendered).toContain("\x1b[38;2;255;0;0m"); // RGB red
    expect(rendered).toContain("Hello");
    expect(rendered).toContain("\x1b[0m");
  });

  it("should apply true color functionally", () => {
    const rendered = color(colorTrue(0, 255, 0))(text("Green")).render();
    expect(rendered).toContain("\x1b[38;2;0;255;0m");
    expect(rendered).toContain("Green");
  });

  it("should apply true color to containers", () => {
    const rendered = statusCard("API", "UP")
      .color(colorTrue(100, 200, 150))
      .render();
    expect(rendered).toContain("\x1b[38;2;100;200;150m");
  });

  it("should handle true color in underlines", () => {
    const lines = underlineColored("^", colorTrue(255, 0, 255))(text("Test"))
      .render()
      .split("\n");
    expect(lines[0]).toBe("Test");
    expect(lines[1]).toContain("\x1b[38;2;255;0;255m");
  });

  it("should handle true color in margins", () => {
    const rendered = marginColored("[INFO]", colorTrue(50, 150, 250))(
      text("Message")
    ).render();
    expect(rendered).toContain("\x1b[38;2;50;150;250m");
    expect(rendered).toContain("[INFO]");
  });

  it("should chain true color with style", () => {
    const rendered = text("Italic and colored")
      .color(colorTrue(128, 128, 255))
      .style(Style.Italic)
      .render();
    expect(rendered).toContain("\x1b[38;2;128;128;255m");
    expect(rendered).toContain("\x1b[3m");
  });

  it("should work with gradient-like colors", () => {
    // Test a gradient from red to blue
    const colors = [];
    for (let i = 0; i < 5; i++) {
      const red = 255 - i * 51;
      const blue = i * 51;
      colors.push(text("█").color(colorTrue(red, 0, blue)).render());
    }

    colors.forEach((coloredBlock) => {
      expect(coloredBlock).toContain("\x1b[38;2;");
      expect(coloredBlock).toContain("█");
    });
  });

  it("should override chained true colors", () => {
    const rendered = text("Hello")
      .color(colorTrue(255, 0, 0))
      .color(colorTrue(0, 255, 0))
      .render();
    expect(rendered).toContain("\x1b[38;2;0;255;0m");
    expect(rendered).not.toContain("\x1b[38;2;255;0;0m");
  });

  it("should work in complex layouts", () => {
    const demo = row(
      statusCard("Users", "1.2K").color(colorTrue(100, 150, 255)),
      statusCard("API", "UP").color(colorTrue(100, 255, 100)),
      statusCard("CPU", "23%").color(colorTrue(255, 200, 100))
    );

    const rendered = demo.render();
    expect(rendered).toContain("\x1b[38;2;100;150;255m");
    expect(rendered).toContain("\x1b[38;2;100;255;100m");
    expect(rendered).toContain("\x1b[38;2;255;200;100m");
  });
});

describe("Fluent margin API", () => {
  it("should apply margin fluently", () => {
    const rendered = text("Hello").margin("[LOG]").render();
    expect(rendered).toContain("[LOG]");
  });

  it("should chain margin with color and style", () => {
    expect(text("Test").color(Color.Green).margin("[INFO]").render()).toContain(
      "[INFO]"
    );
    expect(text("Test").style(Style.Bold).margin("[!]").render()).toContain(
      "[!]"
    );
  });

  it("should work on containers", () => {
    expect(box("Title")(text("Content")).margin("[BOX]").render()).toContain(
      "[BOX]"
    );
    expect(statusCard("API", "UP").margin("[SVC]").render()).toContain("[SVC]");
    expect(tree("Root").margin(">> ").render()).toContain(">> ");
  });

  it("should chain everything", () => {
    const rendered = box("Deploy")(text("Building..."))
      .border(Border.Double)
      .color(Color.BrightBlue)
      .style(Style.Bold)
      .margin("[BUILD]")
      .render();

    expect(rendered).toContain("[BUILD]");
    expect(rendered).toContain("\x1b[94m");
    expect(rendered).toContain("\x1b[1m");
  });
});
