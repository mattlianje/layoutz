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
    // This is the exact example from scala-style-demo.ts
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
