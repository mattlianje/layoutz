import { describe, it, expect } from 'vitest';
import {
  layout,
  section,
  kv,
  ul,
  ol,
  box,
  row,
  hr,
  progressBar,
  statusCard,
  table,
  text,
  br,
  BorderStyle,
  getWidth,
  getHeight,
  underline,
  underlined,
  margin,
  margins,
  tree
} from './layoutz';

describe('layoutz', () => {
  it('should create basic text', () => {
    const t = text("Hello World");
    expect(t.render()).toBe("Hello World");
  });

  it('should handle key-value pairs', () => {
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

  it('should create unordered lists', () => {
    const list = ul(
      text("Connected to database"),
      text("Loaded 28 models"),
      text("Cache warmed")
    );
    
    const expected = `• Connected to database
• Loaded 28 models
• Cache warmed`;
    
    expect(list.render()).toBe(expected);
  });

  it('should create ordered lists', () => {
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

  it('should create boxes', () => {
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

  it('should create sections', () => {
    const sectionElement = section("Database")(
      kv(["host", "localhost"], ["port", "5432"])
    );
    
    const rendered = sectionElement.render();
    expect(rendered).toContain("=== Database ===");
    expect(rendered).toContain("host:");
    expect(rendered).toContain("localhost");
  });

  it('should create progress bars', () => {
    const bar = progressBar(text("Download"), 0.72);
    expect(bar.render()).toBe("Download [██████████████──────] 72%");
  });

  it('should create status cards in rows', () => {
    const result = row(
      statusCard(text("Jobs"), text("✔ 132")),
      statusCard(text("Errors"), text("✘ 7")),
      statusCard(text("Time"), text("12m 33s"))
    );
    
    const rendered = result.render();
    expect(rendered).toContain("Jobs");
    expect(rendered).toContain("✔ 132");
    expect(rendered).toContain("Errors");
    expect(rendered).toContain("✘ 7");
  });

  it('should create tables', () => {
    const tableElement = table(
      [text("Name"), text("Role"), text("Status")],
      [
        [text("Alice"), text("Engineer"), text("Online")],
        [text("Bob"), text("Designer"), text("Offline")],
        [text("Eve"), text("QA"), text("Away")]
      ]
    );
    
    const rendered = tableElement.render();
    expect(rendered).toContain("Name");
    expect(rendered).toContain("Alice");
    expect(rendered).toContain("Engineer");
    expect(rendered).toContain("┌");
    expect(rendered).toContain("├");
    expect(rendered).toContain("└");
  });

  it('should handle line breaks', () => {
    const withBreaks = layout(
      text("First line"),
      br(),
      text("Second line"),
      br(2),
      text("Third line")
    );
    
    const rendered = withBreaks.render();
    const lines = rendered.split('\n');
    expect(lines).toContain("First line");
    expect(lines).toContain("Second line");
    expect(lines).toContain("Third line");
    expect(lines.length).toBeGreaterThan(5); // Should have empty lines
  });

  it('should handle complex layouts', () => {
    const dashboard = layout(
      section("System Status")(
        row(
          statusCard(text("CPU"), text("45%")),
          statusCard(text("Memory"), text("78%"))
        )
      ),
      box("Activity")(
        ul(
          text("User logged in"),
          text("Database updated")
        )
      )
    );
    
    const rendered = dashboard.render();
    expect(rendered).toContain("=== System Status ===");
    expect(rendered).toContain("CPU");
    expect(rendered).toContain("45%");
    expect(rendered).toContain("Activity");
    expect(rendered).toContain("• User logged in");
  });

  it('should calculate element dimensions', () => {
    const simpleText = text("Hello");
    expect(getWidth(simpleText)).toBe(5);
    expect(getHeight(simpleText)).toBe(1);
    
    const multiline = text("Line 1\nLonger line 2\nShort");
    expect(getWidth(multiline)).toBe(13); // "Longer line 2"
    expect(getHeight(multiline)).toBe(3);
  });

  it('should handle nested lists', () => {
    const nested = ul(
      text("Backend"),
      ul("◦")(
        text("API"),
        text("Database")
      ),
      text("Frontend")
    );
    
    const rendered = nested.render();
    expect(rendered).toContain("• Backend");
    expect(rendered).toContain("  ◦ API");
    expect(rendered).toContain("  ◦ Database");
    expect(rendered).toContain("• Frontend");
  });

  it('should create horizontal rules', () => {
    const rule1 = hr();
    const rule2 = hr("=", 20);
    
    expect(rule1.render()).toBe("─".repeat(50));
    expect(rule2.render()).toBe("=".repeat(20));
  });

  // NEW TESTS FOR UNDERLINE AND MARGIN

  it('should underline text with default character', () => {
    const underlined = underline()(text("Hello"));
    const expected = `Hello
─────`;
    expect(underlined.render()).toBe(expected);
  });

  it('should underline text with custom character', () => {
    const underlined = underline("=")(text("Title"));
    const expected = `Title
=====`;
    expect(underlined.render()).toBe(expected);
  });

  it('should underline multiline text', () => {
    const underlined = underline()(text("Line 1\nLonger line 2"));
    const expected = `Line 1
Longer line 2
─────────────`;
    expect(underlined.render()).toBe(expected);
  });

  it('should handle underline with long pattern', () => {
    const underlined = underline("─═─═─═─═")(text("Short"));
    const expected = `Short
─═─═─`;
    expect(underlined.render()).toBe(expected);
  });

  it('should create margins with custom prefix', () => {
    const marginElement = margin("[LOG]")(text("Hello World"));
    expect(marginElement.render()).toBe("[LOG] Hello World");
  });

  it('should create margins with multiple elements', () => {
    const marginElement = margin("[INFO]")(
      text("Line 1"),
      text("Line 2")
    );
    
    const expected = `[INFO] Line 1
[INFO] Line 2`;
    
    expect(marginElement.render()).toBe(expected);
  });

  it('should create error margins with ANSI colors', () => {
    const errorElement = margins.error(text("Connection failed"));
    const rendered = errorElement.render();
    
    expect(rendered).toContain("\u001b[31m"); // Red color
    expect(rendered).toContain("error");
    expect(rendered).toContain("Connection failed");
    expect(rendered).toContain("\u001b[0m"); // Reset
  });

  it('should create warn margins', () => {
    const warnElement = margins.warn(text("Performance issue"));
    const rendered = warnElement.render();
    
    expect(rendered).toContain("\u001b[33m"); // Yellow
    expect(rendered).toContain("warn");
    expect(rendered).toContain("Performance issue");
  });

  it('should create success margins', () => {
    const successElement = margins.success(text("All systems operational"));
    const rendered = successElement.render();
    
    expect(rendered).toContain("\u001b[32m"); // Green
    expect(rendered).toContain("success");
    expect(rendered).toContain("All systems operational");
  });

  it('should create info margins', () => {
    const infoElement = margins.info(text("System information"));
    const rendered = infoElement.render();
    
    expect(rendered).toContain("\u001b[36m"); // Cyan
    expect(rendered).toContain("info");
    expect(rendered).toContain("System information");
  });

  it('should handle margins with complex nested elements', () => {
    const complexMargin = margins.error(
      row(
        statusCard(text("API"), text("DOWN")),
        statusCard(text("DB"), text("SLOW"))
      )
    );
    
    const rendered = complexMargin.render();
    expect(rendered).toContain("error");
    expect(rendered).toContain("API");
    expect(rendered).toContain("DOWN");
    expect(rendered).toContain("DB");
    expect(rendered).toContain("SLOW");
    
    // Each line should have the error prefix
    const lines = rendered.split('\n');
    lines.forEach(line => {
      expect(line).toContain("error");
    });
  });

  it('should handle margins with boxes', () => {
    const boxedContent = box("Status")(
      text("Service is running"),
      text("Memory usage: 45%")
    );
    
    const marginedBox = margins.info(boxedContent);
    const rendered = marginedBox.render();
    
    expect(rendered).toContain("┌");
    expect(rendered).toContain("│");
    expect(rendered).toContain("└");
    expect(rendered).toContain("Status");
    expect(rendered).toContain("Service is running");
    expect(rendered).toContain("Memory usage: 45%");
    
    const lines = rendered.split('\n');
    lines.forEach(line => {
      expect(line).toContain("info");
    });
  });

  it('should handle empty margin content', () => {
    const emptyMargin = margin("[EMPTY]")(text(""));
    expect(emptyMargin.render()).toBe("[EMPTY] ");
  });

  it('should use underlined function directly', () => {
    const result = underlined(text("Direct Usage"), "*");
    const expected = `Direct Usage
************`;
    expect(result.render()).toBe(expected);
  });

  // TREE STRUCTURE TESTS

  it('should create simple tree leaf', () => {
    const leaf = tree("simple-file.txt");
    expect(leaf.render()).toBe("simple-file.txt");
  });

  it('should create tree with children', () => {
    const simpleTree = tree("Files")(
      tree("docs")(tree("README.md"), tree("CHANGELOG.md"))
    );

    const expected = `Files
└── docs/
    ├── README.md
    └── CHANGELOG.md`;

    expect(simpleTree.render()).toBe(expected);
  });

  it('should create complex project tree structure', () => {
    const projectTree = tree("Project Structure")(
      tree("src")(
        tree("main")(tree("Main.ts"), tree("Utils.ts")),
        tree("test")(tree("MainSpec.ts"), tree("UtilsSpec.ts"))
      )
    );

    const expected = `Project Structure
└── src/
    ├── main/
    │   ├── Main.ts
    │   └── Utils.ts
    └── test/
        ├── MainSpec.ts
        └── UtilsSpec.ts`;

    expect(projectTree.render()).toBe(expected);
  });

  it('should handle tree with multiple top-level branches', () => {
    const multiTree = tree("Project")(
      tree("frontend")(
        tree("components")(tree("Button.tsx"), tree("Input.tsx")),
        tree("pages")(tree("Home.tsx"))
      ),
      tree("backend")(
        tree("api")(tree("routes.ts")),
        tree("models")(tree("User.ts"))
      )
    );

    const rendered = multiTree.render();
    expect(rendered).toContain("Project");
    expect(rendered).toContain("├── frontend/");
    expect(rendered).toContain("└── backend/");
    expect(rendered).toContain("│   ├── Button.tsx");
    expect(rendered).toContain("│   └── Input.tsx");
  });

  it('should create consistent tree syntax', () => {
    const consistentTree = tree("Project")(
      tree("src")(
        tree("main")(tree("App.ts")),
        tree("test")(tree("AppSpec.ts"))
      )
    );

    const expected = `Project
└── src/
    ├── main/
    │   └── App.ts
    └── test/
        └── AppSpec.ts`;

    expect(consistentTree.render()).toBe(expected);
  });

  it('should handle deeply nested trees', () => {
    const deepTree = tree("Root")(
      tree("Level1")(
        tree("Level2")(
          tree("Level3")(
            tree("DeepFile.txt")
          )
        )
      )
    );

    const rendered = deepTree.render();
    expect(rendered).toContain("Root");
    expect(rendered).toContain("└── Level1/");
    expect(rendered).toContain("    └── Level2/");
    expect(rendered).toContain("        └── Level3/");
    expect(rendered).toContain("            └── DeepFile.txt");
  });

  // IMPROVED NESTED BULLETS TESTS

  it('should create proper nested bullets with different styles', () => {
    const nestedBullets = ul(
      text("Backend"),
      ul("◦")(
        text("API"),
        text("Database")
      ),
      text("Frontend"),
      ul("◦")(
        text("Components"),
        ul("▪")(
          text("Header"),
          text("Footer")
        )
      )
    );

    const rendered = nestedBullets.render();
    expect(rendered).toContain("• Backend");
    expect(rendered).toContain("  ◦ API");
    expect(rendered).toContain("  ◦ Database");
    expect(rendered).toContain("• Frontend");
    expect(rendered).toContain("  ◦ Components");
    expect(rendered).toContain("    ▪ Header");
    expect(rendered).toContain("    ▪ Footer");
  });

  it('should automatically cycle bullet styles at different levels', () => {
    const autoBullets = ul(
      text("Level 1"),
      ul()(
        text("Level 2"),
        ul()(
          text("Level 3"),
          ul()(
            text("Level 4")
          )
        )
      )
    );

    const rendered = autoBullets.render();
    expect(rendered).toContain("• Level 1");      // Level 0: •
    expect(rendered).toContain("  ◦ Level 2");    // Level 1: ◦
    expect(rendered).toContain("    ▪ Level 3");  // Level 2: ▪
    expect(rendered).toContain("      • Level 4"); // Level 3: cycles back to •
  });
});
