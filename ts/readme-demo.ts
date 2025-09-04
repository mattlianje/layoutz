#!/usr/bin/env ts-node

import {
  layout,
  section,
  kv,
  ul,
  box,
  row,
  table,
  text,
  center,
  underline,
  statusCard,
  margin,
  inlineBar,
  tree,
  BorderStyle
} from './layoutz';

console.log("🎨 Layoutz TypeScript Demo - README Example\n");

// Complex table with nested bullets
let tableElement = table(BorderStyle.Round)(
  [text("Name"), text("Role"), text("Status")],
  [
    [text("Alice"), text("Engineer"), text("Online")],
    [text("Bob"), text("Designer"), text("Offline")],
    [text("Eve"), text("QA"), text("Away")],
    // Nested bullets in table!
    [ul(text("Gegard"), ul()(text("Mousasi"), ul()(text("was a BAD man")))), text("Fighter"), text("Nasty")]
  ]
);

// Full demo
let demo = layout(
  center(row(text("Layoutz"), underline("ˆ")(text("DEMO")))),
  row(
    statusCard(BorderStyle.Thick)(text("API"), text("UP")),
    statusCard(text("Users"), text("1.2K")),
    statusCard(BorderStyle.Double)(text("CPU"), text("23%")),
    tableElement,
    section("Pugilists")(kv(["Kazushi", "Sakuraba"], ["Jet", "Li"], ["Rory", "MacDonald"]))
  ),
  margin("[TypeScript!]")(
    row(
      box("Deploy Status")(layout(
        inlineBar(text("Build"), 1.0),
        inlineBar(text("Test"), 0.8),
        inlineBar(text("Deploy"), 0.3)
      )),
      tree("📁 Project")(tree("src")(tree("main.ts"), tree("api.ts")))
    )
  )
);
