#!/usr/bin/env ts-node

import {
  layout,
  section,
  kv,
  ul,
  box,
  row,
  table,
  center,
  underline,
  statusCard,
  margin,
  inlineBar,
  tree,
  Border,
} from "./layoutz";

console.log("🎨 Scala-style Fluent API Demo\n");

/* Define layouts */
const t = table(
  ["Name", "Role", "Status"],
  [
    ["Alice", "Engineer", "Online"],
    ["Eve", "QA", "Away"],
    [ul("Gegard", ul("Mousasi", ul("was a BAD man"))), "Fighter", "Nasty"],
  ]
).border(Border.Round);

/* Nest, compose, combine them */
const d = layout(
  center(row("Layoutz", underline("ˆ")("DEMO"))),
  row(
    statusCard("Users", "1.2K"),
    statusCard("API", "UP").border(Border.Double),
    statusCard("CPU", "23%").border(Border.Thick),
    t,
    section("Pugilists")(
      kv(["Kazushi", "Sakuraba"], ["Jet", "Li"], ["Rory", "MacDonald"])
    )
  ),
  margin("[TypeScript!]")(
    row(
      box("Deploy Status")(
        layout(
          inlineBar("Build", 1.0),
          inlineBar("Test", 0.8),
          inlineBar("Deploy", 0.3)
        )
      ),
      tree("📁 Project")(tree("src")(tree("main.ts"), tree("api.ts")))
    )
  )
);

/* Get pretty strings w/ .render */
console.log(d.render());

console.log("\n🚀 Now the TypeScript API feels just like the Scala API!");
console.log("✅ Non-curried constructors");
console.log("✅ Fluent .border() methods");
console.log("✅ Automatic string-to-text conversion");
console.log("✅ Clean, readable syntax");
