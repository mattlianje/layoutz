#!/usr/bin/env ts-node

import {
  layout,
  section,
  kv,
  ul,
  ol,
  box,
  row,
  table,
  center,
  underline,
  underlineColored,
  statusCard,
  margin,
  inlineBar,
  tree,
  wrap,
  Border,
  Color,
  Style,
} from "./layoutz";

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
  center(
    row(
      underlineColored("^", Color.BrightMagenta)("Layoutz").style(Style.Bold),
      "... A Small Demo (ちいさい)"
    )
  ),
  row(
    statusCard("Users", "1.2K").color(Color.BrightBlue),
    statusCard("API", "UP").border(Border.Double).color(Color.BrightGreen),
    statusCard("CPU", "23%").border(Border.Thick).color(Color.BrightYellow),
    t,
    section("Pugilists")(
      kv(["Kazushi", "Sakuraba"], ["Jet 李連杰", "Li"], ["Rory", "MacDonald"])
    )
  ),
  row(
    layout(
      box("Wrapped")(wrap(20)("Where there is a will ... Water x Necessaries")),
      ol("Arcole", "Austerlitz", ol("Iéna", ol("Бородино")))
    ),
    margin("[TypeScript!]")(
      box("Deploy Status")(
        layout(
          inlineBar("Build", 1.0),
          inlineBar("Test", 0.8),
          inlineBar("Deploy", 0.3)
        )
      ).color(Color.Green),
      tree("📁 Project")(tree("src")(tree("main.ts"), tree("test.ts"))).color(
        Color.Cyan
      )
    )
  )
);

/* Get pretty strings w/ .render */
console.log(d.render());
