#!/usr/bin/env ts-node

import {
  layout,
  section,
  kv,
  ul,
  ol,
  box,
  row,
  tightRow,
  table,
  center,
  underline,
  underlineColored,
  statusCard,
  margin,
  inlineBar,
  tree,
  wrap,
  text,
  Border,
  Color,
  Style,
  styles,
  colorTrue,
} from "./layoutz";

/*** Define layouts ***/
const t = table(
  ["Name", "Role", "Status"],
  [
    ["Alice", "Engineer", "Online"],
    ["Eve", "QA", "Away"],
    [ul("Gegard", ul("Mousasi", ul("was a BAD man"))), "Fighter", "Nasty"],
  ]
).border(Border.Round);
const gradient = Array.from({ length: 22 }, (_, idx) => {
  const i = idx * 12;
  const r = i < 128 ? i * 2 : 255;
  const g = i < 128 ? 255 : (255 - i) * 2;
  const b = i > 128 ? (i - 128) * 2 : 0; return text("█").color(colorTrue(r, g, b));
});
/*** Nest, compose, combine them ***/
const d = layout(
  center(
    row(
      underlineColored("^", Color.BrightMagenta)("Layoutz").styles(Style.Bold),
      "... A Small Demo (ちいさい)"
    )
  ),
  row(
    statusCard("Users", "1.2K").color(Color.BrightBlue),
    statusCard("API", "UP").border(Border.Double).color(Color.BrightGreen),
    statusCard("CPU", "23%").border(Border.Thick).color(Color.BrightYellow),
    t,
    section("Pugilists")(
      layout(
        kv(["Kazushi", "Sakuraba"], ["Jet 李連杰", "Li"], ["Rory", "MacDonald"]),
        tightRow(...gradient)
      )
    )
  ),
  row(
    layout(
      box("Wrapped")(wrap(20)("Where there is a will ... Water x Necessaries"))
        .color(Color.BrightMagenta)
        .styles(Style.Reverse, Style.Bold),
      ol("Arcole", "Austerlitz", ol("Iéna", ol("Бородино")))
    ),
    margin("[TypeScript!]")(
      box("Deploy Status")(
        layout(
          inlineBar("Build", 1.0), inlineBar("Test", 0.8), inlineBar("Deploy", 0.3)
        )
      ).color(Color.Green),
      tree("📁 Project")(
        tree("src")(tree("main.ts"), tree("test.ts"))
      ).color(Color.Cyan)
    )
  )
);
/*** Get pretty strings w/ .render ***/
console.log(d.render());
