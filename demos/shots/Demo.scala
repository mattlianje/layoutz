//> using scala 3.3.7
//> using file ../../layoutz/src/Layoutz.scala
import layoutz._

/* Define layouts */
val people = table(
  Seq("Name", "Role", "Status"),
  Seq(
    Seq("Alice", "Engineer", "Online"),
    Seq("Eve",   "QA",       "Away"),
    Seq(ul("Gegard", ul("Mousasi", ul("was a BAD man"))), "Fighter", "Nasty")
  )
).border(Border.Round)

val xs = (0 to 400).map(_ * 0.02)

/* Nest, compose, combine them */
val demo = vstack(
  row(
    underlineColored("^", Color.BrightMagenta)("Layoutz").style(Style.Bold),
    "... A Small Demo (ちいさい)"
  ).center(),
  row(
    statusCard("Users", "1.2K").color(Color.BrightBlue),
    statusCard("API", "UP").border(Border.Double).color(Color.BrightGreen),
    statusCard("CPU", "23%").border(Border.Thick).color(Color.BrightYellow),
    people,
    section("Pugilists")(vstack(
      kv("Kazushi" -> "Sakuraba", "Jet 李連杰" -> "Li", "Rory" -> "MacDonald"),
      tightRow((0 to 255 by 12).map { i =>
        val r = if (i < 128) i * 2 else 255
        val g = if (i < 128) 255 else (255 - i) * 2
        val b = if (i > 128) (i - 128) * 2 else 0
        "█".color(Color.True(r, g, b))
      }: _*)
    ))
  ),
  row(
    vstack(
      box("Wrapped")(wrap("Where there is a will ... Water x Necessaries", 20))
        .color(Color.BrightMagenta)
        .style(Style.Reverse ++ Style.Bold),
      ol("Arcole", "Rivoli", "Austerlitz", ol("Iéna", "Wagram", ol("Бородино")))
    ),
    margin("[Scala!]")(
      box("Deploy Status")(
        inlineBar("Build", 1.0),
        inlineBar("Test", 0.8),
        inlineBar("Deploy", 0.3)
      ).color(Color.Green),
      tree("📁 Project")(
        tree("src")(
          tree("main")(tree("App.scala")),
          tree("test")(tree("AppSpec.scala"))
        )
      ).color(Color.Cyan)
    ),
    box("Waves")(
      plot(width = 24, height = 6)(
        Series(xs.map(x => (x, math.sin(x))), "sin").color(Color.BrightCyan),
        Series(xs.map(x => (x, math.cos(x))), "cos").color(Color.BrightMagenta)
      )
    ).border(Border.Round)
  )
)

/* Get pretty strings w/ .render */
@main def run = println(demo.render)
