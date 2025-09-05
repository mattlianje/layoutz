import layoutz._

object ReadmeDemo {
/* Define layouts */
val t = table(Border.Round)(
  Seq("Name", "Role", "Status"),
  Seq(
    Seq("Alice", "Engineer", "Online"),
    Seq("Eve", "QA", "Away"),
    Seq(ul("Gegard", ul("Mousasi", ul("was a BAD man"))), "Fighter", "Nasty")
  )
)

/* Nest, compose, combine them */
val d = layout(
  center(row("Layoutz", underline("ˆ")("DEMO"))),
  row(
    statusCard("Users", "1.2K"),
    statusCard(Border.Double)("API", "UP"),
    statusCard(Border.Thick)("CPU", "23%"),
    t,
    section("Pugilists")(
      kv("Kazushi" -> "Sakuraba", "Jet" -> "Li", "Rory" -> "MacDonald")
    )
  ),
  margin("[Scala!]")(
    row(
      box("Deploy Status")(
        layout(
          inlineBar("Build", 1.0),
          inlineBar("Test", 0.8),
          inlineBar("Deploy", 0.3)
        )
      ),
      tree("📁 Project")(
        tree("src")(tree("main.scala"), tree("api.scala"))
      )
    )
  )
)

/* Get pretty strings w/ .render */
println(d.render)
}
