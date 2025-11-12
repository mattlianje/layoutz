import layoutz._

object ReadmeDemo {

  /** * Define layouts **
    */
  val t = table(
    Seq("Name", "Role", "Status"),
    Seq(
      Seq("Alice", "Engineer", "Online"),
      Seq("Eve", "QA", "Away"),
      Seq(ul("Gegard", ul("Mousasi", ul("was a BAD man"))), "Fighter", "Nasty")
    )
  ).border(Border.Round)

  /** * Nest, compose, combine them **
    */
  val d = layout(
    row(
      underlineColored("^", Color.BrightMagenta)("Layoutz").style(Style.Bold),
      "... A Small Demo (ちいさい)"
    ).center(),
    row(
      statusCard("Users", "1.2K").color(Color.BrightBlue),
      statusCard("API", "UP").border(Border.Double).color(Color.BrightGreen),
      statusCard("CPU", "23%").border(Border.Thick).color(Color.BrightYellow),
      t,
      section("Pugilists")(
        kv("Kazushi" -> "Sakuraba", "Jet 李連杰" -> "Li", "Rory" -> "MacDonald")
      )
    ),
    row(
      layout(
        box("Wrapped")(
          wrap("Where there is a will ... Water x Necessaries", 20)
        )
          .color(Color.BrightMagenta)
          .style(Style.Reverse ++ Style.Bold),
        ol("Arcole", "Austerlitz", ol("Iéna", ol("Бородино")))
      ),
      margin("[Scala!]")(
        box("Deploy Status")(
          inlineBar("Build", 1.0),
          inlineBar("Test", 0.8),
          inlineBar("Deploy", 0.3)
        ).color(Color.Green),
        tree("📁 Project")(
          tree("src")(
            tree("main.scala"),
            tree("test.scala")
          )
        ).color(Color.Cyan)
      )
    )
  )

  /** * Get pretty strings w/ .render **
    */
  def main(args: Array[String]): Unit = {
    println(d.render)
  }
}
