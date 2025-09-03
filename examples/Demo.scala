import layoutz._

object Demo {
  def main(args: Array[String]): Unit = {

    /* Define layouts */
    val tableElement = table(Border.Round)(
      headers = Seq("Name", "Role", "Status"),
      rows = Seq(
        Seq("Alice", "Engineer", "Online"),
        Seq("Bob", "Designer", "Offline"),
        Seq("Eve", "QA", "Away"),
        /* Nest and compose elements */
        Seq(
          ul("Gegard", ul("Mousasi", ul("was a BAD man"))),
          "Fighter",
          "Nasty"
        )
      )
    )

    /* Combine them */
    val demo = layout(
      center(row("layoutz", underline("ˆ")("DEMO"))),
      row(
        statusCard(Border.Thick)("API", "UP"),
        statusCard("Users", "1.2K"),
        statusCard(Border.Double)("CPU", "23%"),
        tableElement,
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
            tree("src", tree("main.scala"), tree("api.scala"))
          )
        )
      )
    )

    /* Get pretty strings w/ .render */
    println(demo.render)
  }
}
