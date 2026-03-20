import layoutz._

object LayoutShowcase extends LayoutzApp[Int, String] {

  def init: (Int, Cmd[String]) = (0, Cmd.none)

  def update(msg: String, tick: Int): (Int, Cmd[String]) = msg match {
    case "tick" => (tick + 1, Cmd.none)
    case _      => (tick, Cmd.none)
  }

  def subscriptions(tick: Int): Sub[String] =
    Sub.time.everyMs(100, "tick")

  def view(tick: Int): Element = {
    val t = table(
      Seq("Name", "Role", "Status"),
      Seq(
        Seq("Alice", "Engineer", "Online"),
        Seq("Eve", "QA", "Away"),
        Seq("Gegard", "Fighter", "Nasty")
      )
    ).border(Border.Round)

    // Animate progress bars
    val buildProg  = math.min(1.0, (tick * 0.02) % 1.4)
    val testProg   = math.min(1.0, math.max(0.0, ((tick * 0.02) - 0.3) % 1.4))
    val deployProg = math.min(1.0, math.max(0.0, ((tick * 0.02) - 0.6) % 1.4))

    // Scrolling gradient
    val gradientOffset = tick % 22
    val gradient = (0 to 255 by 12).zipWithIndex.map { case (i, idx) =>
      val shifted = (i + gradientOffset * 12) % 256
      val r = if (shifted < 128) shifted * 2 else 255
      val g = if (shifted < 128) 255 else (255 - shifted) * 2
      val b = if (shifted > 128) (shifted - 128) * 2 else 0
      "\u2588".color(Color.True(r, g, b))
    }

    layout(
      row(
        underlineColored("^", Color.BrightMagenta)("Layoutz").style(Style.Bold),
        "... A Small Demo"
      ).center(),
      row(
        statusCard("Users", "1.2K").color(Color.BrightBlue),
        statusCard("API", "UP").border(Border.Double).color(Color.BrightGreen),
        statusCard("CPU", s"${20 + (math.sin(tick * 0.06) * 8).toInt}%")
          .border(Border.Thick).color(Color.BrightYellow),
        t,
        section("Pugilists")(
          layout(
            kv("Kazushi" -> "Sakuraba", "Jet Li" -> "Li", "Rory" -> "MacDonald"),
            tightRow(gradient: _*)
          )
        )
      ),
      row(
        layout(
          box("Wrapped")(wrap("Where there is a will ... Water x Necessaries", 20))
            .color(Color.BrightMagenta).style(Style.Reverse ++ Style.Bold),
          ol("Arcole", "Austerlitz", ol("Iena", ol("Borodino")))
        ),
        margin("[Scala!]")(
          box("Deploy Status")(
            inlineBar("Build", buildProg),
            inlineBar("Test", testProg),
            inlineBar("Deploy", deployProg)
          ).color(Color.Green),
          tree("Project")(
            tree("src")(tree("main.scala"), tree("test.scala"))
          ).color(Color.Cyan)
        )
      )
    )
  }
}
