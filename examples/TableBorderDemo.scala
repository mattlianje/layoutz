import layoutz._

object TableBorderDemo extends LayoutzApp[Int, String] {

  def init: (Int, Cmd[String]) = (0, Cmd.none)

  def update(msg: String, tick: Int): (Int, Cmd[String]) = msg match {
    case "tick" => (tick + 1, Cmd.none)
    case _      => (tick, Cmd.none)
  }

  def subscriptions(tick: Int): Sub[String] =
    Sub.time.everyMs(120, "tick")

  private val services = Seq(
    ("API Gateway", "LIVE", "12ms", "99.99%"),
    ("Database",    "LIVE", "3ms",  "99.95%"),
    ("Cache",       "WARN", "1ms",  "98.50%"),
    ("Queue",       "LIVE", "8ms",  "99.90%"),
    ("Auth",        "LIVE", "5ms",  "99.97%")
  )

  private val borderStyles: Seq[(String, Border, Color)] = Seq(
    ("Single", Border.Single, Color.BrightCyan),
    ("Double", Border.Double, Color.BrightMagenta),
    ("Round",  Border.Round,  Color.BrightGreen),
    ("Thick",  Border.Thick,  Color.BrightYellow),
    ("Dashed", Border.Dashed, Color.BrightBlue),
    ("Ascii",  Border.Ascii,  Color.BrightWhite)
  )

  def view(tick: Int): Element = {
    // Cycle which border style is highlighted
    val highlightIdx = (tick / 8) % borderStyles.length

    // Animate latency values slightly
    val tableRows = services.map { case (name, status, baseLatency, uptime) =>
      val baseMs = baseLatency.replace("ms", "").toInt
      val jitter = (math.sin(tick * 0.1 + name.hashCode) * 3).toInt
      val latency = s"${math.max(1, baseMs + jitter)}ms"
      val statusColor = if (status == "LIVE") Color.BrightGreen else Color.BrightYellow
      Seq[Element](
        Text(name),
        status.color(statusColor),
        latency,
        Text(uptime)
      )
    }

    val borderBoxes = borderStyles.zipWithIndex.map { case ((name, border, color), idx) =>
      val isHighlighted = idx == highlightIdx
      val style = if (isHighlighted) Style.Bold else Style.NoStyle
      box(name)(leftAlign("content", 8))
        .border(border)
        .color(color)
        .style(style)
    }

    layout(
      table(
        Seq[Element](
          "Service".style(Style.Bold),
          "Status".style(Style.Bold),
          "Latency".style(Style.Bold),
          "Uptime".style(Style.Bold)
        ),
        tableRows
      ).border(Border.Round),
      br,
      "Border Styles".style(Style.Bold).color(Color.BrightYellow),
      row(borderBoxes.take(3): _*),
      row(borderBoxes.drop(3): _*),
      br,
      row(
        statusCard("Users", "1.2K").color(Color.BrightBlue),
        statusCard("API", "UP").border(Border.Double).color(Color.BrightGreen),
        statusCard("CPU", s"${20 + (math.sin(tick * 0.05) * 8).toInt}%")
          .border(Border.Thick).color(Color.BrightYellow)
      ),
      br,
      tree("Project")(
        tree("src")(
          tree("main")(tree("App.scala"), tree("Config.scala")),
          tree("test")(tree("AppTest.scala"))
        ),
        tree("build.mill")
      ).color(Color.Cyan)
    )
  }
}
