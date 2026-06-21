import layoutz._

object ChartDemo extends LayoutzApp[Int, String] {

  def init: (Int, Cmd[String]) = (0, Cmd.none)

  def update(msg: String, tick: Int): (Int, Cmd[String]) = msg match {
    case "tick" => (tick + 1, Cmd.none)
    case _      => (tick, Cmd.none)
  }

  def subscriptions(tick: Int): Sub[String] =
    Sub.time.everyMs(100, "tick")

  def view(tick: Int): Element = {
    // Animated sin/cos — phase shifts with tick
    val sinPoints = (0 to 50).map { i =>
      (i.toDouble, 5 * math.sin(i * 0.15 + tick * 0.05))
    }
    val cosPoints = (0 to 50).map { i =>
      (i.toDouble, 5 * math.cos(i * 0.15 + tick * 0.05))
    }

    // Animated bar heights — oscillate around base values
    val barValues = Seq(
      ("Mon", 85.0, Color.BrightCyan),
      ("Tue", 120.0, Color.BrightGreen),
      ("Wed", 95.0, Color.BrightMagenta),
      ("Thu", 60.0, Color.BrightYellow),
      ("Fri", 110.0, Color.BrightBlue)
    ).zipWithIndex.map { case ((label, base, color), idx) =>
      val animated = base + 8 * math.sin(tick * 0.08 + idx * 0.9)
      Bar(animated, label).color(color)
    }

    // Scrolling sparkline
    val sparkData = (0 until 30).map { i =>
      math.sin((i + tick) * 0.3) * 10 + 15
    }.map(_.toDouble)

    // Animated heatmap — values shift
    val cols = Seq("6a", "9a", "12", "3p")
    val heatData = Seq(
      Seq("Mon", "Tue", "Wed").indices.flatMap { r =>
        cols.indices.map { c =>
          val base = (r + 1) * 20.0 + c * 15.0
          base + 10 * math.sin(tick * 0.06 + r + c * 0.5)
        }
      }
    ).head.grouped(cols.length).toSeq

    layout(
      row(
        layout(
          "Revenue Share".color(Color.BrightYellow).style(Style.Bold),
          pie(width = 28, height = 9)(
            Slice(45, "Product").color(Color.BrightCyan),
            Slice(30, "Services").color(Color.BrightMagenta),
            Slice(15, "Licensing").color(Color.BrightYellow),
            Slice(10, "Other").color(Color.BrightGreen)
          )
        ),
        layout(
          "Weekly Traffic".color(Color.BrightYellow).style(Style.Bold),
          bar(width = 28, height = 8)(barValues: _*)
        )
      ),
      br,
      row(
        layout(
          "sin(x) & cos(x)".color(Color.BrightYellow).style(Style.Bold),
          plot(width = 35, height = 10)(
            Series(sinPoints, "sin(x)").color(Color.BrightCyan),
            Series(cosPoints, "cos(x)").color(Color.BrightMagenta)
          )
        ),
        layout(
          "Signal".color(Color.BrightYellow).style(Style.Bold),
          sparkline(sparkData).color(Color.BrightCyan),
          br,
          "Activity".color(Color.BrightYellow).style(Style.Bold),
          heatmap(HeatmapData(
            heatData.map(_.map(_.toDouble)),
            rowLabels = Seq("Mon", "Tue", "Wed"),
            colLabels = cols
          ))
        )
      )
    )
  }
}
