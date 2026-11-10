import layoutz._

object ColorDemo extends LayoutzApp[Int, String] {

  def init: (Int, Cmd[String]) = (0, Cmd.none)

  def update(msg: String, tick: Int): (Int, Cmd[String]) = msg match {
    case "tick" => (tick + 1, Cmd.none)
    case _      => (tick, Cmd.none)
  }

  def subscriptions(tick: Int): Sub[String] =
    Sub.time.everyMs(80, "tick")

  def view(tick: Int): Element = {
    // Scrolling 256-color palette
    val offset256 = tick % 72
    val palette256 = (0 until 72).map { i =>
      val idx = 16 + ((i + offset256) * 3) % 216
      "\u2588".color(Color.Full(idx))
    }

    // Animated RGB gradients — hue-shift over time
    val shift = (tick * 3) % 256
    val gradient1 = (0 to 255 by 4).map { i =>
      val h = (i + shift) % 256
      "\u2588".color(Color.True(255, h, 0))
    }
    val gradient2 = (0 to 255 by 4).map { i =>
      val h = (i + shift) % 256
      "\u2588".color(Color.True(0, 255 - h, h))
    }
    val gradient3 = (0 to 255 by 4).map { i =>
      val h = (i + shift) % 256
      val r = if (h < 128) h * 2 else 255
      val g = if (h < 128) 255 else (255 - h) * 2
      val b = if (h > 128) (h - 128) * 2 else 0
      "\u2588".color(Color.True(r, g, b))
    }

    // Cycle which style is highlighted
    val styles: Seq[(String, Style)] = Seq(
      ("Bold", Style.Bold),
      ("Italic", Style.Italic),
      ("Underline", Style.Underline),
      ("Dim", Style.Dim),
      ("Reverse", Style.Reverse),
      ("Strike", Style.Strikethrough)
    )
    val highlightStyle = (tick / 10) % styles.length

    val styledTexts = styles.zipWithIndex.map { case ((name, style), idx) =>
      val color = if (idx == highlightStyle) Color.BrightCyan else Color.White
      name.style(style).color(color)
    }

    layout(
      "Text Styles".style(Style.Bold).color(Color.BrightYellow),
      row(styledTexts: _*),
      br,
      "Combined Styles".style(Style.Bold).color(Color.BrightYellow),
      row(
        "Bold+Italic".style(Style.Bold ++ Style.Italic).color(Color.BrightCyan),
        "Bold+Underline".style(Style.Bold ++ Style.Underline).color(Color.BrightMagenta),
        "Reverse+Bold".style(Style.Reverse ++ Style.Bold).color(Color.BrightGreen)
      ),
      br,
      "Standard Colors".style(Style.Bold).color(Color.BrightYellow),
      row(
        "Red".color(Color.Red), "Green".color(Color.Green), "Blue".color(Color.Blue),
        "Yellow".color(Color.Yellow), "Magenta".color(Color.Magenta), "Cyan".color(Color.Cyan)
      ),
      row(
        "BrightRed".color(Color.BrightRed), "BrightGreen".color(Color.BrightGreen),
        "BrightBlue".color(Color.BrightBlue), "BrightYellow".color(Color.BrightYellow),
        "BrightMagenta".color(Color.BrightMagenta), "BrightCyan".color(Color.BrightCyan)
      ),
      br,
      "256-Color Palette".style(Style.Bold).color(Color.BrightYellow),
      tightRow(palette256: _*),
      br,
      "RGB Gradients".style(Style.Bold).color(Color.BrightYellow),
      tightRow(gradient1: _*),
      tightRow(gradient2: _*),
      tightRow(gradient3: _*),
      br,
      "Background Colors".style(Style.Bold).color(Color.BrightYellow),
      row(
        " Alert ".bg(Color.Red).style(Style.Bold),
        " Success ".bg(Color.Green).style(Style.Bold),
        " Info ".bg(Color.Blue).style(Style.Bold),
        " Warning ".bg(Color.Yellow).color(Color.Black).style(Style.Bold)
      )
    )
  }
}
