import layoutz._

case class ShowcaseState(
    scene: Int = 0,
    tick: Int = 0,
    textValue: String = "",
    items: List[String] = List.empty,
    addingItem: Boolean = false,
    addTick: Int = 0,
    selected: Set[Int] = Set.empty,
    cursor: Int = 0,
    lineOffset: Int = 0,
    tableRow: Int = 0,
    tableSelected: Set[Int] = Set.empty,
    barMode: Int = 0,
    ballY: Double = 10.0,
    ballVy: Double = 0.0,
    gravity: Int = 5,
    ballTrail: List[Double] = List.fill(80)(10.0)
)

sealed trait ShowcaseMsg
case object NextScene extends ShowcaseMsg
case object PrevScene extends ShowcaseMsg
case class GoScene(n: Int) extends ShowcaseMsg
case object Tick extends ShowcaseMsg
case class TypeChar(c: Char) extends ShowcaseMsg
case object Backspace extends ShowcaseMsg
case object SubmitItem extends ShowcaseMsg
case object ToggleSelect extends ShowcaseMsg
case object CursorUp extends ShowcaseMsg
case object CursorDown extends ShowcaseMsg
case object AdjustUp extends ShowcaseMsg
case object AdjustDown extends ShowcaseMsg
case object ToggleBarMode extends ShowcaseMsg
case object KickBall extends ShowcaseMsg

object ShowcaseApp extends LayoutzApp[ShowcaseState, ShowcaseMsg] {

  private val totalScenes = 7
  private val sceneNames = Seq(
    "Bouncing Ball",
    "Text Input & Lists",
    "Borders & Styles",
    "Tables",
    "Charts & Plots",
    "Bar Charts & Sparklines",
    "Selections & Heatmap"
  )

  private val services = Seq(
    ("API Gateway", "LIVE", "12ms", "99.99%"),
    ("Database",    "LIVE", "3ms",  "99.95%"),
    ("Cache",       "WARN", "1ms",  "98.50%"),
    ("Queue",       "LIVE", "8ms",  "99.90%"),
    ("Auth",        "LIVE", "5ms",  "99.97%"),
    ("CDN",         "LIVE", "2ms",  "99.99%")
  )

  def init: (ShowcaseState, Cmd[ShowcaseMsg]) =
    (ShowcaseState(), Cmd.setTitle("layoutz Showcase"))

  def update(msg: ShowcaseMsg, state: ShowcaseState): (ShowcaseState, Cmd[ShowcaseMsg]) =
    msg match {
      case NextScene =>
        (state.copy(scene = (state.scene + 1) % totalScenes), Cmd.none)
      case PrevScene =>
        (state.copy(scene = (state.scene - 1 + totalScenes) % totalScenes), Cmd.none)
      case GoScene(n) if n >= 0 && n < totalScenes =>
        (state.copy(scene = n), Cmd.none)
      case GoScene(_) => (state, Cmd.none)
      case Tick =>
        /* Text input adding animation */
        val s1 = if (state.addingItem && state.addTick >= 8) {
          state.copy(addingItem = false, addTick = 0,
            items = state.items :+ state.textValue, textValue = "")
        } else if (state.addingItem) {
          state.copy(addTick = state.addTick + 1)
        } else state
        /* Ball physics (always running) */
        val g = s1.gravity * 0.08
        val newVy = s1.ballVy - g
        var newY = s1.ballY + newVy * 0.3
        var vy = newVy
        if (newY <= 0) { newY = 0; vy = math.abs(vy) * 0.82 }
        if (newY > 12) { newY = 12; vy = -math.abs(vy) * 0.5 }
        if (math.abs(vy) < 0.05 && newY < 0.1) { newY = 0; vy = 0 }
        val trail = (s1.ballTrail :+ newY).takeRight(80)
        (s1.copy(tick = s1.tick + 1, ballY = newY, ballVy = vy, ballTrail = trail), Cmd.none)
      case TypeChar(c) if !state.addingItem =>
        (state.copy(textValue = state.textValue + c), Cmd.none)
      case TypeChar(_) => (state, Cmd.none)
      case Backspace if !state.addingItem =>
        (state.copy(textValue = state.textValue.dropRight(1)), Cmd.none)
      case Backspace => (state, Cmd.none)
      case SubmitItem if !state.addingItem && state.textValue.nonEmpty =>
        (state.copy(addingItem = true, addTick = 0), Cmd.none)
      case SubmitItem => (state, Cmd.none)
      case ToggleSelect =>
        if (state.scene == 3) {
          val s = state.tableSelected
          val newSel = if (s.contains(state.tableRow)) s - state.tableRow else s + state.tableRow
          (state.copy(tableSelected = newSel), Cmd.none)
        } else if (state.scene == 6) {
          val newSel = if (state.selected.contains(state.cursor))
            state.selected - state.cursor else state.selected + state.cursor
          (state.copy(selected = newSel), Cmd.none)
        } else (state, Cmd.none)
      case CursorUp =>
        if (state.scene == 3)
          (state.copy(tableRow = (state.tableRow - 1 + services.length) % services.length), Cmd.none)
        else if (state.scene == 6)
          (state.copy(cursor = (state.cursor - 1 + 7) % 7), Cmd.none)
        else (state, Cmd.none)
      case CursorDown =>
        if (state.scene == 3)
          (state.copy(tableRow = (state.tableRow + 1) % services.length), Cmd.none)
        else if (state.scene == 6)
          (state.copy(cursor = (state.cursor + 1) % 7), Cmd.none)
        else (state, Cmd.none)
      case AdjustUp =>
        if (state.scene == 0)
          (state.copy(gravity = math.min(state.gravity + 1, 15)), Cmd.none)
        else
          (state.copy(lineOffset = math.min(state.lineOffset + 1, 10)), Cmd.none)
      case AdjustDown =>
        if (state.scene == 0)
          (state.copy(gravity = math.max(state.gravity - 1, 1)), Cmd.none)
        else
          (state.copy(lineOffset = math.max(state.lineOffset - 1, -10)), Cmd.none)
      case ToggleBarMode =>
        (state.copy(barMode = (state.barMode + 1) % 2), Cmd.none)
      case KickBall =>
        (state.copy(ballVy = 5.0), Cmd.none)
    }

  def subscriptions(state: ShowcaseState): Sub[ShowcaseMsg] = Sub.batch(
    Sub.time.everyMs(80, Tick),
    Sub.onKeyPress {
      case Key.Right     => Some(NextScene)
      case Key.Left      => Some(PrevScene)
      case Key.Char('+') => Some(AdjustUp)
      case Key.Char('-') => Some(AdjustDown)
      case Key.Char(' ') if state.scene == 0 => Some(KickBall)
      case Key.Char(' ') if state.scene == 3 || state.scene == 6 => Some(ToggleSelect)
      case Key.Tab if state.scene == 5       => Some(ToggleBarMode)
      case Key.Enter if state.scene == 1     => Some(SubmitItem)
      case Key.Up        => Some(CursorUp)
      case Key.Down      => Some(CursorDown)
      case Key.Backspace => Some(Backspace)
      case Key.Char(c) if state.scene == 1 && (c.isLetterOrDigit || c == ' ') =>
        Some(TypeChar(c))
      case Key.Char(c) if c >= '1' && c <= '7' =>
        Some(GoScene(c - '1'))
      case _ => None
    }
  )

  private val sceneWidth = 75

  override def view(state: ShowcaseState): Element = {
    val header = renderHeader(state)
    val content = state.scene match {
      case 0 => sceneBouncingBall(state)
      case 1 => sceneTextInput(state)
      case 2 => sceneBordersStyles(state)
      case 3 => sceneTables(state)
      case 4 => sceneChartsPlots(state)
      case 5 => sceneBarChartsSparklines(state)
      case 6 => sceneSelectionsHeatmap(state)
      case _ => layout("Unknown scene")
    }
    val footer = renderFooter(state)
    leftAlign(layout(header, br, content, br, footer), sceneWidth)
  }

  private def renderHeader(state: ShowcaseState): Element = {
    val sceneDots = (0 until totalScenes).map { i =>
      if (i == state.scene) "●" else "○"
    }.mkString(" ")

    val prefix = " ─── layoutz "
    val suffix = s" ${state.scene + 1} / $totalScenes"
    val dashCount = math.max(3, sceneWidth - prefix.length - suffix.length)
    val dashes = "─" * dashCount

    layout(
      br,
      rowTight(
        " ─── ".color(Color.BrightBlack),
        "layoutz".style(Style.Bold).color(Color.BrightCyan),
        s" $dashes ".color(Color.BrightBlack),
        s"${state.scene + 1} / $totalScenes".color(Color.BrightBlack)
      ),
      br,
      s" ${sceneNames(state.scene)}".style(Style.Bold).color(Color.BrightYellow),
      s" $sceneDots"
    )
  }

  private def renderFooter(state: ShowcaseState): Element = {
    val hints = state.scene match {
      case 0 => "←/→ scenes  Space kick  +/- gravity  Ctrl+Q quit"
      case 1 => "←/→ scenes  type + Enter to add  Ctrl+Q quit"
      case 3 => "←/→ scenes  ↑/↓ navigate  Space select  Ctrl+Q quit"
      case 4 => "←/→ scenes  +/- move threshold  Ctrl+Q quit"
      case 5 => "←/→ scenes  Tab cycle chart mode  Ctrl+Q quit"
      case 6 => "←/→ scenes  ↑/↓ navigate  Space toggle  Ctrl+Q quit"
      case _ => "←/→ scenes  Ctrl+Q quit"
    }
    hints.color(Color.BrightBlack).style(Style.Dim)
  }

  /* -- Scene 1: Bouncing Ball -- */
  private def sceneBouncingBall(state: ShowcaseState): Element = {
    val trailPoints = state.ballTrail.zipWithIndex.map { case (y, i) =>
      (i.toDouble, y)
    }

    val gLabel = f"g = ${state.gravity * 0.08}%.2f"
    val velLabel = f"vy = ${state.ballVy}%.1f"
    val yLabel = f"y = ${state.ballY}%.1f"

    /* Anchor points pin the y-axis to 0..12 so the scale never shifts */
    val bounds = Seq((0.0, 0.0), (0.0, 12.0))

    columns(
      layout(
        "Trajectory".color(Color.BrightYellow),
        plot(width = 35, height = 12)(
          Series(trailPoints, "ball").color(Color.BrightCyan),
          Series(bounds, " ").color(Color.BrightBlack)
        )
      ),
      box("Physics")(
        leftAlign(layout(
          kv(
            "gravity" -> gLabel,
            "velocity" -> velLabel,
            "height" -> yLabel
          ),
          br,
          {
            val energy = math.min(1.0, (math.abs(state.ballVy) + state.ballY) / 15.0)
            val barW = 14
            val filled = (energy * barW).toInt
            val pct = (energy * 100).toInt
            s"Energy ${"█" * filled}${"░" * (barW - filled)} $pct%"
          }.color(Color.BrightGreen),
          br,
          spinner("Simulating", state.tick, SpinnerStyle.Earth).color(Color.BrightCyan),
          "Space to kick!".color(Color.BrightYellow).style(Style.Bold)
        ), 28)
      ).border(Border.Round).color(Color.BrightMagenta)
    )
  }

  /* -- Scene 2: Text Input & Lists -- */
  private def sceneTextInput(state: ShowcaseState): Element = {
    val inputLine = if (state.addingItem) {
      row(
        spinner("Adding", state.addTick, SpinnerStyle.Dots).color(Color.BrightYellow),
        Text(s"  \"${state.textValue}\"").color(Color.BrightYellow)
      )
    } else {
      val cursor = "_".style(Style.Blink)
      val display = if (state.textValue.isEmpty)
        "Type something...".color(Color.BrightBlack)
      else
        Text(state.textValue).color(Color.BrightWhite)
      rowTight("> ".color(Color.BrightCyan), display, cursor)
    }

    val itemList = if (state.items.isEmpty) {
      "  (no items yet)".color(Color.BrightBlack)
    } else {
      val itemElements: Seq[Element] = state.items.zipWithIndex.map { case (item, i) =>
        val color = Seq(Color.BrightGreen, Color.BrightBlue, Color.BrightMagenta, Color.BrightYellow, Color.BrightCyan)
        rowTight(
          s"  ${i + 1}. ".color(Color.BrightBlack),
          item.color(color(i % color.length))
        )
      }
      Layout(itemElements)
    }

    val boxW = 32

    columns(
      box("Add Items")(
        leftAlign(inputLine, boxW),
        br,
        leftAlign(layout(
          "Items:".style(Style.Bold),
          itemList
        ), boxW)
      ).border(Border.Round).color(Color.BrightCyan),
      box("Stats")(
        leftAlign(layout(
          rowTight("Total items: ", state.items.length.toString.color(Color.BrightCyan).style(Style.Bold)),
          rowTight("Longest:     ", (if (state.items.isEmpty) "-" else state.items.maxBy(_.length)).color(Color.BrightMagenta)),
          rowTight("Shortest:    ", (if (state.items.isEmpty) "-" else state.items.minBy(_.length)).color(Color.BrightMagenta))
        ), boxW),
        br,
        leftAlign(
          if (state.items.length >= 3)
            "Nice collection!".color(Color.BrightGreen).style(Style.Bold)
          else
            s"Add ${3 - state.items.length} more...".color(Color.BrightBlack),
          boxW
        )
      ).border(Border.Round)
    )
  }

  /* -- Scene 3: Borders & Styles -- */
  private def sceneBordersStyles(state: ShowcaseState): Element = {
    val borderStyles: Seq[(Border, String, Color)] = Seq(
      (Border.Single, "Single", Color.BrightCyan),
      (Border.Double, "Double", Color.BrightMagenta),
      (Border.Round, "Round", Color.BrightGreen),
      (Border.Thick, "Thick", Color.BrightYellow),
      (Border.Dashed, "Dashed", Color.BrightBlue),
      (Border.Ascii, "Ascii", Color.BrightWhite)
    )

    val (topRow, bottomRow) = borderStyles.splitAt(3)
    val borderRow1 = row(topRow.map { case (style, name, c) =>
      box(name)(leftAlign(name, 8)).border(style).color(c)
    }: _*)
    val borderRow2 = row(bottomRow.map { case (style, name, c) =>
      box(name)(leftAlign(name, 8)).border(style).color(c)
    }: _*)

    layout(
      "Border Styles".style(Style.Bold).color(Color.BrightYellow),
      borderRow1,
      borderRow2,
      br,
      "Text Styles".style(Style.Bold).color(Color.BrightYellow),
      row(
        box("Standard")(
          "Bold".style(Style.Bold).color(Color.BrightCyan),
          "Italic".style(Style.Italic).color(Color.BrightMagenta),
          "Underline".style(Style.Underline).color(Color.BrightGreen)
        ).border(Border.Round),
        box("Extended")(
          "Dim".style(Style.Dim).color(Color.BrightYellow),
          "Strikethrough".style(Style.Strikethrough).color(Color.BrightRed),
          "Bold+Italic".style(Style.Bold ++ Style.Italic).color(Color.BrightWhite)
        ).border(Border.Round)
      )
    )
  }

  /* -- Scene 4: Tables -- */
  private def sceneTables(state: ShowcaseState): Element = {
    val coloredRows: Seq[Seq[Element]] = services.zipWithIndex.map { case ((name, status, lat, up), idx) =>
      val isActive = idx == state.tableRow
      val isSel = state.tableSelected.contains(idx)
      val mark = if (isSel) "* " else "  "
      val cells = Seq(s"$mark$name", status, lat, up)
      cells.map { cell =>
        if (isActive && isSel)
          cell.color(Color.BrightGreen).style(Style.Bold ++ Style.Reverse)
        else if (isActive)
          cell.color(Color.BrightCyan).style(Style.Bold ++ Style.Reverse)
        else if (isSel)
          cell.color(Color.BrightGreen)
        else
          Text(cell): Element
      }
    }

    val selCount = state.tableSelected.size
    val selInfo = if (selCount > 0)
      s"$selCount selected".color(Color.BrightGreen)
    else
      "none selected".color(Color.BrightBlack)

    layout(
      table(
        Seq[Element](
          "Service".style(Style.Bold),
          "Status".style(Style.Bold),
          "Latency".style(Style.Bold),
          "Uptime".style(Style.Bold)
        ),
        coloredRows
      ).border(Border.Round),
      rowTight(
        s" Row ${state.tableRow + 1}/${services.length}  |  ".color(Color.BrightBlack),
        selInfo
      )
    )
  }

  /* -- Scene 5: Charts & Plots -- */
  private def sceneChartsPlots(state: ShowcaseState): Element = {
    val sinPoints = (0 to 100).map { i =>
      val x = i * 0.08
      (x, math.sin(x + state.tick * 0.06) * 4)
    }

    val intercept = state.lineOffset * 0.5
    val linePoints = (0 to 100).map { i =>
      val x = i * 0.08
      (x, 0.5 * x + intercept)
    }

    val sign = if (intercept >= 0) "+" else "-"
    val lineLabel = f"0.5x $sign ${math.abs(intercept)}%.1f"

    columns(
      layout(
        s"sin(x) & y = $lineLabel  [+/- to shift]".color(Color.BrightYellow),
        plot(width = 35, height = 12)(
          Series(sinPoints, "sin(x)").color(Color.BrightCyan),
          Series(linePoints, "linear").color(Color.BrightYellow)
        )
      ),
      layout(
        "Revenue Share".color(Color.BrightYellow),
        pie(width = 30, height = 8)(
          Slice(45, "Product").color(Color.BrightCyan),
          Slice(30, "Services").color(Color.BrightMagenta),
          Slice(15, "Licensing").color(Color.BrightYellow),
          Slice(10, "Other").color(Color.BrightGreen)
        )
      )
    )
  }

  /* -- Scene 6: Bar Charts & Sparklines -- */
  private def sceneBarChartsSparklines(state: ShowcaseState): Element = {
    val sparkData = (0 until 30).map(i =>
      math.sin((i + state.tick) * 0.3) * 10 + 15
    ).map(_.toDouble)

    val modeName = if (state.barMode == 0) "Vertical Bars" else "Stacked Bars"

    val chartElement: Element = state.barMode match {
      case 0 =>
        bar(width = 30, height = 8)(
          Bar(85, "Mon").color(Color.BrightCyan),
          Bar(120, "Tue").color(Color.BrightGreen),
          Bar(95, "Wed").color(Color.BrightMagenta),
          Bar(110, "Thu").color(Color.BrightYellow),
          Bar(75, "Fri").color(Color.BrightBlue)
        )
      case _ =>
        stackedBar(width = 30, height = 8)(
          StackedBar(Seq(
            Bar(50, "Online").color(Color.BrightCyan),
            Bar(35, "Retail").color(Color.BrightGreen),
            Bar(15, "Other").color(Color.BrightMagenta)
          ), "Q1"),
          StackedBar(Seq(
            Bar(70, "Online").color(Color.BrightCyan),
            Bar(30, "Retail").color(Color.BrightGreen),
            Bar(20, "Other").color(Color.BrightMagenta)
          ), "Q2"),
          StackedBar(Seq(
            Bar(45, "Online").color(Color.BrightCyan),
            Bar(55, "Retail").color(Color.BrightGreen),
            Bar(10, "Other").color(Color.BrightMagenta)
          ), "Q3"),
          StackedBar(Seq(
            Bar(60, "Online").color(Color.BrightCyan),
            Bar(40, "Retail").color(Color.BrightGreen),
            Bar(25, "Other").color(Color.BrightMagenta)
          ), "Q4")
        )
    }

    columns(
      layout(
        "Live Signal".color(Color.BrightYellow),
        sparkline(sparkData).color(Color.BrightCyan)
      ),
      layout(
        s"$modeName  [Tab to cycle]".color(Color.BrightYellow),
        chartElement
      )
    )
  }

  /* -- Scene 7: Selections & Heatmap -- */
  private def sceneSelectionsHeatmap(state: ShowcaseState): Element = {
    val days = Seq("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    val hours = Seq("6am", "9am", "12pm", "3pm", "6pm", "9pm")

    val selectorLines: Seq[Element] = days.zipWithIndex.map { case (day, idx) =>
      val isSelected = state.selected.contains(idx)
      val isCursor = state.cursor == idx
      val check = if (isSelected) "[x]" else "[ ]"
      val arrow = if (isCursor) "> " else "  "
      val label = s"$arrow$check $day"
      if (isCursor && isSelected)
        label.color(Color.BrightGreen).style(Style.Bold)
      else if (isCursor)
        label.color(Color.BrightCyan).style(Style.Bold)
      else if (isSelected)
        label.color(Color.BrightGreen)
      else
        Text(label)
    }

    val selCount = state.selected.size

    /* Static realistic weekly activity pattern — no dependency on selections */
    val heatData = Seq(
      Seq(10.0, 45.0, 80.0, 75.0, 50.0, 15.0),  // Mon
      Seq(12.0, 50.0, 85.0, 70.0, 55.0, 20.0),  // Tue
      Seq( 8.0, 40.0, 90.0, 80.0, 60.0, 25.0),  // Wed
      Seq(15.0, 55.0, 75.0, 65.0, 45.0, 18.0),  // Thu
      Seq(10.0, 48.0, 70.0, 60.0, 35.0, 30.0),  // Fri
      Seq( 5.0, 15.0, 25.0, 30.0, 40.0, 55.0),  // Sat
      Seq( 3.0, 10.0, 20.0, 25.0, 35.0, 45.0)   // Sun
    )

    columns(
      box("Schedule")(
        Layout(selectorLines),
        br,
        s"$selCount of ${days.length} active".color(if (selCount > 0) Color.BrightGreen else Color.BrightBlack)
      ).border(Border.Round).color(Color.BrightCyan),
      box("Weekly Activity")(
        Heatmap(
          HeatmapData(
            rows = heatData,
            rowLabels = days,
            colLabels = hours
          ),
          cellWidth = 5
        )
      ).border(Border.Round)
    )
  }
}

