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
    ballTrail: List[Double] = List.fill(80)(10.0),
    rayTheta: Double = 0.6,
    rayPhi: Double = 0.35,
    rayDist: Double = 3.8,
    rayMorph: Double = 0.0,
    rayMorphTarget: Double = 0.0
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
case object RayRotL extends ShowcaseMsg
case object RayRotR extends ShowcaseMsg
case object RayRotU extends ShowcaseMsg
case object RayRotD extends ShowcaseMsg
case object RayNextShape extends ShowcaseMsg

object ShowcaseApp extends LayoutzApp[ShowcaseState, ShowcaseMsg] {

  private val totalScenes = 8
  private val sceneNames = Seq(
    "Ray Marcher",
    "Physics Game",
    "Text Input & Lists",
    "Borders & Styles",
    "Tables",
    "Charts & Plots",
    "Bar Charts & Sparklines",
    "Selections & Heatmap"
  )

  private val services = Seq(
    ("API Gateway", "LIVE", "12ms", "99.99%"),
    ("Database", "LIVE", "3ms", "99.95%"),
    ("Cache", "WARN", "1ms", "98.50%"),
    ("Queue", "LIVE", "8ms", "99.90%"),
    ("Auth", "LIVE", "5ms", "99.97%"),
    ("CDN", "LIVE", "2ms", "99.99%")
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
      case Tick       =>
        /* Text input adding animation */
        val s1 = if (state.addingItem && state.addTick >= 8) {
          state.copy(
            addingItem = false,
            addTick = 0,
            items = state.items :+ state.textValue,
            textValue = ""
          )
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
        /* Ray marcher shape morph (eases toward the selected shape) */
        val mDiff = s1.rayMorphTarget - s1.rayMorph
        val newMorph =
          if (math.abs(mDiff) < 0.01) s1.rayMorphTarget else s1.rayMorph + mDiff * 0.06
        (
          s1.copy(
            tick = s1.tick + 1,
            ballY = newY,
            ballVy = vy,
            ballTrail = trail,
            rayMorph = newMorph
          ),
          Cmd.none
        )
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
        if (state.scene == 4) {
          val s = state.tableSelected
          val newSel = if (s.contains(state.tableRow)) s - state.tableRow else s + state.tableRow
          (state.copy(tableSelected = newSel), Cmd.none)
        } else if (state.scene == 7) {
          val newSel = if (state.selected.contains(state.cursor))
            state.selected - state.cursor
          else state.selected + state.cursor
          (state.copy(selected = newSel), Cmd.none)
        } else (state, Cmd.none)
      case CursorUp =>
        if (state.scene == 4)
          (
            state.copy(tableRow = (state.tableRow - 1 + services.length) % services.length),
            Cmd.none
          )
        else if (state.scene == 7)
          (state.copy(cursor = (state.cursor - 1 + 7) % 7), Cmd.none)
        else (state, Cmd.none)
      case CursorDown =>
        if (state.scene == 4)
          (state.copy(tableRow = (state.tableRow + 1) % services.length), Cmd.none)
        else if (state.scene == 7)
          (state.copy(cursor = (state.cursor + 1) % 7), Cmd.none)
        else (state, Cmd.none)
      case AdjustUp =>
        if (state.scene == 1)
          (state.copy(gravity = math.min(state.gravity + 1, 15)), Cmd.none)
        else if (state.scene == 0)
          (state.copy(rayDist = math.max(state.rayDist - 0.25, 2.0)), Cmd.none)
        else
          (state.copy(lineOffset = math.min(state.lineOffset + 1, 10)), Cmd.none)
      case AdjustDown =>
        if (state.scene == 1)
          (state.copy(gravity = math.max(state.gravity - 1, 1)), Cmd.none)
        else if (state.scene == 0)
          (state.copy(rayDist = math.min(state.rayDist + 0.25, 8.0)), Cmd.none)
        else
          (state.copy(lineOffset = math.max(state.lineOffset - 1, -10)), Cmd.none)
      case ToggleBarMode =>
        (state.copy(barMode = (state.barMode + 1) % 2), Cmd.none)
      case KickBall =>
        (state.copy(ballVy = 5.0), Cmd.none)
      case RayRotL => (state.copy(rayTheta = state.rayTheta - 0.15), Cmd.none)
      case RayRotR => (state.copy(rayTheta = state.rayTheta + 0.15), Cmd.none)
      case RayRotU => (state.copy(rayPhi = math.min(state.rayPhi + 0.1, 1.3)), Cmd.none)
      case RayRotD => (state.copy(rayPhi = math.max(state.rayPhi - 0.1, -1.3)), Cmd.none)
      case RayNextShape =>
        val next = (math.round(state.rayMorphTarget).toInt + 1) % 2
        (state.copy(rayMorphTarget = next.toDouble), Cmd.none)
    }

  def subscriptions(state: ShowcaseState): Sub[ShowcaseMsg] = Sub.batch(
    Sub.time.everyMs(80, Tick),
    Sub.onKeyPress {
      /* Ray Marcher scene: WASD orbits, Space switches shape, arrows still change scenes */
      case Key.Char('a') if state.scene == 0                     => Some(RayRotL)
      case Key.Char('d') if state.scene == 0                     => Some(RayRotR)
      case Key.Char('w') if state.scene == 0                     => Some(RayRotU)
      case Key.Char('s') if state.scene == 0                     => Some(RayRotD)
      case Key.Char(' ') if state.scene == 0                     => Some(RayNextShape)
      case Key.Right                                             => Some(NextScene)
      case Key.Left                                              => Some(PrevScene)
      case Key.Char('+')                                         => Some(AdjustUp)
      case Key.Char('-')                                         => Some(AdjustDown)
      case Key.Char(' ') if state.scene == 1                     => Some(KickBall)
      case Key.Char(' ') if state.scene == 4 || state.scene == 7 => Some(ToggleSelect)
      case Key.Tab if state.scene == 6                           => Some(ToggleBarMode)
      case Key.Enter if state.scene == 2                         => Some(SubmitItem)
      case Key.Up                                                => Some(CursorUp)
      case Key.Down                                              => Some(CursorDown)
      case Key.Backspace                                         => Some(Backspace)
      case Key.Char(c) if state.scene == 2 && (c.isLetterOrDigit || c == ' ') =>
        Some(TypeChar(c))
      case Key.Char(c) if c >= '1' && c <= '8' =>
        Some(GoScene(c - '1'))
      case _ => None
    }
  )

  private val sceneWidth = 75

  override def view(state: ShowcaseState): Element = {
    val header = renderHeader(state)
    val content = state.scene match {
      case 0 => sceneRayMarcher(state)
      case 1 => scenePhysicsGame(state)
      case 2 => sceneTextInput(state)
      case 3 => sceneBordersStyles(state)
      case 4 => sceneTables(state)
      case 5 => sceneChartsPlots(state)
      case 6 => sceneBarChartsSparklines(state)
      case 7 => sceneSelectionsHeatmap(state)
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
      case 0 => "←/→ scenes  WASD orbit  +/- zoom  Space shape  Ctrl+Q quit"
      case 1 => "←/→ scenes  Space kick  +/- gravity  Ctrl+Q quit"
      case 2 => "←/→ scenes  type + Enter to add  Ctrl+Q quit"
      case 4 => "←/→ scenes  ↑/↓ navigate  Space select  Ctrl+Q quit"
      case 5 => "←/→ scenes  +/- move threshold  Ctrl+Q quit"
      case 6 => "←/→ scenes  Tab cycle chart mode  Ctrl+Q quit"
      case 7 => "←/→ scenes  ↑/↓ navigate  Space toggle  Ctrl+Q quit"
      case _ => "←/→ scenes  Ctrl+Q quit"
    }
    hints.color(Color.BrightBlack).style(Style.Dim)
  }

  /* -- Scene 2: Physics Game -- */
  private def scenePhysicsGame(state: ShowcaseState): Element = {
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
        leftAlign(
          layout(
            kv(
              "gravity"  -> gLabel,
              "velocity" -> velLabel,
              "height"   -> yLabel
            ),
            br, {
              val energy = math.min(1.0, (math.abs(state.ballVy) + state.ballY) / 15.0)
              val barW = 14
              val filled = (energy * barW).toInt
              val pct = (energy * 100).toInt
              s"Energy ${"█" * filled}${"░" * (barW - filled)} $pct%"
            }.color(Color.BrightGreen),
            br,
            spinner("Simulating", state.tick / 3, SpinnerStyle.Earth).color(Color.BrightCyan),
            "Press Space to kick ball!".color(Color.BrightYellow).style(Style.Bold)
          ),
          28
        )
      ).border(Border.Round).color(Color.BrightMagenta)
    )
  }

  /* -- Scene 3: Text Input & Lists -- */
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
        val color = Seq(
          Color.BrightGreen,
          Color.BrightBlue,
          Color.BrightMagenta,
          Color.BrightYellow,
          Color.BrightCyan
        )
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
        leftAlign(
          layout(
            "Items:".style(Style.Bold),
            itemList
          ),
          boxW
        )
      ).border(Border.Round).color(Color.BrightCyan),
      box("Stats")(
        leftAlign(
          layout(
            rowTight(
              "Total items: ",
              state.items.length.toString.color(Color.BrightCyan).style(Style.Bold)
            ),
            rowTight(
              "Longest:     ",
              (if (state.items.isEmpty) "-" else state.items.maxBy(_.length)).color(
                Color.BrightMagenta
              )
            ),
            rowTight(
              "Shortest:    ",
              (if (state.items.isEmpty) "-" else state.items.minBy(_.length)).color(
                Color.BrightMagenta
              )
            )
          ),
          boxW
        ),
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

  /* -- Scene 4: Borders & Styles -- */
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

  /* -- Scene 5: Tables -- */
  private def sceneTables(state: ShowcaseState): Element = {
    val coloredRows: Seq[Seq[Element]] =
      services.zipWithIndex.map { case ((name, status, lat, up), idx) =>
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

  /* -- Scene 6: Charts & Plots -- */
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

  /* -- Scene 7: Bar Charts & Sparklines -- */
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
          StackedBar(
            Seq(
              Bar(50, "Online").color(Color.BrightCyan),
              Bar(35, "Retail").color(Color.BrightGreen),
              Bar(15, "Other").color(Color.BrightMagenta)
            ),
            "Q1"
          ),
          StackedBar(
            Seq(
              Bar(70, "Online").color(Color.BrightCyan),
              Bar(30, "Retail").color(Color.BrightGreen),
              Bar(20, "Other").color(Color.BrightMagenta)
            ),
            "Q2"
          ),
          StackedBar(
            Seq(
              Bar(45, "Online").color(Color.BrightCyan),
              Bar(55, "Retail").color(Color.BrightGreen),
              Bar(10, "Other").color(Color.BrightMagenta)
            ),
            "Q3"
          ),
          StackedBar(
            Seq(
              Bar(60, "Online").color(Color.BrightCyan),
              Bar(40, "Retail").color(Color.BrightGreen),
              Bar(25, "Other").color(Color.BrightMagenta)
            ),
            "Q4"
          )
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

  /* -- Scene 8: Selections & Heatmap -- */
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

    val baseData = Seq(
      Seq(10.0, 45.0, 80.0, 75.0, 50.0, 15.0),
      Seq(12.0, 50.0, 85.0, 70.0, 55.0, 20.0),
      Seq(8.0, 40.0, 90.0, 80.0, 60.0, 25.0),
      Seq(15.0, 55.0, 75.0, 65.0, 45.0, 18.0),
      Seq(10.0, 48.0, 70.0, 60.0, 35.0, 30.0),
      Seq(5.0, 15.0, 25.0, 30.0, 40.0, 55.0),
      Seq(3.0, 10.0, 20.0, 25.0, 35.0, 45.0)
    )

    /* Selected days stay bright, unselected dim out */
    val heatData = if (state.selected.isEmpty) baseData
    else baseData.zipWithIndex.map { case (row, idx) =>
      if (state.selected.contains(idx)) row else row.map(_ * 0.15)
    }

    columns(
      box("Schedule")(
        Layout(selectorLines),
        br,
        s"$selCount of ${days.length} active".color(if (selCount > 0) Color.BrightGreen
        else Color.BrightBlack)
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

  /* -- Scene 1: Ray Marcher -- */
  private def sceneRayMarcher(state: ShowcaseState): Element = {
    val fb = RayScene.renderFrame(state.rayTheta, state.rayPhi, state.rayDist, state.rayMorph)

    columns(
      fb,
      box("Camera")(
        leftAlign(
          layout(
            kv(
              "θ"    -> f"${state.rayTheta % (2 * math.Pi)}%.2f",
              "φ"    -> f"${state.rayPhi}%.2f",
              "zoom" -> f"${state.rayDist}%.1f"
            ).color(Color.BrightBlue),
            br,
            spinner("render", state.tick / 2, SpinnerStyle.Dots).color(Color.BrightCyan),
            spinner("light", state.tick / 2, SpinnerStyle.Moon).color(Color.BrightYellow),
            br,
            layout(
              "wasd   orbit".color(Color.BrightYellow),
              "+/-    zoom".color(Color.BrightYellow),
              "Space  shape".color(Color.BrightYellow)
            ).style(Style.Dim)
          ),
          20
        )
      ).border(Border.Round).color(Color.BrightMagenta)
    )
  }
}

/* ---- Ray marcher helpers for Scene 1 ---- */
case class RV3(x: Double, y: Double, z: Double) {
  def +(o: RV3): RV3 = RV3(x + o.x, y + o.y, z + o.z)
  def -(o: RV3): RV3 = RV3(x - o.x, y - o.y, z - o.z)
  def *(s: Double): RV3 = RV3(x * s, y * s, z * s)
  def dot(o: RV3): Double = x * o.x + y * o.y + z * o.z
  def cross(o: RV3): RV3 = RV3(y * o.z - z * o.y, z * o.x - x * o.z, x * o.y - y * o.x)
  def length: Double = math.sqrt(this dot this)
  def normalized: RV3 = { val l = length; if (l < 1e-10) RV3.Zero else this * (1.0 / l) }
  def unary_- : RV3 = RV3(-x, -y, -z)
}
object RV3 {
  val Zero = RV3(0, 0, 0)
  val Up   = RV3(0, 1, 0)
}

case class RPixel(ch: Char, r: Int, g: Int, b: Int)

object RayScene {
  import layoutz._

  val shapeNames = Vector("torus", "cube")

  private val W = 46
  private val H = 22

  private val MaxSteps = 50
  private val MaxDist = 20.0
  private val Eps = 0.005
  private val Ramp =
    " .'`^\",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
  private val LightDir = RV3(0.8, 1.0, -0.6).normalized
  private val FillDir  = RV3(-0.6, 0.4, 0.7).normalized // dim opposite-side fill

  private def clamp(x: Double, lo: Double, hi: Double): Double = math.max(lo, math.min(hi, x))
  private def mix(a: Double, b: Double, t: Double): Double = a * (1.0 - t) + b * t
  private def smoothstep(lo: Double, hi: Double, x: Double): Double = {
    val t = clamp((x - lo) / (hi - lo), 0.0, 1.0); t * t * (3.0 - 2.0 * t)
  }

  private def sdTorus(p: RV3, bigR: Double, r: Double): Double = {
    val qx = math.sqrt(p.x * p.x + p.z * p.z) - bigR
    math.sqrt(qx * qx + p.y * p.y) - r
  }

  private def sdRoundBox(p: RV3, b: RV3, r: Double): Double = {
    val q = RV3(math.abs(p.x) - b.x, math.abs(p.y) - b.y, math.abs(p.z) - b.z)
    val outer = RV3(math.max(q.x, 0), math.max(q.y, 0), math.max(q.z, 0)).length
    outer + math.min(math.max(q.x, math.max(q.y, q.z)), 0.0) - r
  }

  private def scene(p: RV3, morph: Double): Double = {
    val torus = sdTorus(p, 0.9, 0.38)
    val cube = sdRoundBox(p, RV3(0.72, 0.72, 0.72), 0.12)
    mix(torus, cube, smoothstep(0, 1, clamp(morph, 0.0, 1.0)))
  }

  private def calcNormal(p: RV3, morph: Double): RV3 = {
    val e = 0.001
    RV3(
      scene(RV3(p.x + e, p.y, p.z), morph) - scene(RV3(p.x - e, p.y, p.z), morph),
      scene(RV3(p.x, p.y + e, p.z), morph) - scene(RV3(p.x, p.y - e, p.z), morph),
      scene(RV3(p.x, p.y, p.z + e), morph) - scene(RV3(p.x, p.y, p.z - e), morph)
    ).normalized
  }

  @annotation.tailrec
  private def march(ro: RV3, rd: RV3, morph: Double, t: Double = 0.0, i: Int = 0): Double =
    if (i >= MaxSteps || t >= MaxDist) -1.0
    else {
      val d = scene(ro + rd * t, morph)
      if (d < Eps) t else march(ro, rd, morph, t + d, i + 1)
    }

  private def bgPixel(rd: RV3): RPixel = {
    val vy = rd.y * 0.5 + 0.5
    val bg = math.max(18.0 - (1.0 - vy) * 8.0, 4.0).toInt
    RPixel(' ', bg, bg, bg + 6)
  }

  private def shade(ro: RV3, rd: RV3, morph: Double): RPixel = {
    val t = march(ro, rd, morph)
    if (t < 0) bgPixel(rd)
    else {
      val hit = ro + rd * t
      val n = calcNormal(hit, morph)
      val diff = math.max(n dot LightDir, 0.0)
      val fill = math.max(n dot FillDir, 0.0) * 0.35
      val refl = n * (2.0 * (n dot LightDir)) - LightDir
      val spec = math.pow(math.max(refl dot -rd, 0.0), 32.0) * 0.6
      val ao = 1.0 - clamp(scene(hit + n * 0.1, morph) * 5.0, 0.0, 0.4)
      /* Floor keeps any hit surface above the ramp's sparse/whitespace glyphs,
       * so faces read as solid (never see-through) even when unlit. */
      val lum = clamp(math.max((0.12 + diff * 0.7 + fill + spec) * ao, 0.18), 0.0, 1.0)
      val ch = Ramp(clamp(lum * (Ramp.length - 1), 0, Ramp.length - 1).toInt)
      val nx = n.x * 0.5 + 0.5; val ny = n.y * 0.5 + 0.5; val nz = n.z * 0.5 + 0.5
      RPixel(
        ch,
        clamp((nx * 0.55 + lum * 0.45) * 235 + 20, 0, 255).toInt,
        clamp((ny * 0.45 + lum * 0.55) * 215 + 15, 0, 255).toInt,
        clamp((nz * 0.50 + lum * 0.50 + 0.05) * 200 + 30, 0, 255).toInt
      )
    }
  }

  case class FrameBuffer(pixels: Array[RPixel], w: Int, h: Int) extends Element {
    def render: String = {
      val sb = new java.lang.StringBuilder(w * h * 20)
      var i = 0; var y = 0
      while (y < h) {
        var x = 0
        while (x < w) {
          val p = pixels(i)
          sb.append("[38;2;").append(p.r).append(';').append(p.g).append(';').append(p.b)
            .append('m').append(p.ch)
          i += 1; x += 1
        }
        sb.append("[0m")
        if (y < h - 1) sb.append('\n')
        y += 1
      }
      sb.toString
    }
  }

  def renderFrame(theta: Double, phi: Double, dist: Double, morph: Double): FrameBuffer = {
    val ro = RV3(
      dist * math.sin(theta) * math.cos(phi),
      dist * math.sin(phi),
      dist * math.cos(theta) * math.cos(phi)
    )
    val fwd = (-ro).normalized
    val right = (fwd cross RV3.Up).normalized
    val up = right cross fwd
    val aspect = W.toDouble / H.toDouble * 0.48

    val pixels = new Array[RPixel](W * H)
    var i = 0; var py = 0
    while (py < H) {
      val v = 0.5 - py.toDouble / H
      var px = 0
      while (px < W) {
        val u = (px.toDouble / W - 0.5) * aspect
        val rd = (fwd + right * u + up * v).normalized
        pixels(i) = shade(ro, rd, morph)
        i += 1; px += 1
      }
      py += 1
    }
    FrameBuffer(pixels, W, H)
  }
}

object ShowcaseAppRunner {
  def main(args: Array[String]): Unit =
    ShowcaseApp.run(alignment = Alignment.Center)
}
