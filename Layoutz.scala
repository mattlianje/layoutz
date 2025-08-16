/*
 * +==========================================================================+
 * |                                layoutz                                   |
 * |                  Friendly, expressive print-layout DSL                   |
 * |                            Version 0.0.2                                 |
 * |                 Compatible with Scala 2.12, 2.13, and 3                  |
 * |                                                                          |
 * | Copyright 2025 Matthieu Court (matthieu.court@protonmail.com)            |
 * | Apache License 2.0                                                       |
 * +==========================================================================+
 */
package object layoutz {
  import scala.language.implicitConversions

  private object Dimensions {
    val MinContentPadding = 2
    val BorderThickness = 2
    val SidePadding = 2
    val ProgressBarWidth = 20
    val TreeIndentation = 4
    val TreeConnectorSpacing = 3
    val DefaultRuleWidth = 50
    val BulletIndentation = 2

    /* Chart constants */
    val DefaultChartWidth = 40
    val ChartLabelMaxWidth = 15
    val ChartLabelSpacing = 15

    /* Box constants */
    val BoxInnerPadding = 4 /* Total padding inside boxes (2 on each side) */
    val BoxBorderWidth = 2 /* Width taken by left+right borders */

    /* Terminal/Input constants */
    val PrintableAsciiStart = 32
    val PrintableAsciiEnd = 126
    val CtrlCharOffset = 64
  }

  private object Glyphs {
    /* Box drawing */
    val TopLeft = "┌"; val TopRight = "┐"; val BottomLeft = "└";
    val BottomRight = "┘"
    val Horizontal = "─"; val Vertical = "│"; val Cross = "┼"
    val TeeDown = "┬"; val TeeUp = "┴"; val TeeRight = "├"; val TeeLeft = "┤"

    /* Content */
    val Bullet = "•"; val Space = " "; val BarFilled = "█"; val BarEmpty = "─"

    /* Tree */
    val TreeBranch = "├──"; val TreeLastBranch = "└──"; val TreeVertical = "│"
    val TreeIndent = " " * Dimensions.TreeIndentation
  }

  /** Core layout element */
  sealed trait Element {
    def render: String
    final def width: Int = {
      val lines = render.split('\n')
      if (lines.isEmpty) 0 else lines.map(_.length).max
    }
    final def height: Int = render.split('\n').length
  }

  final case class Text(content: String) extends Element {
    def render: String = content
  }

  case object LineBreak extends Element {
    def render: String = "\n"
  }
  final case class HorizontalRule(
      char: String = "─",
      ruleWidth: Option[Int] = None
  ) extends Element {
    def render: String = {
      val actualWidth = ruleWidth.getOrElse(Dimensions.DefaultRuleWidth)
      char * actualWidth
    }
  }
  final case class Bullet(text: Option[String], children: Seq[Element])
      extends Element {

    def render: String = renderAtLevel(this, 0)

    private def renderAtLevel(element: Element, level: Int): String = {
      element match {
        case Bullet(Some(text), children) =>
          val indent = " " * (level * Dimensions.BulletIndentation)
          val mainItem = s"$indent${Glyphs.Bullet} $text"
          if (children.isEmpty) {
            mainItem
          } else {
            val childItems = children.map(renderAtLevel(_, level + 1))
            (mainItem +: childItems).mkString("\n")
          }
        case Bullet(None, children) =>
          children.map(renderAtLevel(_, level)).mkString("\n")
        case other =>
          val indent = " " * (level * Dimensions.BulletIndentation)
          s"$indent${Glyphs.Bullet} ${other.render}"
      }
    }
  }

  /** Structured key-value pairs */
  final case class KeyValue(pairs: Seq[(String, String)]) extends Element {
    def render: String = {
      if (pairs.isEmpty) return ""

      val maxKeyLength = pairs.map(_._1.length).max
      pairs
        .map { case (key, value) =>
          val paddedKey = key.padTo(maxKeyLength, ' ')
          s"$paddedKey : $value"
        }
        .mkString("\n")
    }
  }

  /** Tabular data with headers and borders */
  final case class Table(
      headers: Seq[String],
      rows: Seq[Seq[String]],
      style: BorderStyle = BorderStyle.Single
  ) extends Element {
    def render: String = {
      val allRows = headers +: rows
      val columnWidths = calculateColumnWidths(allRows)

      val borders = TableBorders(columnWidths, style)
      val headerRow = buildTableRow(headers, columnWidths, style)
      val dataRows = rows.map(row => buildTableRow(row, columnWidths, style))

      (Seq(
        borders.top,
        headerRow,
        borders.separator
      ) ++ dataRows :+ borders.bottom).mkString("\n")
    }

    private def calculateColumnWidths(allRows: Seq[Seq[String]]): Seq[Int] =
      headers.indices.map { columnIndex =>
        allRows.map(_(columnIndex).length).max
      }

    private case class TableBorders(
        top: String,
        separator: String,
        bottom: String
    )

    private object TableBorders {
      def apply(widths: Seq[Int], style: BorderStyle): TableBorders = {
        val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
          style.chars
        val segments = widths.map(horizontal * _)

        /* Table junction characters */
        val (teeDown, teeUp, teeLeft, teeRight, cross) = style match {
          case BorderStyle.Single => ("┬", "┴", "┤", "├", "┼")
          case BorderStyle.Double => ("╦", "╩", "╣", "╠", "╬")
          case BorderStyle.Thick  => ("┳", "┻", "┫", "┣", "╋")
          case BorderStyle.Round =>
            ("┬", "┴", "┤", "├", "┼") /* Round uses single junctions */
          case BorderStyle.Custom(_, h, _) =>
            (h, h, h, h, h) /* Use horizontal char for all junctions */
        }

        TableBorders(
          top = segments.mkString(
            s"$topLeft$horizontal",
            s"$horizontal$teeDown$horizontal",
            s"$horizontal$topRight"
          ),
          separator = segments.mkString(
            s"$teeRight$horizontal",
            s"$horizontal$cross$horizontal",
            s"$horizontal$teeLeft"
          ),
          bottom = segments.mkString(
            s"$bottomLeft$horizontal",
            s"$horizontal$teeUp$horizontal",
            s"$horizontal$bottomRight"
          )
        )
      }
    }

    private def buildTableRow(
        cells: Seq[String],
        widths: Seq[Int],
        style: BorderStyle
    ): String = {
      val (_, _, _, _, _, vertical) = style.chars
      cells
        .zip(widths)
        .map { case (cell, width) =>
          cell.padTo(width, ' ')
        }
        .mkString(
          s"$vertical ",
          s" $vertical ",
          s" $vertical"
        )
    }
  }

  /** Horizontal progress indicator */
  final case class InlineBar(label: String, progress: Double) extends Element {
    def render: String = {
      val clampedProgress = math.max(0.0, math.min(1.0, progress))
      val filledSegments = (clampedProgress * Dimensions.ProgressBarWidth).toInt
      val emptySegments = Dimensions.ProgressBarWidth - filledSegments

      val bar =
        Glyphs.BarFilled * filledSegments + Glyphs.BarEmpty * emptySegments
      val percentage = (clampedProgress * 100).toInt

      s"$label [$bar] $percentage%"
    }
  }

  /** Dashboard status card */
  final case class StatusCard(
      label: String,
      content: String,
      color: Option[String] = None
  ) extends Element {
    def render: String = {
      val maxTextLength = math.max(label.length, content.length)
      val contentWidth = maxTextLength + Dimensions.MinContentPadding

      val topBorder =
        Glyphs.TopLeft + Glyphs.Horizontal * (contentWidth + 2) + Glyphs.TopRight
      val bottomBorder =
        Glyphs.BottomLeft + Glyphs.Horizontal * (contentWidth + 2) + Glyphs.BottomRight
      val labelLine =
        s"${Glyphs.Vertical} ${label.padTo(contentWidth, Glyphs.Space.head)} ${Glyphs.Vertical}"
      val contentLine =
        s"${Glyphs.Vertical} ${content.padTo(contentWidth, Glyphs.Space.head)} ${Glyphs.Vertical}"

      Seq(topBorder, labelLine, contentLine, bottomBorder).mkString("\n")
    }
  }

  /** Change diff visualization */
  final case class DiffBlock(added: Seq[String], removed: Seq[String])
      extends Element {
    def render: String = {
      val removedLines = removed.map(item => s"- $item")
      val addedLines = added.map(item => s"+ $item")
      val allChanges = removedLines ++ addedLines

      if (allChanges.isEmpty) "No changes"
      else s"Changes:\n${allChanges.mkString("\n")}"
    }
  }

  /** Text input field with label and current value */
  final case class TextInput(
      label: String,
      value: String,
      placeholder: String = "",
      active: Boolean = false
  ) extends Element {
    def render: String = {
      val displayValue = if (value.nonEmpty) value else placeholder
      val cursor = if (active) "_" else ""
      val activeMarker = if (active) ">" else " "
      s"$activeMarker $label: $displayValue$cursor"
    }
  }

  /** Animated spinner for loading states */
  final case class Spinner(
      label: String = "",
      frame: Int = 0,
      style: SpinnerStyle = SpinnerStyle.Dots
  ) extends Element {
    def render: String = {
      val spinChar = style.frames(frame % style.frames.length)
      if (label.nonEmpty) s"$spinChar $label" else spinChar
    }

    def nextFrame: Spinner = copy(frame = frame + 1)
  }

  sealed trait SpinnerStyle {
    def frames: Array[String]
  }

  object SpinnerStyle {
    case object Dots extends SpinnerStyle {
      val frames = Array("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")
    }
    case object Line extends SpinnerStyle {
      val frames = Array("|", "/", "-", "\\")
    }
    case object Clock extends SpinnerStyle {
      val frames = Array(
        "🕐",
        "🕑",
        "🕒",
        "🕓",
        "🕔",
        "🕕",
        "🕖",
        "🕗",
        "🕘",
        "🕙",
        "🕚",
        "🕛"
      )
    }
    case object Bounce extends SpinnerStyle {
      val frames = Array("⠁", "⠂", "⠄", "⠂")
    }
  }

  /** Badge/tag for status indicators */
  final case class Badge(text: String, style: BadgeStyle = BadgeStyle.Default)
      extends Element {
    def render: String = style match {
      case BadgeStyle.Default => s"[$text]"
      case BadgeStyle.Success => s"✅ $text"
      case BadgeStyle.Error   => s"❌ $text"
      case BadgeStyle.Warning => s"⚠️  $text"
      case BadgeStyle.Info    => s"ℹ️  $text"
    }
  }

  sealed trait BadgeStyle
  object BadgeStyle {
    case object Default extends BadgeStyle
    case object Success extends BadgeStyle
    case object Error extends BadgeStyle
    case object Warning extends BadgeStyle
    case object Info extends BadgeStyle
  }

  /** Simple column layout */
  final case class Columns(elements: Seq[Element], spacing: Int = 2)
      extends Element {
    def render: String = {
      if (elements.isEmpty) return ""

      val rendered = elements.map(_.render.split('\n'))
      val maxHeight = rendered.map(_.length).max
      val widths = elements.map(_.width)

      (0 until maxHeight)
        .map { row =>
          rendered
            .zip(widths)
            .map { case (lines, width) =>
              val line = if (row < lines.length) lines(row) else ""
              line.padTo(width, ' ')
            }
            .mkString(" " * spacing)
        }
        .mkString("\n")
    }
  }

  /** Simple horizontal bar chart */
  final case class Chart(
      data: Seq[(String, Double)],
      maxWidth: Int = Dimensions.DefaultChartWidth
  ) extends Element {
    def render: String = {
      if (data.isEmpty) return "No data"

      val maxValue = data.map(_._2).max
      val scale = maxWidth.toDouble / maxValue

      data
        .map { case (label, value) =>
          val barLength = (value * scale).toInt
          val bar = "█" * barLength
          val truncatedLabel = label.take(Dimensions.ChartLabelMaxWidth)
          val padding = " " * (Dimensions.ChartLabelSpacing - math.min(
            label.length,
            Dimensions.ChartLabelMaxWidth
          ))
          s"$truncatedLabel$padding │$bar $value"
        }
        .mkString("\n")
    }
  }

  /** Banner - decorative text in a box */
  final case class Banner(text: String, style: BorderStyle = BorderStyle.Double)
      extends Element {
    def render: String = {
      val lines = if (text.isEmpty) Array("") else text.split('\n')
      val maxWidth = if (lines.isEmpty) 0 else lines.map(_.length).max
      val totalWidth = maxWidth + Dimensions.BoxInnerPadding

      val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
        style.chars

      val top =
        topLeft + horizontal * (totalWidth - Dimensions.BoxBorderWidth) + topRight
      val bottom =
        bottomLeft + horizontal * (totalWidth - Dimensions.BoxBorderWidth) + bottomRight

      val content = lines.map { line =>
        val padding = maxWidth - line.length
        s"$vertical $line${" " * padding} $vertical"
      }

      (top +: content :+ bottom).mkString("\n")
    }
  }

  /** Unified border styling for all box-like elements */
  sealed trait BorderStyle {
    def chars: (
        String,
        String,
        String,
        String,
        String,
        String
    ) // TL, TR, BL, BR, H, V
  }
  object BorderStyle {
    case object Single extends BorderStyle {
      val chars = ("┌", "┐", "└", "┘", "─", "│")
    }
    case object Double extends BorderStyle {
      val chars = ("╔", "╗", "╚", "╝", "═", "║")
    }
    case object Thick extends BorderStyle {
      val chars = ("┏", "┓", "┗", "┛", "━", "┃")
    }
    case object Round extends BorderStyle {
      val chars = ("╭", "╮", "╰", "╯", "─", "│")
    }
    final case class Custom(
        corner: String,
        horizontal: String,
        vertical: String
    ) extends BorderStyle {
      val chars = (corner, corner, corner, corner, horizontal, vertical)
    }
  }

  /* Keep BannerStyle for backward compatibility */
  type BannerStyle = BorderStyle
  object BannerStyle {
    val Single = BorderStyle.Single
    val Double = BorderStyle.Double
    val Thick = BorderStyle.Thick
    val Round = BorderStyle.Round
  }

  /** Box - bordered container with optional title */
  final case class Box(
      title: String = "",
      content: Element,
      style: BorderStyle = BorderStyle.Single
  ) extends Element {
    def render: String = {
      val contentLines = content.render.split('\n')
      val contentWidth =
        if (contentLines.isEmpty) 0 else contentLines.map(_.length).max
      val titleWidth =
        if (title.nonEmpty) title.length + Dimensions.MinContentPadding else 0
      val innerWidth = math.max(contentWidth, titleWidth)
      val totalWidth = innerWidth + Dimensions.BoxInnerPadding

      val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
        style.chars

      val topBorder = if (title.nonEmpty) {
        val titlePadding = totalWidth - title.length - Dimensions.BoxBorderWidth
        val leftPad = titlePadding / 2
        val rightPad = titlePadding - leftPad
        s"$topLeft${horizontal * leftPad}$title${horizontal * rightPad}$topRight"
      } else {
        s"$topLeft${horizontal * (totalWidth - Dimensions.BoxBorderWidth)}$topRight"
      }

      val bottomBorder =
        s"$bottomLeft${horizontal * (totalWidth - Dimensions.BoxBorderWidth)}$bottomRight"

      val paddedContent = contentLines.map { line =>
        val padding = innerWidth - line.length
        s"$vertical $line${" " * padding} $vertical"
      }

      (topBorder +: paddedContent :+ bottomBorder).mkString("\n")
    }

    private case class BoxDimensions(totalWidth: Int, contentPadding: Int)

    private object BoxDimensions {
      def calculate(
          title: String,
          contentLines: Array[String]
      ): BoxDimensions = {
        val contentWidth =
          if (contentLines.isEmpty) 0 else contentLines.map(_.length).max
        val minContentWidth =
          math.max(contentWidth, title.length + Dimensions.MinContentPadding)
        val totalContentPadding = Dimensions.SidePadding * 2
        val totalWidth = math.max(
          title.length + totalContentPadding + Dimensions.BorderThickness,
          minContentWidth + totalContentPadding
        )
        val contentPadding = totalWidth - totalContentPadding
        BoxDimensions(totalWidth, contentPadding)
      }
    }

    private case class BoxBorders(top: String, bottom: String)

    private object BoxBorders {
      def apply(title: String, dimensions: BoxDimensions): BoxBorders = {
        val dashesNeeded =
          dimensions.totalWidth - title.length - Dimensions.BorderThickness
        val leftDashes = dashesNeeded / 2
        val rightDashes = dashesNeeded - leftDashes

        BoxBorders(
          top =
            s"${Glyphs.TopLeft}${Glyphs.Horizontal * leftDashes}$title${Glyphs.Horizontal * rightDashes}${Glyphs.TopRight}",
          bottom =
            Glyphs.BottomLeft + Glyphs.Horizontal * (dimensions.totalWidth - Dimensions.BorderThickness) + Glyphs.BottomRight
        )
      }
    }

    private def formatBoxContent(
        line: String,
        dimensions: BoxDimensions
    ): String =
      s"${Glyphs.Vertical} ${line.padTo(dimensions.contentPadding, Glyphs.Space.head)} ${Glyphs.Vertical}"
  }

  /** Section with title header */
  final case class Section(
      title: String,
      content: Element,
      glyph: String = "=",
      flankingChars: Option[Int] = None
  ) extends Element {
    def render: String = {
      val flanking =
        flankingChars.getOrElse(3) // Default to 3 chars per side like "==="
      val header = s"${glyph * flanking} $title ${glyph * flanking}"
      s"$header\n${content.render}"
    }
  }

  /** Horizontal element arrangement */
  final case class Row(elements: Seq[Element]) extends Element {
    def render: String = {
      if (elements.isEmpty) return ""

      val renderedElements = elements.map(_.render.split('\n'))
      val maxHeight = renderedElements.map(_.length).max
      val elementWidths = elements.map(_.width)
      val paddedElements =
        prepareElementsForRow(renderedElements, elementWidths, maxHeight)

      (0 until maxHeight)
        .map { rowIndex =>
          paddedElements.map(_(rowIndex)).mkString(Glyphs.Space).stripTrailing()
        }
        .mkString("\n")
    }

    private def prepareElementsForRow(
        renderedElements: Seq[Array[String]],
        elementWidths: Seq[Int],
        maxHeight: Int
    ): Seq[Array[String]] = {
      renderedElements.zip(elementWidths).map { case (lines, width) =>
        val paddedLines = lines ++ Array.fill(maxHeight - lines.length)("")
        paddedLines.map(line => line.padTo(width, Glyphs.Space.head))
      }
    }
  }

  /** Tree structure GADT for hierarchical data visualization. Supports both
    * branch nodes (with children) and leaf nodes (terminals).
    */
  sealed trait TreeNode extends Element

  final case class TreeBranch(name: String, children: Seq[TreeNode])
      extends TreeNode {
    def render: String = TreeRenderer.render(this, "", isLast = true)
  }

  final case class TreeLeaf(name: String) extends TreeNode {
    def render: String = name
  }

  final case class Tree(title: String, root: TreeNode) extends Element {
    def render: String = s"$title\n${root.render}"
  }

  private object TreeRenderer {
    def render(node: TreeNode, prefix: String, isLast: Boolean): String = {
      node match {
        case TreeLeaf(name) =>
          val connector =
            if (isLast) Glyphs.TreeLastBranch else Glyphs.TreeBranch
          s"$prefix$connector $name"

        case TreeBranch(name, children) =>
          val connector =
            if (isLast) Glyphs.TreeLastBranch else Glyphs.TreeBranch
          val nodeLine = s"$prefix$connector $name/"

          if (children.isEmpty) {
            nodeLine
          } else {
            val childPrefix = prefix + (
              if (isLast) Glyphs.TreeIndent
              else s"${Glyphs.TreeVertical}   "
            )
            val childLines = children.zipWithIndex.map { case (child, index) =>
              val isLastChild = index == children.length - 1
              render(child, childPrefix, isLastChild)
            }
            (nodeLine +: childLines).mkString("\n")
          }
      }
    }
  }

  /** Root layout container */
  final case class Layout(elements: Seq[Element]) extends Element {
    def render: String = elements.map(_.render).mkString("\n\n")
  }

  /* DSL constructors */
  def layout(elements: Element*): Layout = Layout(elements)
  def section(title: String)(content: Element): Section =
    Section(title, content)
  def section(title: String, glyph: String)(content: Element): Section =
    Section(title, content, glyph)
  def section(title: String, glyph: String, flankingChars: Int)(
      content: Element
  ): Section = Section(title, content, glyph, Some(flankingChars))
  def kv(pairs: (String, String)*): KeyValue = KeyValue(pairs)
  def table(
      headers: Seq[String],
      rows: Seq[Seq[String]],
      style: BorderStyle = BorderStyle.Single
  ): Table =
    Table(headers, rows, style)
  def inlineBar(label: String, progress: Double): InlineBar =
    InlineBar(label, progress)
  def statusCard(
      label: String,
      content: String,
      color: String = ""
  ): StatusCard =
    StatusCard(label, content, Option(color).filter(_.nonEmpty))
  def diffBlock(
      added: Seq[String] = Seq.empty,
      removed: Seq[String] = Seq.empty
  ): DiffBlock =
    DiffBlock(added, removed)
  /* Empty box (no title) */
  def box(content: Element): Box = Box("", content, BorderStyle.Single)
  def box(content: Element, style: BorderStyle): Box = Box("", content, style)

  /* Box with title */
  def box(title: String)(content: Element): Box =
    Box(title, content, BorderStyle.Single)
  def box(title: String, style: BorderStyle)(content: Element): Box =
    Box(title, content, style)
  def row(elements: Element*): Row = Row(elements)
  def tree(title: String)(root: TreeNode): Tree = Tree(title, root)
  def branch(name: String, children: TreeNode*): TreeBranch =
    TreeBranch(name, children)
  def leaf(name: String): TreeLeaf = TreeLeaf(name)
  def br: LineBreak.type = LineBreak
  def hr(char: String = "─", width: Int): HorizontalRule =
    HorizontalRule(char, Some(width))
  def hr(char: String): HorizontalRule = HorizontalRule(char, None)
  def hr: HorizontalRule = HorizontalRule()

  def bullet(text: String, children: Element*): Bullet =
    Bullet(Some(text), children)
  def bullets(items: Bullet*): Bullet = Bullet(None, items)
  def bullets(first: String, rest: String*): Bullet = {
    val allItems = (first +: rest).map(text => Text(text))
    Bullet(None, allItems)
  }

  def textInput(
      label: String,
      value: String = "",
      placeholder: String = "",
      active: Boolean = false
  ): TextInput = TextInput(label, value, placeholder, active)

  def spinner(
      label: String = "",
      frame: Int = 0,
      style: SpinnerStyle = SpinnerStyle.Dots
  ): Spinner = Spinner(label, frame, style)

  def badge(text: String, style: BadgeStyle = BadgeStyle.Default): Badge =
    Badge(text, style)
  def columns(elements: Element*): Columns = Columns(elements)
  def chart(data: (String, Double)*): Chart = Chart(data)
  def banner(
      text: String = "",
      style: BorderStyle = BorderStyle.Double
  ): Banner = Banner(text, style)

  implicit def stringToText(s: String): Text = Text(s)

  /* Elm-style App Architecture */
  sealed trait Key
  final case class CharKey(c: Char) extends Key
  final case class SpecialKey(name: String) extends Key

  /* Navigation keys */
  case object EnterKey extends Key
  case object EscapeKey extends Key
  case object TabKey extends Key
  case object BackspaceKey extends Key
  case object DeleteKey extends Key
  case object ArrowUpKey extends Key
  case object ArrowDownKey extends Key
  case object ArrowLeftKey extends Key
  case object ArrowRightKey extends Key

  /* System tick keys (auto-generated by runtime) */
  case object ProgressTickKey extends Key
  case object SpinnerTickKey extends Key

  trait LayoutzApp[State, Message] {
    def init: State
    def update(msg: Message, state: State): State
    def onKey(k: Key): Option[Message]
    def view(state: State): Element
  }
  object LayoutzRuntime {
    import java.util.concurrent.{Executors}
    import scala.concurrent.{ExecutionContext, Future}
    import org.jline.terminal.{Terminal, TerminalBuilder}
    import scala.util.Try

    def run[State, Message](app: LayoutzApp[State, Message]): Unit = {
      val terminal = TerminalBuilder.builder().system(true).build()
      try {
        val executor = Executors.newFixedThreadPool(2)
        implicit val ec: ExecutionContext =
          ExecutionContext.fromExecutor(executor)

        @volatile var currentState = app.init
        @volatile var shouldContinue = true
        val stateLock = new Object()

        def updateState(message: Message): Unit = {
          stateLock.synchronized {
            currentState = app.update(message, currentState)
          }
        }

        def readState(): State = {
          stateLock.synchronized {
            currentState
          }
        }

        terminal.enterRawMode()
        terminal.writer().print("\u001b[2J\u001b[H\u001b[?25l")
        terminal.writer().flush()

        val renderFuture = Future {
          var lastTickTime = System.currentTimeMillis()
          var lastRenderedState: Option[String] = None
          while (shouldContinue) {
            try {
              val currentTime = System.currentTimeMillis()

              /* Auto-tick every 100ms */
              if (currentTime - lastTickTime >= 100) {
                Try {
                  app.onKey(ProgressTickKey).foreach(updateState)
                  app.onKey(SpinnerTickKey).foreach(updateState)
                }.recover { case _ => }
                lastTickTime = currentTime
              }

              /* Only redraw if state changed */
              val currentRender = app.view(readState()).render
              if (lastRenderedState.forall(_ != currentRender)) {
                terminal.writer().print("\u001b[2J\u001b[H")
                terminal.writer().println(currentRender)
                terminal.writer().println("\nPress Ctrl+Q to quit")
                terminal.writer().flush()
                lastRenderedState = Some(currentRender)
              }

              Thread.sleep(50) /* Fast FPS for smooth rendering */
            } catch {
              case _: InterruptedException =>
              case ex: Exception =>
                terminal.writer().println(s"Render error: ${ex.getMessage}")
                terminal.writer().flush()
            }
          }
        }

        val inputFuture = Future {
          while (shouldContinue) {
            try {
              val reader = terminal.reader()
              val input = reader.read()
              val key = parseKey(input, reader)

              /* Check for Ctrl+Q (ASCII 17) to quit with clean terminal */
              if (input == 17) { /* Ctrl+Q */
                terminal
                  .writer()
                  .print("\u001b[2J\u001b[H") /* Clear screen, move to top */
                terminal.writer().print("\u001b[?25h") /* Show cursor */
                terminal.writer().println("Application terminated.")
                terminal.writer().flush()
                shouldContinue = false
              } else {
                app.onKey(key).foreach(updateState)
              }
            } catch {
              case _: InterruptedException =>
              case ex: Exception           =>
            }
          }
        }

        try {
          while (shouldContinue) {
            Thread.sleep(100)
          }
        } finally {
          executor.shutdown()
        }
      } catch {
        case ex: Exception =>
          println(s"Terminal error: ${ex.getMessage}")
      } finally {
        Try(terminal.close())
      }
    }

    private def parseKey(c: Int, reader: Any): Key = c match {
      case 10 | 13 => EnterKey
      case 27 => /* ESC - check for arrow keys */
        try {
          Thread.sleep(5) /* Brief pause for sequence to arrive */
          val next1 = reader.asInstanceOf[{ def read(): Int }].read()
          if (next1 == 91) { /* '[' character */
            val next2 = reader.asInstanceOf[{ def read(): Int }].read()
            next2 match {
              case 65 => ArrowUpKey /* ESC[A */
              case 66 => ArrowDownKey /* ESC[B */
              case 67 => ArrowRightKey /* ESC[C */
              case 68 => ArrowLeftKey /* ESC[D */
              case _  => EscapeKey
            }
          } else {
            EscapeKey
          }
        } catch {
          case _: Exception => EscapeKey
        }
      case 9                        => TabKey
      case 8 | 127                  => BackspaceKey
      case c if c >= 32 && c <= 126 => CharKey(c.toChar)
      case c if c < 32              => SpecialKey(s"Ctrl+${(c + 64).toChar}")
      case c                        => CharKey(c.toChar)
    }
  }
}
