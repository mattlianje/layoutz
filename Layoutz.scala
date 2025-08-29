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
      if (lines.isEmpty) 0 else lines.map(stripAnsiCodes(_).length).max
    }
    final def height: Int = {
      val rendered = render
      if (rendered.isEmpty) 0
      else {
        val lines = rendered.split("\n", -1)
        math.max(1, lines.length)
      }
    }
  }

  /** Strip ANSI escape sequences to get visual width */
  private def stripAnsiCodes(text: String): String = {
    text.replaceAll("\u001b\\[[0-9;]*m", "")
  }

  /** Flatten multiline elements to single line for components that need
    * single-line content
    */
  private def flattenToSingleLine(element: Element): String = {
    element.render.split('\n').mkString(" ")
  }

  final case class Text(content: String) extends Element {
    def render: String = content
  }

  case object LineBreak extends Element {
    def render: String = "\n"
  }

  /** Margin element that adds a prefix to each line of content */
  final case class Margin(prefix: String, elements: Seq[Element])
      extends Element {
    def render: String = {
      val content =
        if (elements.length == 1) elements.head else Layout(elements)
      val rendered = content.render
      val lines = rendered.split('\n')

      lines.map(line => s"$prefix $line").mkString("\n")
    }
  }

  /** Underline element that draws a line under any element */
  final case class Underline(
      element: Element,
      underlineChar: String = "─"
  ) extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')
      val maxWidth =
        if (lines.isEmpty) 0
        else lines.map(line => stripAnsiCodes(line).length).max

      if (maxWidth == 0) return content

      // Create underline, truncating if underlineChar pattern is too long
      val underlinePattern = underlineChar
      val underline = if (underlinePattern.length >= maxWidth) {
        underlinePattern.take(maxWidth)
      } else {
        val repeats = maxWidth / underlinePattern.length
        val remainder = maxWidth % underlinePattern.length
        (underlinePattern * repeats) + underlinePattern.take(remainder)
      }

      content + "\n" + underline
    }
  }

  /** Ordered list with numbered items - supports automatic nesting */
  final case class OrderedList(items: Seq[Element]) extends Element {

    /* Numbering styles for different nesting levels */
    private def getNumbering(index: Int, level: Int): String = level % 3 match {
      case 0 => (index + 1).toString // 1, 2, 3...
      case 1 => ('a' + index).toChar.toString // a, b, c...
      case 2 => toRomanNumeral(index + 1) // i, ii, iii...
    }

    private def toRomanNumeral(n: Int): String = {
      val mappings = Seq((10, "x"), (9, "ix"), (5, "v"), (4, "iv"), (1, "i"))

      def convert(num: Int, remaining: Seq[(Int, String)]): String = {
        if (num == 0 || remaining.isEmpty) ""
        else {
          val (value, symbol) = remaining.head
          if (num >= value) symbol + convert(num - value, remaining)
          else convert(num, remaining.tail)
        }
      }

      convert(n, mappings)
    }

    def render: String = renderAtLevel(0)

    private def renderAtLevel(level: Int): String = {
      if (items.isEmpty) return ""

      var itemNumber = 0 /* Track numbering for non-nested items only */

      items
        .map { item =>
          item match {
            case nestedList: OrderedList =>
              /* Nested list - render with increased level, don't increment numbering */
              nestedList.renderAtLevel(level + 1)
            case other =>
              /* Regular item - render with current level numbering */
              val number = getNumbering(itemNumber, level)
              itemNumber += 1 /* Only increment for actual items */
              val content = other.render
              val lines = content.split('\n')
              val indent = "  " * level /* 2 spaces per level */

              if (lines.length == 1) {
                s"$indent$number. ${lines.head}"
              } else {
                val firstLine = s"$indent$number. ${lines.head}"
                val lineIndent = indent + " " * (number.length + 2)
                val remainingLines = lines.tail.map(line => s"$lineIndent$line")
                (firstLine +: remainingLines).mkString("\n")
              }
          }
        }
        .mkString("\n")
    }
  }

  /** Unordered list with bullet points - supports automatic nesting */
  final case class UnorderedList(items: Seq[Element], bullet: String = "•")
      extends Element {

    /* Bullet styles for different nesting levels */
    private val bulletStyles = Array("•", "◦", "▪")

    def render: String = renderAtLevel(0)

    private def renderAtLevel(level: Int): String = {
      if (items.isEmpty) return ""

      val currentBullet = if (bullet == "•") {
        /* Auto bullet - use level-appropriate style */
        bulletStyles(level % bulletStyles.length)
      } else {
        /* Custom bullet - use as specified */
        bullet
      }

      items
        .map { item =>
          item match {
            case nestedList: UnorderedList =>
              /* Nested list - render with increased level */
              nestedList.renderAtLevel(level + 1)
            case other =>
              /* Regular item - render with current level indentation */
              val content = other.render
              val lines = content.split('\n')
              val indent = "  " * level /* 2 spaces per level */

              if (lines.length == 1) {
                s"$indent$currentBullet ${lines.head}"
              } else {
                val firstLine = s"$indent$currentBullet ${lines.head}"
                val lineIndent = indent + " " * (currentBullet.length + 1)
                val remainingLines = lines.tail.map(line => s"$lineIndent$line")
                (firstLine +: remainingLines).mkString("\n")
              }
          }
        }
        .mkString("\n")
    }
  }

  /** Center-align element within specified width */
  final case class Centered(element: Element, targetWidth: Int)
      extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')

      lines
        .map { line =>
          val lineLength = stripAnsiCodes(line).length
          if (lineLength >= targetWidth) {
            line // If line is already wider than target width, don't truncate
          } else {
            val totalPadding = targetWidth - lineLength
            val leftPadding =
              (totalPadding + 1) / 2 /* Give extra space to left when odd */
            val rightPadding = totalPadding - leftPadding
            (" " * leftPadding) + line + (" " * rightPadding)
          }
        }
        .mkString("\n")
    }
  }

  /** Auto-center element based on layout context */
  final case class AutoCentered(element: Element) extends Element {
    def render: String = element.render // Will be resolved by container
  }

  /** Left-align element within specified width */
  final case class LeftAligned(element: Element, targetWidth: Int)
      extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')

      lines
        .map { line =>
          val lineLength = stripAnsiCodes(line).length
          if (lineLength >= targetWidth) {
            line // If line is already wider than target width, don't truncate
          } else {
            line + (" " * (targetWidth - lineLength))
          }
        }
        .mkString("\n")
    }
  }

  /** Right-align element within specified width */
  final case class RightAligned(element: Element, targetWidth: Int)
      extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')

      lines
        .map { line =>
          val lineLength = stripAnsiCodes(line).length
          if (lineLength >= targetWidth) {
            line // If line is already wider than target width, don't truncate
          } else {
            (" " * (targetWidth - lineLength)) + line
          }
        }
        .mkString("\n")
    }
  }

  /** Text wrapping element that breaks long lines at word boundaries */
  final case class Wrapped(element: Element, maxWidth: Int) extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')

      lines.flatMap(wrapLine).mkString("\n")
    }

    private def wrapLine(line: String): Seq[String] = {
      if (line.length <= maxWidth) {
        Seq(line)
      } else {
        val words = line.split(" ", -1) // -1 to preserve trailing empty strings
        val result = scala.collection.mutable.ArrayBuffer[String]()
        var currentLine = ""

        for (word <- words) {
          val testLine =
            if (currentLine.isEmpty) word else currentLine + " " + word

          if (testLine.length <= maxWidth) {
            currentLine = testLine
          } else {
            if (currentLine.nonEmpty) {
              result += currentLine
              currentLine = word
            } else {
              result += word
              currentLine = ""
            }
          }
        }

        if (currentLine.nonEmpty) {
          result += currentLine
        }

        if (result.isEmpty) Seq("") else result.toSeq
      }
    }
  }

  /** Text justification - wraps and makes each line fit exactly the target
    * width by distributing spaces
    */
  final case class Justified(
      element: Element,
      targetWidth: Int,
      justifyLastLine: Boolean = false
  ) extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')

      val allLines = lines.flatMap(wrapLine)

      allLines.zipWithIndex
        .map { case (line, index) =>
          val isLastLine = index == allLines.length - 1
          if (isLastLine && !justifyLastLine && allLines.length > 1) {
            line
          } else {
            justifyLine(line, targetWidth)
          }
        }
        .mkString("\n")
    }

    private def wrapLine(line: String): Seq[String] = {
      if (line.length <= targetWidth) {
        Seq(line)
      } else {
        val words = line.split(" ", -1)
        val result = scala.collection.mutable.ArrayBuffer[String]()
        var currentLine = ""

        for (word <- words) {
          val testLine =
            if (currentLine.isEmpty) word else currentLine + " " + word

          if (testLine.length <= targetWidth) {
            currentLine = testLine
          } else {
            if (currentLine.nonEmpty) {
              result += currentLine
              currentLine = word
            } else {
              result += word
              currentLine = ""
            }
          }
        }

        if (currentLine.nonEmpty) {
          result += currentLine
        }

        if (result.isEmpty) Seq("") else result.toSeq
      }
    }

    private def justifyLine(line: String, width: Int): String = {
      val trimmedLine = line.trim
      if (trimmedLine.length >= width) {
        return trimmedLine
      }

      val words = trimmedLine.split("\\s+").filter(_.nonEmpty)
      if (words.length <= 1) {
        return trimmedLine.padTo(width, ' ')
      }

      val totalWordLength = words.map(_.length).sum
      val totalSpacesNeeded = width - totalWordLength
      val gaps = words.length - 1

      if (gaps == 0) {
        return trimmedLine.padTo(width, ' ')
      }

      val baseSpaces = totalSpacesNeeded / gaps
      val extraSpaces = totalSpacesNeeded % gaps

      val result = new StringBuilder()
      for (i <- words.indices) {
        result.append(words(i))
        if (i < words.length - 1) {
          result.append(" " * baseSpaces)
          if (i < extraSpaces) {
            result.append(" ")
          }
        }
      }

      result.toString
    }
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

  /** Structured key-value pairs */
  final case class KeyValue(pairs: Seq[(String, String)]) extends Element {
    def render: String = {
      if (pairs.isEmpty) return ""

      val maxKeyLength = pairs.map(_._1.length).max
      val alignmentPosition = maxKeyLength + 2

      pairs
        .map { case (key, value) =>
          val keyWithColon = s"$key:"
          val spacesNeeded = alignmentPosition - keyWithColon.length
          val padding = " " * math.max(1, spacesNeeded)
          s"$keyWithColon$padding$value"
        }
        .mkString("\n")
    }
  }

  /** Tabular data with headers and borders */
  final case class Table(
      headers: Seq[Element],
      rows: Seq[Seq[Element]],
      style: Border = Border.Single
  ) extends Element {
    def render: String = {
      // Render all elements to get their line structures
      val headerLines = headers.map(_.render.split('\n'))
      val rowLines = rows.map(_.map(_.render.split('\n')))
      val allRowLines = headerLines +: rowLines

      val columnWidths = calculateColumnWidths(allRowLines)
      val borders = TableBorders(columnWidths, style)

      // Build header row(s)
      val headerRowHeight = headerLines.map(_.length).max
      val headerRows = buildMultilineTableRows(
        headerLines,
        columnWidths,
        headerRowHeight,
        style
      )

      // Build data rows
      val dataRows = rowLines.flatMap { row =>
        val rowHeight = row.map(_.length).max
        buildMultilineTableRows(row, columnWidths, rowHeight, style)
      }

      (Seq(borders.top) ++ headerRows ++ Seq(
        borders.separator
      ) ++ dataRows :+ borders.bottom).mkString("\n")
    }

    private def calculateColumnWidths(
        allRowLines: Seq[Seq[Array[String]]]
    ): Seq[Int] =
      headers.indices.map { columnIndex =>
        allRowLines.flatMap { row =>
          if (columnIndex < row.length) {
            row(columnIndex).map(line => stripAnsiCodes(line).length)
          } else Seq(0)
        }.max
      }

    private case class TableBorders(
        top: String,
        separator: String,
        bottom: String
    )

    private object TableBorders {
      def apply(widths: Seq[Int], style: Border): TableBorders = {
        val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
          style.chars
        val segments = widths.map(horizontal * _)

        /* Table junction characters */
        val (teeDown, teeUp, teeLeft, teeRight, cross) = style match {
          case Border.Single => ("┬", "┴", "┤", "├", "┼")
          case Border.Double => ("╦", "╩", "╣", "╠", "╬")
          case Border.Thick  => ("┳", "┻", "┫", "┣", "╋")
          case Border.Round =>
            ("┬", "┴", "┤", "├", "┼") /* Round uses single junctions */
          case Border.Custom(_, h, _) =>
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

    private def buildMultilineTableRows(
        cellLines: Seq[Array[String]],
        widths: Seq[Int],
        rowHeight: Int,
        style: Border
    ): Seq[String] = {
      val (_, _, _, _, _, vertical) = style.chars

      (0 until rowHeight).map { lineIndex =>
        cellLines
          .zip(widths)
          .map { case (lines, width) =>
            val line = if (lineIndex < lines.length) lines(lineIndex) else ""
            val visibleLength = stripAnsiCodes(line).length
            val padding = width - visibleLength
            line + (" " * math.max(0, padding))
          }
          .mkString(
            s"$vertical ",
            s" $vertical ",
            s" $vertical"
          )
      }
    }
  }

  /** Horizontal progress indicator */
  final case class InlineBar(label: Element, progress: Double) extends Element {
    def render: String = {
      val clampedProgress = math.max(0.0, math.min(1.0, progress))
      val filledSegments = (clampedProgress * Dimensions.ProgressBarWidth).toInt
      val emptySegments = Dimensions.ProgressBarWidth - filledSegments

      val bar =
        Glyphs.BarFilled * filledSegments + Glyphs.BarEmpty * emptySegments
      val percentage = (clampedProgress * 100).toInt

      s"${flattenToSingleLine(label)} [$bar] $percentage%"
    }
  }

  /** Dashboard status card */
  final case class StatusCard(
      label: Element,
      content: Element,
      style: Border = Border.Single
  ) extends Element {
    def render: String = {
      val labelRendered = label.render
      val contentRendered = content.render

      // Handle multiline content properly
      val labelLines = labelRendered.split('\n')
      val contentLines = contentRendered.split('\n')
      val allLines = labelLines ++ contentLines

      // Find maximum visible width across all lines
      val maxTextLength =
        if (allLines.isEmpty) 0
        else allLines.map(line => stripAnsiCodes(line).length).max
      val contentWidth = maxTextLength + Dimensions.MinContentPadding

      val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
        style.chars

      val topBorder =
        topLeft + horizontal * (contentWidth + 2) + topRight
      val bottomBorder =
        bottomLeft + horizontal * (contentWidth + 2) + bottomRight

      // Create lines for label and content, padding each line individually
      val labelCardLines = labelLines.map { line =>
        val visibleLength = stripAnsiCodes(line).length
        val padding = contentWidth - visibleLength
        s"$vertical $line${" " * padding} $vertical"
      }

      val contentCardLines = contentLines.map { line =>
        val visibleLength = stripAnsiCodes(line).length
        val padding = contentWidth - visibleLength
        s"$vertical $line${" " * padding} $vertical"
      }

      (Seq(topBorder) ++ labelCardLines ++ contentCardLines :+ bottomBorder)
        .mkString("\n")
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
  final case class Badge(
      content: Element,
      style: BadgeStyle = BadgeStyle.Default
  ) extends Element {
    def render: String = {
      val text = content.render
      style match {
        case BadgeStyle.Default => s"[$text]"
        case BadgeStyle.Success => s"[OK] $text"
        case BadgeStyle.Error   => s"[ERR] $text"
        case BadgeStyle.Warning => s"[WARN] $text"
        case BadgeStyle.Info    => s"[INFO] $text"
      }
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
      data: Seq[(Element, Double)],
      maxWidth: Int = Dimensions.DefaultChartWidth
  ) extends Element {
    def render: String = {
      if (data.isEmpty) return "No data"

      val maxValue = data.map(_._2).max
      val scale = maxWidth.toDouble / maxValue

      data
        .map { case (labelElement, value) =>
          // Flatten multiline labels to single line for chart display
          val label = flattenToSingleLine(labelElement)
          val barLength = (value * scale).toInt
          val bar = "█" * barLength
          val visibleLabelLength = stripAnsiCodes(label).length
          val truncatedLabel =
            if (visibleLabelLength <= Dimensions.ChartLabelMaxWidth) label
            else stripAnsiCodes(label).take(Dimensions.ChartLabelMaxWidth)
          val padding = " " * (Dimensions.ChartLabelSpacing - math.min(
            visibleLabelLength,
            Dimensions.ChartLabelMaxWidth
          ))
          s"$truncatedLabel$padding │$bar $value"
        }
        .mkString("\n")
    }
  }

  /** Banner - decorative text in a box */
  final case class Banner(content: Element, style: Border = Border.Double)
      extends Element {
    def render: String = {
      val rendered = content.render
      val lines = if (rendered.isEmpty) Array("") else rendered.split('\n')
      val maxWidth =
        if (lines.isEmpty) 0
        else lines.map(line => stripAnsiCodes(line).length).max
      val totalWidth = maxWidth + Dimensions.BoxInnerPadding

      val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
        style.chars

      val top =
        topLeft + horizontal * (totalWidth - Dimensions.BoxBorderWidth) + topRight
      val bottom =
        bottomLeft + horizontal * (totalWidth - Dimensions.BoxBorderWidth) + bottomRight

      val contentLines = lines.map { line =>
        val visibleLength = stripAnsiCodes(line).length
        val padding = maxWidth - visibleLength
        val actualPadding = " " * padding
        s"$vertical $line$actualPadding $vertical"
      }

      (top +: contentLines :+ bottom).mkString("\n")
    }
  }

  /** Unified border styling for all box-like elements */
  sealed trait Border {
    def chars: (
        String,
        String,
        String,
        String,
        String,
        String
    ) // TL, TR, BL, BR, H, V
  }
  object Border {
    case object Single extends Border {
      val chars = ("┌", "┐", "└", "┘", "─", "│")
    }
    case object Double extends Border {
      val chars = ("╔", "╗", "╚", "╝", "═", "║")
    }
    case object Thick extends Border {
      val chars = ("┏", "┓", "┗", "┛", "━", "┃")
    }
    case object Round extends Border {
      val chars = ("╭", "╮", "╰", "╯", "─", "│")
    }
    final case class Custom(
        corner: String,
        horizontal: String,
        vertical: String
    ) extends Border {
      val chars = (corner, corner, corner, corner, horizontal, vertical)
    }
  }

  /* Keep BannerStyle and BorderStyle for backward compatibility */
  type BannerStyle = Border
  type BorderStyle = Border
  object BannerStyle {
    val Single = Border.Single
    val Double = Border.Double
    val Thick = Border.Thick
    val Round = Border.Round
  }
  object BorderStyle {
    val Single = Border.Single
    val Double = Border.Double
    val Thick = Border.Thick
    val Round = Border.Round
  }

  /** Box - bordered container with optional title */
  final case class Box(
      title: String = "",
      elements: Seq[Element],
      style: Border = Border.Single
  ) extends Element {
    def render: String = {
      /* Combine all elements into a single layout */
      val content =
        if (elements.length == 1) elements.head else Layout(elements)
      val contentLines = content.render.split('\n')
      val contentWidth =
        if (contentLines.isEmpty) 0
        else contentLines.map(line => stripAnsiCodes(line).length).max
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
        val padding = innerWidth - stripAnsiCodes(line).length
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
      flankingChars: Int = 3
  ) extends Element {
    def render: String = {
      val header = s"${glyph * flankingChars} $title ${glyph * flankingChars}"
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
    def render: String = {
      /* Calculaute layout max width for auto-centering */
      val layoutWidth = calculateLayoutWidth(elements)

      /* Resolve auto-centred eneements and render */
      val resolvedElements = elements.map {
        case AutoCentered(element) => Centered(element, layoutWidth)
        case other                 => other
      }

      resolvedElements.map(_.render).mkString("\n")
    }

    private def calculateLayoutWidth(elements: Seq[Element]): Int = {
      val widths = elements.map {
        case AutoCentered(element) => element.width
        case other                 => other.width
      }
      if (widths.nonEmpty) widths.max else Dimensions.DefaultRuleWidth
    }
  }

  /** DSL CONSTRUCTORS
    */

  /** Create a vertical layout of elements.
    *
    * @param elements
    *   the elements to arrange vertically
    * @return
    *   a Layout containing all elements stacked vertically
    */
  def layout(elements: Element*): Layout = Layout(elements)

  /** Create a titled section with default separator (=).
    *
    * @param title
    *   the section title
    * @param content
    *   the section content
    * @return
    *   a Section with title header and content
    */
  def section(title: String)(content: Element): Section =
    Section(title, content)

  /** Create a titled section with custom separator character.
    *
    * @param title
    *   the section title
    * @param glyph
    *   the character used for the separator line
    * @param content
    *   the section content
    * @return
    *   a Section with custom separator
    */
  def section(title: String, glyph: String)(content: Element): Section =
    Section(title, content, glyph)

  /** Create a titled section with custom separator and flanking character
    * count.
    *
    * @param title
    *   the section title
    * @param glyph
    *   the character used for the separator line
    * @param flankingChars
    *   number of separator characters on each side of title
    * @param content
    *   the section content
    * @return
    *   a Section with fully customized separator
    */
  def section(title: String, glyph: String, flankingChars: Int)(
      content: Element
  ): Section = Section(title, content, glyph, flankingChars)

  /** Create aligned key-value pairs.
    *
    * @param pairs
    *   tuples of (key, value) strings
    * @return
    *   a KeyValue element with aligned key-value pairs
    */
  def kv(pairs: (String, String)*): KeyValue = KeyValue(pairs)

  /** Create a table with custom border style (curried for composability).
    *
    * @param style
    *   the border style to use
    * @param headers
    *   sequence of header elements
    * @param rows
    *   sequence of rows, each containing a sequence of cell elements
    * @return
    *   a Table with specified styling
    */
  def table(
      style: Border = Border.Single
  )(headers: Seq[Element], rows: Seq[Seq[Element]]): Table =
    Table(headers, rows, style)

  /** Create a table with single border style.
    *
    * @param headers
    *   sequence of header elements
    * @param rows
    *   sequence of rows, each containing a sequence of cell elements
    * @return
    *   a Table with single-line borders
    */
  def table(headers: Seq[Element], rows: Seq[Seq[Element]]): Table =
    Table(headers, rows, Border.Single)

  /** Create a progress bar with label.
    *
    * @param label
    *   the label element to display before the bar
    * @param progress
    *   progress value between 0.0 and 1.0
    * @return
    *   an InlineBar showing progress percentage
    */
  def inlineBar(label: Element, progress: Double): InlineBar =
    InlineBar(label, progress)

  /** Create a status card with custom border style (curried for composability).
    *
    * @param style
    *   the border style to use
    * @param label
    *   the card label element
    * @param content
    *   the card content element
    * @return
    *   a StatusCard with specified styling
    */
  def statusCard(
      style: Border = Border.Single
  )(label: Element, content: Element): StatusCard =
    StatusCard(label, content, style)

  /** Create a status card with single border style.
    *
    * @param label
    *   the card label element
    * @param content
    *   the card content element
    * @return
    *   a StatusCard with single-line borders
    */
  def statusCard(label: Element, content: Element): StatusCard =
    StatusCard(label, content)

  /** Create a diff visualization showing added/removed items.
    *
    * @param added
    *   sequence of strings that were added
    * @param removed
    *   sequence of strings that were removed
    * @return
    *   a DiffBlock showing changes with + and - prefixes
    */
  def diffBlock(
      added: Seq[String] = Seq.empty,
      removed: Seq[String] = Seq.empty
  ): DiffBlock =
    DiffBlock(added, removed)

  /** Create a bordered box with optional title and custom style (curried for
    * composability).
    *
    * @param style
    *   the border style to use
    * @param title
    *   optional title to display in the top border
    * @param elements
    *   the elements to contain within the box
    * @return
    *   a Box with specified styling and content
    */
  def box(style: Border = Border.Single)(title: String = "")(
      elements: Element*
  ): Box =
    Box(title, elements, style)

  /** Create a simple box without title.
    *
    * @param elements
    *   the elements to contain within the box
    * @return
    *   a Box with single-line borders and no title
    */
  def box()(elements: Element*): Box = Box("", elements, Border.Single)

  /** Create a titled box with single border style.
    *
    * @param title
    *   the title to display in the top border
    * @param elements
    *   the elements to contain within the box
    * @return
    *   a Box with single-line borders and title
    */
  def box(title: String)(elements: Element*): Box =
    Box(title, elements, Border.Single)

  /** Arrange elements horizontally.
    *
    * @param elements
    *   the elements to arrange side by side
    * @return
    *   a Row with elements arranged horizontally
    */
  def row(elements: Element*): Row = Row(elements)

  /** Create a tree visualization with title.
    *
    * @param title
    *   the title to display above the tree
    * @param root
    *   the root TreeNode of the tree structure
    * @return
    *   a Tree with title and hierarchical structure
    */
  def tree(title: String)(root: TreeNode): Tree = Tree(title, root)

  /** Create a tree branch node.
    *
    * @param name
    *   the name of this branch
    * @param children
    *   the child nodes under this branch
    * @return
    *   a TreeBranch with child nodes
    */
  def branch(name: String, children: TreeNode*): TreeBranch =
    TreeBranch(name, children)

  /** Create a tree leaf node.
    *
    * @param name
    *   the name of this leaf
    * @return
    *   a TreeLeaf with no children
    */
  def leaf(name: String): TreeLeaf = TreeLeaf(name)

  /** SPACING & LAYOUT
    */

  /** Single line break */
  def br: LineBreak.type = LineBreak

  /** Multiple line breaks */
  def br(n: Int): Element = {
    if (n <= 0) Text("")
    else if (n == 1) LineBreak
    else Layout(List.fill(n)(LineBreak))
  }

  /** Single space */
  def space: Element = Text(" ")

  /** Multiple spaces */
  def space(n: Int): Element = {
    if (n <= 0) Text("")
    else Text(" " * n)
  }

  /** Horizontal rule with custom character and width */
  def hr(char: String = "─")(
      width: Int = Dimensions.DefaultRuleWidth
  ): HorizontalRule =
    HorizontalRule(char, Some(width))

  /** Default horizontal rule */
  def hr(): HorizontalRule = HorizontalRule()

  /** INTERACTIVE ELEMENTS
    */

  /** Interactive text input field */
  def textInput(
      label: String,
      value: String = "",
      placeholder: String = "",
      active: Boolean = false
  ): TextInput = TextInput(label, value, placeholder, active)

  /** Animated loading spinner */
  def spinner(
      label: String = "",
      frame: Int = 0,
      style: SpinnerStyle = SpinnerStyle.Dots
  ): Spinner = Spinner(label, frame, style)

  /** Status badge with optional styling */
  def badge(content: Element, style: BadgeStyle = BadgeStyle.Default): Badge =
    Badge(content, style)

  /** Arrange elements in columns with spacing */
  def columns(elements: Element*): Columns = Columns(elements)

  /** Horizontal bar chart */
  def chart(data: (Element, Double)*): Chart = Chart(data)

  /** Decorative banner with custom border style (curried for composability) */
  def banner(style: Border = Border.Double)(content: Element): Banner =
    Banner(content, style)

  /** Empty banner with double border */
  def banner(): Banner = Banner(Text(""), Border.Double)

  /** Banner with double border style */
  def banner(content: Element): Banner = Banner(content, Border.Double)

  /** TEXT FORMATTING
    */

  /** Add underline to an element with custom character */
  def underline(char: String = "─")(element: Element): Underline =
    Underline(element, char)

  /** Add default underline to an element */
  def underline(element: Element): Underline = Underline(element, "─")

  /** Ordered (numbered) list */
  def ol(items: Element*): OrderedList = OrderedList(items)

  /** Unordered (bulleted) list with default bullets */
  def ul(items: Element*): UnorderedList = UnorderedList(items)

  /** Unordered list with custom bullet character */
  def ul(bullet: String)(items: Element*): UnorderedList =
    UnorderedList(items, bullet)

  /** ALIGNMENT & FLOW
    */

  /** Center-align element within specified width */
  def center(element: Element, width: Int): Centered = Centered(element, width)

  /** Auto-center element within layout context */
  def center(element: Element): AutoCentered = AutoCentered(element)

  /** Left-align element within specified width */
  def leftAlign(element: Element, width: Int): LeftAligned =
    LeftAligned(element, width)

  /** Right-align element within specified width */
  def rightAlign(element: Element, width: Int): RightAligned =
    RightAligned(element, width)

  /** Wrap text at word boundaries within specified width */
  def wrap(element: Element, width: Int): Wrapped = Wrapped(element, width)

  /** Justify text to exact width by distributing spaces */
  def justify(element: Element, width: Int): Justified =
    Justified(element, width)

  /** Justify all lines including the last line */
  def justifyAll(element: Element, width: Int): Justified =
    Justified(
      element,
      width,
      justifyLastLine = true
    )

  /** MARGINS & PREFIXES
    */

  /** Add a prefix margin to elements */
  def margin(prefix: String)(elements: Element*): Margin =
    Margin(prefix, elements)

  /** Predefined status margins with color coding */
  object margins {
    def error(elements: Element*): Margin =
      Margin("[\u001b[31merror\u001b[0m]", elements)
    def warn(elements: Element*): Margin =
      Margin("[\u001b[33mwarn\u001b[0m]", elements)
    def success(elements: Element*): Margin =
      Margin("[\u001b[32msuccess\u001b[0m]", elements)
    def info(elements: Element*): Margin =
      Margin("[\u001b[36minfo\u001b[0m]", elements)
  }

  /** Alias for margins object */
  val margin = margins

  /** IMPLICIT CONVERSIONS
    */

  /** Automatic conversion from String to Text element. Allows using strings
    * directly wherever Elements are expected.
    *
    * @param s
    *   the string to convert
    * @return
    *   a Text element containing the string
    */
  implicit def stringToText(s: String): Text = Text(s)

  /** Automatic conversion from Seq[String] to Seq[Element]. Allows using string
    * sequences directly with varargs expansion.
    *
    * @param strings
    *   the sequence of strings to convert
    * @return
    *   a sequence of Text elements
    */
  implicit def stringSeqToElementSeq(strings: Seq[String]): Seq[Element] =
    strings.map(Text(_))

  /** APP FRAMEWORK
    */
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

    /** Run this application */
    def run(): Unit = LayoutzRuntime.run(this)
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

    private def parseKey(
        c: Int,
        reader: org.jline.utils.NonBlockingReader
    ): Key = c match {
      case 10 | 13 => EnterKey
      case 27 => /* ESC - check for arrow keys */
        try {
          Thread.sleep(5) /* Brief pause for sequence to arrive */
          val next1 = reader.read()
          if (next1 == 91) { /* '[' character */
            val next2 = reader.read()
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
