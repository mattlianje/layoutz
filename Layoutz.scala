/*
 * +==========================================================================+
 * |                                layoutz                                   |
 * |                  Friendly, expressive print-layout & TUI DSL             |
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
    val MIN_CONTENT_PADDING = 2
    val BORDER_THICKNESS = 2
    val SIDE_PADDING = 2
    val PROGRESS_BAR_WIDTH = 20
    val TREE_INDENTATION = 4
    val TREE_CONNECTOR_SPACING = 3
    val DEFAULT_RULE_WIDTH = 50

    /* Chart constants */
    val DEFAULT_CHART_WIDTH = 40
    val CHART_LABEL_MAX_WIDTH = 15
    val CHART_LABEL_SPACING = 15

    /* Box constants */
    val BOX_INNER_PADDING = 4 /* Total padding inside boxes (2 on each side) */
    val BOX_BORDER_WIDTH = 2 /* Width taken by left+right borders */

    /* Terminal/Input constants */
    val PRINTABLE_ASCII_START = 32
    val PRINTABLE_ASCII_END = 126
    val CTRL_CHAR_OFFSET = 64
  }

  private object Glyphs {
    /* Box drawing */
    val TOP_LEFT = "┌"; val TOP_RIGHT = "┐"; val BOTTOM_LEFT = "└";
    val BOTTOM_RIGHT = "┘"
    val HORIZONTAL = "─"; val VERTICAL = "│"; val CROSS = "┼"
    val TEE_DOWN = "┬"; val TEE_UP = "┴"; val TEE_RIGHT = "├";
    val TEE_LEFT = "┤"

    /* Content */
    val BULLET = "•"; val SPACE = " "; val BAR_FILLED = "█"; val BAR_EMPTY = "─"

    /* Tree */
    val TREE_BRANCH = "├──"; val TREE_LAST_BRANCH = "└──";
    val TREE_VERTICAL = "│"
    val TREE_INDENT = " " * Dimensions.TREE_INDENTATION
  }

  /** Core layout element */
  sealed trait Element {
    def render: String
    final def width: Int = {
      val rendered = render
      if (rendered.isEmpty) return 0

      var maxWidth = 0
      var start = 0
      var i = 0
      while (i <= rendered.length) {
        if (i == rendered.length || rendered.charAt(i) == '\n') {
          if (i > start) {
            val lineWidth = stripAnsiCodes(rendered.substring(start, i)).length
            if (lineWidth > maxWidth) maxWidth = lineWidth
          }
          start = i + 1
        }
        i += 1
      }
      maxWidth
    }

    final def height: Int = {
      val rendered = render
      if (rendered.isEmpty) return 1

      var lines = 1
      var i = 0
      while (i < rendered.length) {
        if (rendered.charAt(i) == '\n') lines += 1
        i += 1
      }
      lines
    }

    /* ═══════════════════════════════════════════════════════════════════════════
     * FLUENT TRANSFORMATIONS - Available on all elements */

    /** Center this element within specified width */
    final def center(width: Int): Centered = Centered(this, width)

    /** Auto-center this element within layout context */
    final def center(): AutoCentered = AutoCentered(this)

    /** Left-align this element within specified width */
    final def leftAlign(width: Int): LeftAligned = LeftAligned(this, width)

    /** Right-align this element within specified width */
    final def rightAlign(width: Int): RightAligned = RightAligned(this, width)

    /** Add padding around this element */
    final def pad(padding: Int): Padded = Padded(this, padding)

    /** Wrap this element's text at word boundaries within specified width */
    final def wrap(width: Int): Wrapped = Wrapped(this, width)

    /** Truncate this element to specified width with optional ellipsis */
    final def truncate(maxWidth: Int, ellipsis: String = "..."): Truncated =
      Truncated(this, maxWidth, ellipsis)

    /** Add underline to this element with default character */
    final def underline(): Underline = Underline(this, "─")

    /** Add underline to this element with custom character */
    final def underline(char: String): Underline = Underline(this, char)

    /** Justify this element's text to exact width by distributing spaces */
    final def justify(width: Int): Justified = Justified(this, width)

    /** Justify all lines of this element including the last line */
    final def justifyAll(width: Int): Justified =
      Justified(this, width, justifyLastLine = true)

    /** Add a prefix margin to this element */
    final def margin(prefix: String): Margin = Margin(prefix, Seq(this))

    /** Add error margin (red) to this element */
    final def marginError(): Margin =
      Margin("[\u001b[31merror\u001b[0m]", Seq(this))

    /** Add warning margin (yellow) to this element */
    final def marginWarn(): Margin =
      Margin("[\u001b[33mwarn\u001b[0m]", Seq(this))

    /** Add success margin (green) to this element */
    final def marginSuccess(): Margin =
      Margin("[\u001b[32msuccess\u001b[0m]", Seq(this))

    /** Add info margin (cyan) to this element */
    final def marginInfo(): Margin =
      Margin("[\u001b[36minfo\u001b[0m]", Seq(this))
  }

  private val AnsiEscapeRegex = "\u001b\\[[0-9;]*m".r

  /** Strip ANSI escape sequences to get visual width */
  private def stripAnsiCodes(text: String): String = {
    AnsiEscapeRegex.replaceAllIn(text, "")
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

      // Underlines, but truncates if unerline pattern is too long
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
      case 0 => (index + 1).toString
      case 1 => ('a' + index).toChar.toString
      case 2 => toRomanNumeral(index + 1)
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
              val indent =
                "  " * level /* 2 spaces per level (TODO Maybe - custom indentors) */

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
      val actualWidth = ruleWidth.getOrElse(Dimensions.DEFAULT_RULE_WIDTH)
      char * actualWidth
    }
  }

  /** Fluent horizontal rule builder */
  final case class HorizontalRuleBuilder(
      char: String = "─",
      ruleWidth: Option[Int] = None
  ) extends Element {

    def char(newChar: String): HorizontalRuleBuilder = copy(char = newChar)

    def width(newWidth: Int): HorizontalRuleBuilder =
      copy(ruleWidth = Some(newWidth))

    def render: String = {
      val actualWidth = ruleWidth.getOrElse(Dimensions.DEFAULT_RULE_WIDTH)
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

    def border(newStyle: Border): Table = copy(style = newStyle)
    def render: String = {
      val expectedColumnCount = headers.length

      // Normalize rows to have consistent column count
      val normalizedRows = rows.map(normalizeRowLength(_, expectedColumnCount))

      val headerLines = headers.map(_.render.split('\n'))
      val rowLines = normalizedRows.map(_.map(_.render.split('\n')))
      val allRowLines = headerLines +: rowLines

      val columnWidths = calculateColumnWidths(allRowLines)
      val borders = TableBorders(columnWidths, style)

      val headerRowHeight = headerLines.map(_.length).max
      val headerRows = buildMultilineTableRows(
        headerLines,
        columnWidths,
        headerRowHeight,
        style
      )

      val dataRows = rowLines.flatMap { row =>
        val rowHeight = row.map(_.length).max
        buildMultilineTableRows(row, columnWidths, rowHeight, style)
      }

      (Seq(borders.top) ++ headerRows ++ Seq(
        borders.separator
      ) ++ dataRows :+ borders.bottom).mkString("\n")
    }

    /** Normalize row length to match expected column count. Truncates if too
      * long, pads with empty strings if too short.
      */
    private def normalizeRowLength(
        row: Seq[Element],
        expectedColumnCount: Int
    ): Seq[Element] = {
      if (row.length == expectedColumnCount) {
        row
      } else if (row.length > expectedColumnCount) {
        // Truncate if too long
        row.take(expectedColumnCount)
      } else {
        // Pad with empty strings if too short
        val paddingNeeded = expectedColumnCount - row.length
        row ++ Seq.fill(paddingNeeded)(Text(""))
      }
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
      val filledSegments =
        (clampedProgress * Dimensions.PROGRESS_BAR_WIDTH).toInt
      val emptySegments = Dimensions.PROGRESS_BAR_WIDTH - filledSegments

      val bar =
        Glyphs.BAR_FILLED * filledSegments + Glyphs.BAR_EMPTY * emptySegments
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

    def border(newStyle: Border): StatusCard = copy(style = newStyle)
    def render: String = {
      val labelRendered = label.render
      val contentRendered = content.render

      val labelLines = labelRendered.split('\n')
      val contentLines = contentRendered.split('\n')
      val allLines = labelLines ++ contentLines

      val maxTextLength =
        if (allLines.isEmpty) 0
        else allLines.map(line => stripAnsiCodes(line).length).max
      val contentWidth = maxTextLength + Dimensions.MIN_CONTENT_PADDING

      val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
        style.chars

      val topBorder =
        topLeft + horizontal * (contentWidth + 2) + topRight
      val bottomBorder =
        bottomLeft + horizontal * (contentWidth + 2) + bottomRight

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

  /** Horizontal bar chart */
  final case class Chart(
      data: Seq[(Element, Double)],
      maxWidth: Int = Dimensions.DEFAULT_CHART_WIDTH
  ) extends Element {
    def render: String = {
      if (data.isEmpty) return "No data"

      val maxValue = data.map(_._2).max
      val scale = maxWidth.toDouble / maxValue

      data
        .map { case (labelElement, value) =>
          // Flattens multiline labels to single line for chart display
          val label = flattenToSingleLine(labelElement)
          val barLength = (value * scale).toInt
          val bar = "█" * barLength
          val strippedLabel = stripAnsiCodes(label)
          val visibleLabelLength = strippedLabel.length
          val truncatedLabel =
            if (visibleLabelLength <= Dimensions.CHART_LABEL_MAX_WIDTH) label
            else strippedLabel.take(Dimensions.CHART_LABEL_MAX_WIDTH)
          val padding = " " * (Dimensions.CHART_LABEL_SPACING - math.min(
            visibleLabelLength,
            Dimensions.CHART_LABEL_MAX_WIDTH
          ))
          s"$truncatedLabel$padding │$bar $value"
        }
        .mkString("\n")
    }
  }

  /** Banner - decorative text in a box */
  final case class Banner(content: Element, style: Border = Border.Double)
      extends Element {

    def border(newStyle: Border): Banner = copy(style = newStyle)
    def render: String = {
      val rendered = content.render
      val lines = if (rendered.isEmpty) Array("") else rendered.split('\n')
      val maxWidth =
        if (lines.isEmpty) 0
        else lines.map(line => stripAnsiCodes(line).length).max
      val totalWidth = maxWidth + Dimensions.BOX_INNER_PADDING

      val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
        style.chars

      val top =
        topLeft + horizontal * (totalWidth - Dimensions.BOX_BORDER_WIDTH) + topRight
      val bottom =
        bottomLeft + horizontal * (totalWidth - Dimensions.BOX_BORDER_WIDTH) + bottomRight

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

    def border(newStyle: Border): Box = copy(style = newStyle)
    def render: String = {
      /* Combine all elements into a single layout */
      val content =
        if (elements.length == 1) elements.head else Layout(elements)
      val contentLines = content.render.split('\n')
      val contentWidth =
        if (contentLines.isEmpty) 0
        else contentLines.map(line => stripAnsiCodes(line).length).max
      val titleWidth =
        if (title.nonEmpty) title.length + Dimensions.MIN_CONTENT_PADDING else 0
      val innerWidth = math.max(contentWidth, titleWidth)
      val totalWidth = innerWidth + Dimensions.BOX_INNER_PADDING

      val (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical) =
        style.chars

      val topBorder = if (title.nonEmpty) {
        val titlePadding =
          totalWidth - title.length - Dimensions.BOX_BORDER_WIDTH
        val leftPad = titlePadding / 2
        val rightPad = titlePadding - leftPad
        s"$topLeft${horizontal * leftPad}$title${horizontal * rightPad}$topRight"
      } else {
        s"$topLeft${horizontal * (totalWidth - Dimensions.BOX_BORDER_WIDTH)}$topRight"
      }

      val bottomBorder =
        s"$bottomLeft${horizontal * (totalWidth - Dimensions.BOX_BORDER_WIDTH)}$bottomRight"

      val paddedContent = contentLines.map { line =>
        val padding = innerWidth - stripAnsiCodes(line).length
        s"$vertical $line${" " * padding} $vertical"
      }

      (topBorder +: paddedContent :+ bottomBorder).mkString("\n")
    }

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
          paddedElements.map(_(rowIndex)).mkString(Glyphs.SPACE).stripTrailing()
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
        paddedLines.map(line => line.padTo(width, Glyphs.SPACE.head))
      }
    }
  }

  /** Tree structure GADT for hierarchical data visualization. Supports both
    * branch nodes (with children) and leaf nodes (terminals).
    */
  sealed trait TreeNode extends Element

  final case class TreeBranch(name: String, children: Seq[TreeNode])
      extends TreeNode {
    def render: String = TreeRenderer.renderRoot(this)
    def renderAsChild(prefix: String, isLast: Boolean): String =
      TreeRenderer.render(this, prefix, isLast)
  }

  final case class TreeLeaf(name: String) extends TreeNode {
    def render: String = name
  }

  private object TreeRenderer {
    def renderRoot(node: TreeBranch): String = {
      val rootLine = node.name
      if (node.children.isEmpty) {
        rootLine
      } else {
        val childLines = node.children.zipWithIndex.map { case (child, index) =>
          val isLastChild = index == node.children.length - 1
          render(child, "", isLastChild)
        }
        (rootLine +: childLines).mkString("\n")
      }
    }

    def render(node: TreeNode, prefix: String, isLast: Boolean): String = {
      node match {
        case TreeLeaf(name) =>
          val connector =
            if (isLast) Glyphs.TREE_LAST_BRANCH else Glyphs.TREE_BRANCH
          s"$prefix$connector $name"

        case TreeBuilder(name) =>
          val connector =
            if (isLast) Glyphs.TREE_LAST_BRANCH else Glyphs.TREE_BRANCH
          s"$prefix$connector $name"

        case TreeBranch(name, children) =>
          val connector =
            if (isLast) Glyphs.TREE_LAST_BRANCH else Glyphs.TREE_BRANCH
          val nodeLine = s"$prefix$connector $name/"

          if (children.isEmpty) {
            nodeLine
          } else {
            val childPrefix = prefix + (
              if (isLast) Glyphs.TREE_INDENT
              else s"${Glyphs.TREE_VERTICAL}   "
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
      if (widths.nonEmpty) widths.max else Dimensions.DEFAULT_RULE_WIDTH
    }
  }

  /* ═══════════════════════════════════════════════════════════════════════════
   * DSL CONSTRUCTORS */

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

  /** Create a table.
    *
    * @param headers
    *   sequence of header elements
    * @param rows
    *   sequence of rows, each containing a sequence of cell elements
    * @return
    *   a Table with default single-line borders (use .border() to change)
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

  /** Create a status card.
    *
    * @param label
    *   the card label element
    * @param content
    *   the card content element
    * @return
    *   a StatusCard with default single-line borders (use .border() to change)
    */
  def statusCard(label: Element, content: Element): StatusCard =
    StatusCard(label, content, Border.Single)

  /** Create a bordered box with optional title.
    *
    * @param title
    *   optional title to display in the top border
    * @param elements
    *   the elements to contain within the box
    * @return
    *   a Box with default single-line borders (use .border() to change)
    */
  def box(title: String = "")(elements: Element*): Box =
    Box(title, elements, Border.Single)

  /** Arrange elements horizontally.
    *
    * @param elements
    *   the elements to arrange side by side
    * @return
    *   a Row with elements arranged horizontally
    */
  def row(elements: Element*): Row = Row(elements)

  /** Tree builder that can act as both leaf and branch creator */
  case class TreeBuilder(name: String) extends TreeNode {
    def apply(children: TreeNode*): TreeBranch = TreeBranch(name, children)
    def render: String = name // Renders as just the name when used as a leaf
  }

  /** Create a tree node.
    *   - tree("name") creates a leaf
    *   - tree("name")(children...) creates a branch
    *
    * @param name
    *   the name of this tree node
    * @return
    *   a TreeBuilder that acts as leaf or can create branches
    */
  def tree(name: String): TreeBuilder = TreeBuilder(name)

  /* ═══════════════════════════════════════════════════════════════════════════
   * SPACING & LAYOUT */

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

  /** Add padding around an element */
  final case class Padded(element: Element, padding: Int) extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')
      val paddedLines =
        lines.map(line => (" " * padding) + line + (" " * padding))
      val emptyLine = " " * (paddedLines.headOption.map(_.length).getOrElse(0))

      (Seq.fill(padding)(emptyLine) ++ paddedLines ++ Seq.fill(padding)(
        emptyLine
      )).mkString("\n")
    }
  }

  /** Add padding around an element */
  def pad(padding: Int)(element: Element): Padded = Padded(element, padding)

  /** Truncate text with ellipsis if it exceeds max width */
  final case class Truncated(
      element: Element,
      maxWidth: Int,
      ellipsis: String = "..."
  ) extends Element {
    def render: String = {
      val content = element.render
      val lines = content.split('\n')

      lines
        .map { line =>
          val visibleLength = stripAnsiCodes(line).length
          if (visibleLength <= maxWidth) line
          else {
            val truncateAt = maxWidth - ellipsis.length
            if (truncateAt <= 0) ellipsis.take(maxWidth)
            else {
              // Handle ANSI codes properly when truncating
              val stripped = stripAnsiCodes(line)
              stripped.take(truncateAt) + ellipsis
            }
          }
        }
        .mkString("\n")
    }
  }

  /** Truncate element if too wide */
  def truncate(maxWidth: Int, ellipsis: String = "...")(
      element: Element
  ): Truncated =
    Truncated(element, maxWidth, ellipsis)

  /** Empty element - renders nothing (useful for conditional layouts) */
  case object Empty extends Element {
    def render: String = ""
  }

  /** Vertical separator line */
  final case class VerticalRule(char: String = "│", lineCount: Int)
      extends Element {
    def render: String = (char + "\n") * math.max(1, lineCount - 1) + char
  }

  /** Create vertical separator */
  def vr(lineCount: Int, char: String = "│"): VerticalRule =
    VerticalRule(char, lineCount)

  /** Empty element for conditional rendering */
  def empty: Empty.type = Empty

  /** Create a fluent horizontal rule builder. Use .width() and .char() to
    * customize.
    *
    * @example
    *   {{{hr.width(40).char("═")}}}
    */
  def hr: HorizontalRuleBuilder = HorizontalRuleBuilder()

  /* ═══════════════════════════════════════════════════════════════════════════
   * INTERACTIVE ELEMENTS */

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

  /** Arrange elements in columns with spacing */
  def columns(elements: Element*): Columns = Columns(elements)

  /** Horizontal bar chart */
  def chart(data: (Element, Double)*): Chart = Chart(data)

  /** Create a decorative banner.
    *
    * @param content
    *   the content element to display in the banner
    * @return
    *   a Banner with default double-line borders (use .border() to change)
    */
  def banner(content: Element): Banner = Banner(content, Border.Double)

  /** Create an empty banner.
    *
    * @return
    *   an empty Banner with default double-line borders (use .border() to
    *   change)
    */
  def banner(): Banner = Banner(Text(""), Border.Double)

  /* ═══════════════════════════════════════════════════════════════════════════
   * TEXT FORMATTING */

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

  /* ═══════════════════════════════════════════════════════════════════════════
   * ALIGNMENT & FLOW */

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

  /* ═══════════════════════════════════════════════════════════════════════════
   * MARGINS & PREFIXES */

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

  /* ═══════════════════════════════════════════════════════════════════════════
   * IMPLICIT CONVERSIONS */

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

  /* ═══════════════════════════════════════════════════════════════════════════
   * APP RUNTIME */
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

  case object Tick extends Key

  /** Configuration for the Layoutz runtime */
  case class RuntimeConfig(
      tickIntervalMs: Long = 100,
      renderIntervalMs: Long = 50,
      quitKey: Int = 17, // Ctrl+Q
      showQuitMessage: Boolean = true,
      quitMessage: String = "Press Ctrl+Q to quit"
  )

  trait Terminal {
    def enterRawMode(): Unit
    def exitRawMode(): Unit
    def clearScreen(): Unit
    def hideCursor(): Unit
    def showCursor(): Unit
    def write(text: String): Unit
    def writeLine(text: String): Unit
    def flush(): Unit
    def readInput(): Int // Blocking read
    def readInputNonBlocking(): Option[Int] // Non-blocking read
    def close(): Unit
  }

  sealed trait RuntimeError
  case class TerminalError(message: String, cause: Option[Throwable] = None)
      extends RuntimeError
  case class RenderError(message: String, cause: Option[Throwable] = None)
      extends RuntimeError
  case class InputError(message: String, cause: Option[Throwable] = None)
      extends RuntimeError

  type RuntimeResult[T] = Either[RuntimeError, T]

  /** JLine terminal implementation */
  class JLineTerminal private (terminal: org.jline.terminal.Terminal)
      extends Terminal {
    private val reader = terminal.reader()

    def enterRawMode(): Unit = terminal.enterRawMode()
    def exitRawMode(): Unit = () // JLine handles this automatically
    def clearScreen(): Unit = {
      terminal.writer().print("\u001b[2J\u001b[H")
      terminal.writer().flush()
    }
    def hideCursor(): Unit = {
      terminal.writer().print("\u001b[?25l")
      terminal.writer().flush()
    }
    def showCursor(): Unit = {
      terminal.writer().print("\u001b[?25h")
      terminal.writer().flush()
    }
    def write(text: String): Unit = terminal.writer().print(text)
    def writeLine(text: String): Unit = terminal.writer().println(text)
    def flush(): Unit = terminal.writer().flush()
    def readInput(): Int = {
      reader.read()
    }
    def readInputNonBlocking(): Option[Int] = {
      try {
        if (reader.ready()) Some(reader.read()) else None
      } catch {
        case _: Exception => None
      }
    }
    def close(): Unit = scala.util.Try(terminal.close())

    // TODO: Stop exposing JLine reader directly for multi-byte sequences
    def getReader(): org.jline.utils.NonBlockingReader = reader
  }

  object JLineTerminal {
    def create(): RuntimeResult[JLineTerminal] = {
      try {
        import org.jline.terminal.TerminalBuilder
        val terminal = TerminalBuilder.builder().system(true).build()
        Right(new JLineTerminal(terminal))
      } catch {
        case ex: Exception =>
          Left(
            TerminalError(
              s"Failed to create terminal: ${ex.getMessage}",
              Some(ex)
            )
          )
      }
    }
  }

  /** Key parser abstraction */
  trait KeyParser {
    def parseKey(input: Int, terminal: Terminal): Key
  }

  /** Default key parser implementation */
  object DefaultKeyParser extends KeyParser {
    def parseKey(input: Int, terminal: Terminal): Key = input match {
      case 10 | 13 => EnterKey
      case 27 => // ESC - check for arrow keys
        terminal match {
          case jline: JLineTerminal =>
            // TODO: don't use jline directly
            parseEscapeSequence(jline.getReader())
          case _ =>
            parseEscapeSequenceGeneric(terminal)
        }
      case 9                        => TabKey
      case 8 | 127                  => BackspaceKey
      case c if c >= 32 && c <= 126 => CharKey(c.toChar)
      case c if c < 32              => SpecialKey(s"Ctrl+${(c + 64).toChar}")
      case c                        => CharKey(c.toChar)
    }

    private def parseEscapeSequence(
        reader: org.jline.utils.NonBlockingReader
    ): Key = {
      try {
        // TODO: Better way to pause waiting for complete sequence to arrive
        Thread.sleep(5)
        val next1 = reader.read()
        if (next1 == 91) { // '[' character
          val next2 = reader.read()
          next2 match {
            case 65 => ArrowUpKey // ESC[A
            case 66 => ArrowDownKey // ESC[B
            case 67 => ArrowRightKey // ESC[C
            case 68 => ArrowLeftKey // ESC[D
            case _  => EscapeKey
          }
        } else {
          EscapeKey
        }
      } catch {
        case _: Exception => EscapeKey
      }
    }

    private def parseEscapeSequenceGeneric(terminal: Terminal): Key = {
      try {
        Thread.sleep(5)
        terminal.readInputNonBlocking() match {
          case Some(91) => // '[' character
            Thread.sleep(5)
            terminal.readInputNonBlocking() match {
              case Some(65) => ArrowUpKey // ESC[A
              case Some(66) => ArrowDownKey // ESC[B
              case Some(67) => ArrowRightKey // ESC[C
              case Some(68) => ArrowLeftKey // ESC[D
              case _        => EscapeKey
            }
          case _ => EscapeKey
        }
      } catch {
        case _: Exception => EscapeKey
      }
    }
  }

  /** Application lifecycle management */
  trait LayoutzApp[State, Message] {
    def init: State
    def update(msg: Message, state: State): State
    def onKey(k: Key): Option[Message]
    def view(state: State): Element

    /** Run this application with default configuration */
    def run(): Unit = LayoutzRuntime.run(this)

    /** Run this application with custom configuration */
    def run(config: RuntimeConfig): Unit = LayoutzRuntime.run(this, config)

    /** Run this application with custom terminal */
    def run(terminal: Terminal): Unit =
      LayoutzRuntime.run(this, RuntimeConfig(), terminal)

    /** Run this application with custom configuration and terminal */
    def run(config: RuntimeConfig, terminal: Terminal): Unit =
      LayoutzRuntime.run(this, config, terminal)
  }

  object LayoutzRuntime {
    import scala.util.Try

    def run[State, Message](
        app: LayoutzApp[State, Message],
        config: RuntimeConfig = RuntimeConfig(),
        terminalOpt: Option[Terminal] = None
    ): RuntimeResult[Unit] = {
      terminalOpt.orElse(JLineTerminal.create().toOption) match {
        case Some(terminal) =>
          Right(runWithTerminal(app, config, terminal))
        case None =>
          Left(TerminalError("Failed to initialize terminal"))
      }
    }

    def run[State, Message](
        app: LayoutzApp[State, Message],
        config: RuntimeConfig,
        terminal: Terminal
    ): RuntimeResult[Unit] = {
      Right(runWithTerminal(app, config, terminal))
    }

    private def runWithTerminal[State, Message](
        app: LayoutzApp[State, Message],
        config: RuntimeConfig,
        terminal: Terminal
    ): Unit = {
      try {
        val runtime = new RuntimeInstance(app, config, terminal)
        runtime.run()
      } finally {
        terminal.close()
      }
    }

    /** Runtime instance that manages the application lifecycle */
    private class RuntimeInstance[State, Message](
        app: LayoutzApp[State, Message],
        config: RuntimeConfig,
        terminal: Terminal
    ) {

      @volatile private var currentState = app.init
      @volatile private var shouldContinue = true
      private val stateLock = new Object()
      private val keyParser = DefaultKeyParser

      def run(): Unit = {
        initialize()

        val renderThread = new Thread(() => runRenderLoop(), "LayoutzRender")
        val tickThread = new Thread(() => runTickLoop(), "LayoutzTick")
        val inputThread = new Thread(() => runInputLoop(), "LayoutzInput")

        renderThread.setDaemon(true)
        tickThread.setDaemon(true)

        renderThread.start()
        tickThread.start()
        inputThread.start()

        try {
          inputThread.join()
        } catch {
          case _: InterruptedException => shouldContinue = false
        } finally {
          shouldContinue = false
          cleanup()
        }
      }

      private def initialize(): Unit = {
        terminal.enterRawMode()
        terminal.clearScreen()
        terminal.hideCursor()
      }

      private def cleanup(): Unit = {
        terminal.clearScreen()
        terminal.showCursor()
        terminal.writeLine("Application terminated.")
        terminal.flush()
      }

      private def updateState(message: Message): Unit = {
        stateLock.synchronized {
          currentState = app.update(message, currentState)
        }
      }

      private def readState(): State = {
        stateLock.synchronized {
          currentState
        }
      }

      private def runRenderLoop(): Unit = {
        var lastRenderedState: Option[String] = None

        while (shouldContinue) {
          try {
            val currentRender = app.view(readState()).render
            if (lastRenderedState.forall(_ != currentRender)) {
              terminal.clearScreen()
              terminal.writeLine(currentRender)
              if (config.showQuitMessage) {
                terminal.writeLine(s"\n${config.quitMessage}")
              }
              terminal.flush()
              lastRenderedState = Some(currentRender)
            }
            Thread.sleep(config.renderIntervalMs)
          } catch {
            case ex: Exception => handleRenderError(ex)
          }
        }
      }

      private def runTickLoop(): Unit = {
        while (shouldContinue) {
          try {
            app.onKey(Tick).foreach(updateState)
            Thread.sleep(config.tickIntervalMs)
          } catch {
            case ex: Exception => handleTickError(ex)
          }
        }
      }

      private def runInputLoop(): Unit = {
        while (shouldContinue) {
          try {
            val input = terminal.readInput()
            if (input == config.quitKey) {
              shouldContinue = false
            } else {
              val key = keyParser.parseKey(input, terminal)
              app.onKey(key).foreach(updateState)
            }
          } catch {
            case ex: Exception =>
              handleInputError(ex)
              Thread.sleep(10)
          }
        }
      }

      private def handleRenderError(ex: Throwable): Unit = {
        terminal.writeLine(s"Render error: ${ex.getMessage}")
        terminal.flush()
      }

      private def handleTickError(ex: Throwable): Unit = {}

      private def handleInputError(ex: Throwable): Unit = {
        try {
          terminal.writeLine(s"\nInput error: ${ex.getMessage}")
          terminal.flush()
        } catch {
          case _: Exception =>
        }
      }
    }
  }
}
