/*
 * +==========================================================================+
 * |                               Layoutz                                    |
 * |               Friendly, expressive print-layout & TUI DSL                |
 * |                           Version 0.0.6                                  |
 * |              Compatible with Scala 2.12, 2.13, and 3                     |
 * |                             (JVM, Native)                                |
 * |                                                                          |
 * | Copyright 2025-2026 Matthieu Court (matthieu.court@protonmail.com)       |
 * | Apache License 2.0                                                       |
 * +==========================================================================+
 */

package object layoutz {

  import scala.language.implicitConversions

  type Width = Int
  type Height = Int
  type Padding = Int

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
    val BULLET_STYLES = Array("•", "◦", "▪")

    /* Tree */
    val TREE_BRANCH = "├──"; val TREE_LAST_BRANCH = "└──";
    val TREE_VERTICAL = "│"
    val TREE_INDENT = " " * Dimensions.TREE_INDENTATION

    /* Vertical block 1/8 to 8/8 */
    val BLOCK_CHARS = Array(' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█')

    /*
     * Braille dot positions: each char is 2x4 dots
     * ┌───┬───┐
     * │ 1 │ 4 │  bits: 0x01, 0x08
     * │ 2 │ 5 │  bits: 0x02, 0x10
     * │ 3 │ 6 │  bits: 0x04, 0x20
     * │ 7 │ 8 │  bits: 0x40, 0x80
     * └───┴───┘
     */
    val BRAILLE_DOTS: Array[Array[Int]] = Array(
      Array(0x01, 0x08),
      Array(0x02, 0x10),
      Array(0x04, 0x20),
      Array(0x40, 0x80)
    )
  }

  private object Palette {
    val DEFAULT_COLORS: Array[Color] = Array(
      Color.Blue,
      Color.Green,
      Color.Red,
      Color.Yellow,
      Color.Magenta,
      Color.Cyan
    )
  }

  private object Unicode {
    val ANSI_ESCAPE_REGEX = "\u001b\\[[0-9;]*m".r

    /* Character width ranges for terminal display */
    val ASCII_FAST_PATH_END = 0x0300
    val COMBINING_DIACRITICAL = (0x0300, 0x0370)
    val HANGUL_JAMO = (0x1100, 0x1200)
    val GEOMETRIC_SHAPES = (0x25a0, 0x2600)
    val CJK_MAIN = (0x2e80, 0x9fff)
    val HANGUL_SYLLABLES = (0xac00, 0xd7a4)
    val CJK_COMPAT_IDEOGRAPHS = (0xf900, 0xfb00)
    val VERTICAL_FORMS = (0xfe10, 0xfe20)
    val CJK_COMPAT_FORMS = (0xfe30, 0xfe70)
    val FULLWIDTH_FORMS = (0xff00, 0xff61)
    val FULLWIDTH_SYMBOLS = (0xffe0, 0xffe7)
    val EMOJI_START = 0x1f000
    val SUPPLEMENTARY_IDEOGRAPHS = (0x20000, 0x2ffff)
    val TERTIARY_IDEOGRAPHS = (0x30000, 0x3ffff)
  }

  /** Core layout element */
  trait Element {
    def render: String

    final def width: Width = {
      val rendered = render
      if (rendered.isEmpty) 0
      else {
        val lines = rendered.split('\n')
        if (lines.isEmpty) 0 else lines.map(realLength).max
      }
    }

    final def height: Height = {
      val rendered = render
      if (rendered.isEmpty) 1
      else rendered.count(_ == '\n') + 1
    }

    final def center(width: Int): Centered = Centered(this, width)
    final def center(): AutoCentered = AutoCentered(this)
    final def leftAlign(width: Int): LeftAligned = LeftAligned(this, width)
    final def rightAlign(width: Int): RightAligned = RightAligned(this, width)
    final def pad(padding: Int): Padded = Padded(this, padding)
    final def wrap(width: Int): Wrapped = Wrapped(this, width)
    final def truncate(maxWidth: Int, ellipsis: String = "..."): Truncated =
      Truncated(this, maxWidth, ellipsis)
    final def underline(char: Element = Text("─"), color: Color = Color.NoColor): Underline =
      Underline(this, char.render, if (color == Color.NoColor) None else Some(color))
    @deprecated("Use underline(char, color) instead", "0.6.0")
    final def underlineColored(char: Element, color: Color): Underline =
      underline(char, color)
    final def justify(width: Int): Justified = Justified(this, width)
    final def justifyAll(width: Int): Justified = Justified(this, width, justifyLastLine = true)
    final def margin(prefix: Element): Margin = Margin(prefix.render, Seq(this), None)
    final def marginColored(prefix: Element, color: Color): Margin =
      Margin(prefix.render, Seq(this), Some(color))
    final def color(c: Color): Colored = Colored(c, this)
    final def style(s: Style): Styled = Styled(s, this)
    final def bg(c: Color): BgColored = BgColored(c, this)
    final def putStrLn: Unit = println(render)

  }

  private def stripAnsiCodes(text: String): String =
    Unicode.ANSI_ESCAPE_REGEX.replaceAllIn(text, "")

  private def charWidth(c: Char): Int = {
    import Unicode._
    val cp = c.toInt
    if (cp < ASCII_FAST_PATH_END) 1
    else if (cp >= COMBINING_DIACRITICAL._1 && cp < COMBINING_DIACRITICAL._2) 0
    else if (cp >= HANGUL_JAMO._1 && cp < HANGUL_JAMO._2) 2
    else if (cp >= GEOMETRIC_SHAPES._1 && cp < GEOMETRIC_SHAPES._2) 2
    else if (cp >= CJK_MAIN._1 && cp < CJK_MAIN._2) 2
    else if (cp >= HANGUL_SYLLABLES._1 && cp < HANGUL_SYLLABLES._2) 2
    else if (cp >= CJK_COMPAT_IDEOGRAPHS._1 && cp < CJK_COMPAT_IDEOGRAPHS._2) 2
    else if (cp >= VERTICAL_FORMS._1 && cp < VERTICAL_FORMS._2) 2
    else if (cp >= CJK_COMPAT_FORMS._1 && cp < CJK_COMPAT_FORMS._2) 2
    else if (cp >= FULLWIDTH_FORMS._1 && cp < FULLWIDTH_FORMS._2) 2
    else if (cp >= FULLWIDTH_SYMBOLS._1 && cp < FULLWIDTH_SYMBOLS._2) 2
    else if (cp >= EMOJI_START) 2
    else if (cp >= SUPPLEMENTARY_IDEOGRAPHS._1 && cp < SUPPLEMENTARY_IDEOGRAPHS._2) 2
    else if (cp >= TERTIARY_IDEOGRAPHS._1 && cp < TERTIARY_IDEOGRAPHS._2) 2
    else 1
  }

  /** Calculate real terminal width of string (handles ANSI, emoji, CJK) */
  def realLength(text: String): Int =
    stripAnsiCodes(text).map(charWidth).sum

  /** Flatten multiline elements to single line for components that need single-line content
    */
  private def flattenToSingleLine(element: Element): String =
    element.render.split('\n').mkString(" ")

  /** ANSI color support with 16 standard terminal colors */
  sealed trait Color {
    def code: String

    /** Derive background ANSI code from foreground code */
    def bgCode: String = code match {
      case ""                       => ""
      case c if c.startsWith("38;") => "48" + c.drop(2)
      case c                        => (c.toInt + 10).toString
    }

    /** Apply this color to an element */
    def apply(element: Element): Colored = Colored(this, element)

    /** Apply this color to a string (auto-converts to Text) */
    def apply(text: String): Colored = Colored(this, Text(text))

    /** Apply this color as background to an element */
    def bg(element: Element): BgColored = BgColored(this, element)

    /** Apply this color as background to a string (auto-converts to Text) */
    def bg(text: String): BgColored = BgColored(this, Text(text))
  }

  object Color {
    /* No-op color (for conditional formatting) */
    case object NoColor extends Color { val code = "" }

    /* Standard colors */
    case object Black extends Color { val code = "30" }
    case object Red extends Color { val code = "31" }
    case object Green extends Color { val code = "32" }
    case object Yellow extends Color { val code = "33" }
    case object Blue extends Color { val code = "34" }
    case object Magenta extends Color { val code = "35" }
    case object Cyan extends Color { val code = "36" }
    case object White extends Color { val code = "37" }

    /* Bright variants */
    case object BrightBlack extends Color { val code = "90" }
    case object BrightRed extends Color { val code = "91" }
    case object BrightGreen extends Color { val code = "92" }
    case object BrightYellow extends Color { val code = "93" }
    case object BrightBlue extends Color { val code = "94" }
    case object BrightMagenta extends Color { val code = "95" }
    case object BrightCyan extends Color { val code = "96" }
    case object BrightWhite extends Color { val code = "97" }

    /* 256-color palette (0-255) */
    final case class Full(colorCode: Int) extends Color {
      val code = s"38;5;$colorCode"
    }

    /* 24-bit true color (RGB) */
    final case class True(r: Int, g: Int, b: Int) extends Color {
      val code = s"38;2;$r;$g;$b"
    }

  }

  /** ANSI text styles (bold, italic, etc.) */
  sealed trait Style {
    def code: String

    /** Combine with another style */
    def ++(other: Style): CombinedStyle = CombinedStyle(this, other)
  }

  /** Combined style that applies multiple styles */
  final case class CombinedStyle(first: Style, second: Style) extends Style {
    def code: String = first.code // Not used directly, but kept for interface

    override def ++(other: Style): CombinedStyle = CombinedStyle(this, other)

    /** Flatten all styles in this combined style */
    def flatten: List[Style] = {
      val firstStyles = first match {
        case c: CombinedStyle => c.flatten
        case s                => List(s)
      }
      val secondStyles = second match {
        case c: CombinedStyle => c.flatten
        case s                => List(s)
      }
      firstStyles ++ secondStyles
    }

  }

  object Style {
    /* No-op style (for conditional formatting) */
    case object NoStyle extends Style { val code = "" }

    case object Bold extends Style { val code = "1" }
    case object Dim extends Style { val code = "2" }
    case object Italic extends Style { val code = "3" }
    case object Underline extends Style { val code = "4" }
    case object Blink extends Style { val code = "5" }
    case object Reverse extends Style { val code = "7" }
    case object Hidden extends Style { val code = "8" }
    case object Strikethrough extends Style { val code = "9" }
  }

  /** Wrap text with ANSI color codes */
  private def wrapAnsi(color: Color, content: String): String =
    if (color.code.isEmpty) content
    else "\u001b[" + color.code + "m" + content + "\u001b[0m"

  /** Wrap text with ANSI background color codes */
  private def wrapBgAnsi(color: Color, content: String): String =
    if (color.bgCode.isEmpty) content
    else "\u001b[" + color.bgCode + "m" + content + "\u001b[0m"

  /** Wrap text with ANSI style codes */
  private def wrapStyle(style: Style, content: String): String =
    if (style.code.isEmpty) content
    else "\u001b[" + style.code + "m" + content + "\u001b[0m"

  /** Re-apply an ANSI code after every inner reset so nested colors don't break the outer wrapper */
  private def reapplyAfterResets(ansiPrefix: String, line: String): String =
    if (ansiPrefix.isEmpty) line
    else ansiPrefix + line.replace("\u001b[0m", "\u001b[0m" + ansiPrefix) + "\u001b[0m"

  /** Element wrapper that applies color to its content */
  final case class Colored(color: Color, element: Element) extends Element {

    def render: String = {
      val rendered = element.render
      if (color.code.isEmpty) rendered
      else {
        val prefix = "\u001b[" + color.code + "m"
        val lines = rendered.split('\n')
        lines.map(line => reapplyAfterResets(prefix, line)).mkString("\n")
      }
    }

  }

  /** Element wrapper that applies background color to its content */
  final case class BgColored(color: Color, element: Element) extends Element {

    def render: String = {
      val rendered = element.render
      if (color.bgCode.isEmpty) rendered
      else {
        val prefix = "\u001b[" + color.bgCode + "m"
        val lines = rendered.split('\n')
        lines.map(line => reapplyAfterResets(prefix, line)).mkString("\n")
      }
    }

  }

  /** Element wrapper that applies style to its content */
  final case class Styled(style: Style, element: Element) extends Element {

    def render: String = {
      val rendered = element.render
      val lines = rendered.split('\n')

      style match {
        case combined: CombinedStyle =>
          val styles = combined.flatten
          val prefix = styles.map(s => "\u001b[" + s.code + "m").mkString
          lines
            .map(line => if (prefix.isEmpty) line else reapplyAfterResets(prefix, line))
            .mkString("\n")
        case _ =>
          if (style.code.isEmpty) rendered
          else {
            val prefix = "\u001b[" + style.code + "m"
            lines.map(line => reapplyAfterResets(prefix, line)).mkString("\n")
          }
      }
    }

  }

  final case class Text(content: String) extends Element {
    def render: String = content
  }

  case object LineBreak extends Element {
    def render: String = "\n"
  }

  /** Margin element that adds a prefix to each line of content */
  final case class Margin(
      prefix: String,
      elements: Seq[Element],
      color: Option[Color] = None
  ) extends Element {

    def render: String = {
      val content =
        if (elements.length == 1) elements.head else Layout(elements)
      val rendered = content.render
      val lines = rendered.split('\n')

      val coloredPrefix = color match {
        case Some(c) => wrapAnsi(c, prefix)
        case None    => prefix
      }

      lines.map(line => s"$coloredPrefix $line").mkString("\n")
    }

  }

  /** Underline element that draws a line under any element */
  final case class Underline(
      element: Element,
      underlineChar: String = "─",
      color: Option[Color] = None
  ) extends Element {

    def render: String = {
      val content = element.render
      val lines = content.split('\n')
      val maxWidth = if (lines.isEmpty) 0 else lines.map(realLength).max

      if (maxWidth == 0) return content

      /* Build underline by repeating pattern to match width */
      val underline = if (underlineChar.length >= maxWidth) {
        underlineChar.take(maxWidth)
      } else {
        val repeats = maxWidth / underlineChar.length
        val remainder = maxWidth % underlineChar.length
        (underlineChar * repeats) + underlineChar.take(remainder)
      }

      val coloredUnderline = color match {
        case Some(c) => wrapAnsi(c, underline)
        case None    => underline
      }

      content + "\n" + coloredUnderline
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

      def convert(num: Int, remaining: Seq[(Int, String)]): String =
        if (num == 0 || remaining.isEmpty) ""
        else {
          val (value, symbol) = remaining.head
          if (num >= value) symbol + convert(num - value, remaining)
          else convert(num, remaining.tail)
        }

      convert(n, mappings)
    }

    def render: String = renderAtLevel(0)

    private def renderAtLevel(level: Int): String = {
      if (items.isEmpty) return ""

      val indent = "  " * level
      val (_, rendered) = items.foldLeft((0, Seq.empty[String])) { case ((num, acc), item) =>
        item match {
          case nestedList: OrderedList =>
            (num, acc :+ nestedList.renderAtLevel(level + 1))
          case other =>
            val number = getNumbering(num, level)
            val content = other.render
            val lines = content.split('\n')
            val result = if (lines.length == 1) {
              s"$indent$number. ${lines.head}"
            } else {
              val lineIndent = indent + " " * (number.length + 2)
              (s"$indent$number. ${lines.head}" +: lines.tail.map(l => s"$lineIndent$l")).mkString(
                "\n"
              )
            }
            (num + 1, acc :+ result)
        }
      }
      rendered.mkString("\n")
    }

  }

  final case class UnorderedList(items: Seq[Element], bullet: String = "•")
      extends Element {

    def render: String = renderAtLevel(0)

    private def renderAtLevel(level: Int): String = {
      if (items.isEmpty) return ""

      val currentBullet = if (bullet == "•") {
        /* Auto bullet - use level-appropriate style */
        Glyphs.BULLET_STYLES(level % Glyphs.BULLET_STYLES.length)
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

      /* Find the longest line to center the whole block as a unit */
      val maxLineLength = if (lines.isEmpty) 0 else lines.map(realLength).max

      if (maxLineLength >= targetWidth) {
        /* Content already wider than target - don't modify */
        content
      } else {
        /* Center the whole block - all lines get the same left padding */
        val totalPadding = targetWidth - maxLineLength
        val leftPadding = (totalPadding + 1) / 2

        lines
          .map { line =>
            val lineLength = realLength(line)
            val rightPadding = targetWidth - lineLength - leftPadding
            (" " * leftPadding) + line + (" " * math.max(0, rightPadding))
          }
          .mkString("\n")
      }
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
          val lineLength = realLength(line)
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
          val lineLength = realLength(line)
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

    private def wrapLine(line: String): Seq[String] =
      if (line.length <= maxWidth) Seq(line)
      else {
        val words = line.split(" ", -1)
        val (current, lines) = words.foldLeft(("", Seq.empty[String])) { case ((cur, acc), word) =>
          val test = if (cur.isEmpty) word else cur + " " + word
          if (test.length <= maxWidth) (test, acc)
          else if (cur.nonEmpty) (word, acc :+ cur)
          else ("", acc :+ word)
        }
        val result = if (current.nonEmpty) lines :+ current else lines
        if (result.isEmpty) Seq("") else result
      }

  }

  /** Text justification - wraps and makes each line fit exactly the target width by distributing
    * spaces
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

    private def wrapLine(line: String): Seq[String] =
      if (line.length <= targetWidth) Seq(line)
      else {
        val words = line.split(" ", -1)
        val (current, lines) = words.foldLeft(("", Seq.empty[String])) { case ((cur, acc), word) =>
          val test = if (cur.isEmpty) word else cur + " " + word
          if (test.length <= targetWidth) (test, acc)
          else if (cur.nonEmpty) (word, acc :+ cur)
          else ("", acc :+ word)
        }
        val result = if (current.nonEmpty) lines :+ current else lines
        if (result.isEmpty) Seq("") else result
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
  final case class KeyValue(pairs: Seq[(Element, Element)]) extends Element {

    def render: String = {
      if (pairs.isEmpty) return ""

      val renderedPairs = pairs.map { case (k, v) => (k.render, v.render) }
      val maxKeyLength = renderedPairs.map(p => realLength(p._1)).max
      val alignmentPosition = maxKeyLength + 2

      renderedPairs
        .map { case (key, value) =>
          val keyWithColon = s"$key:"
          val spacesNeeded = alignmentPosition - realLength(keyWithColon)
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
      borderStyle: Border = Border.Single
  ) extends Element {

    def render: String = {
      val expectedColumnCount = headers.length

      val normalizedRows = rows.map(normalizeRowLength(_, expectedColumnCount))

      val headerLines = headers.map(_.render.split('\n'))
      val rowLines = normalizedRows.map(_.map(_.render.split('\n')))
      val allRowLines = headerLines +: rowLines

      val columnWidths = calculateColumnWidths(allRowLines)
      val borders = TableBorders(columnWidths, borderStyle)

      val headerRowHeight = headerLines.map(_.length).max
      val headerRows = buildMultilineTableRows(
        headerLines,
        columnWidths,
        headerRowHeight,
        borderStyle
      )

      val dataRows = rowLines.flatMap { row =>
        val rowHeight = row.map(_.length).max
        buildMultilineTableRows(row, columnWidths, rowHeight, borderStyle)
      }

      (Seq(borders.top) ++ headerRows ++ Seq(
        borders.separator
      ) ++ dataRows :+ borders.bottom).mkString("\n")
    }

    /** Normalize row length to match expected column count. Truncates if too long, pads with empty
      * strings if too short.
      */
    private def normalizeRowLength(
        row: Seq[Element],
        expectedColumnCount: Int
    ): Seq[Element] =
      if (row.length == expectedColumnCount) {
        row
      } else if (row.length > expectedColumnCount) {
        row.take(expectedColumnCount)
      } else {
        val paddingNeeded = expectedColumnCount - row.length
        row ++ Seq.fill(paddingNeeded)(Text(""))
      }

    private def calculateColumnWidths(
        allRowLines: Seq[Seq[Array[String]]]
    ): Seq[Int] =
      headers.indices.map { columnIndex =>
        allRowLines.flatMap { row =>
          if (columnIndex < row.length) {
            row(columnIndex).map(line => realLength(line))
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
        val (tl, tr, bl, br) = (style.topLeft, style.topRight, style.bottomLeft, style.bottomRight)
        val (ht, hb) = (style.hTop, style.hBottom)
        val topSegments = widths.map(ht * _)
        val bottomSegments = widths.map(hb * _)

        /* Table junction characters */
        val (teeDown, teeUp, teeLeft, teeRight, cross) = style match {
          case Border.None => (" ", " ", " ", " ", " ")
          case Border.Single | Border.Dashed | Border.Dotted | Border.Round =>
            ("┬", "┴", "┤", "├", "┼")
          case Border.Double   => ("╦", "╩", "╣", "╠", "╬")
          case Border.Thick    => ("┳", "┻", "┫", "┣", "╋")
          case Border.Ascii    => ("+", "+", "+", "+", "+")
          case Border.Block    => ("█", "█", "█", "█", "█")
          case Border.Markdown => ("|", "|", "|", "|", "|")
          case Border.InnerHalfBlock =>
            ("▄", "▀", "▌", "▐", "▄")
          case Border.OuterHalfBlock =>
            ("▀", "▄", "▐", "▌", "▀")
          case Border.Custom(_, h, _) =>
            (h, h, h, h, h)
        }

        TableBorders(
          top = topSegments.mkString(
            s"$tl$ht",
            s"$ht$teeDown$ht",
            s"$ht$tr"
          ),
          separator = topSegments.mkString(
            s"$teeRight$ht",
            s"$ht$cross$ht",
            s"$ht$teeLeft"
          ),
          bottom = bottomSegments.mkString(
            s"$bl$hb",
            s"$hb$teeUp$hb",
            s"$hb$br"
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
      val (vl, vr) = (style.vLeft, style.vRight)

      (0 until rowHeight).map { lineIndex =>
        cellLines
          .zip(widths)
          .map { case (lines, width) =>
            val line = if (lineIndex < lines.length) lines(lineIndex) else ""
            val visibleLength = realLength(line)
            val padding = width - visibleLength
            line + (" " * math.max(0, padding))
          }
          .mkString(
            s"$vl ",
            s" $vl ",
            s" $vr"
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
      borderStyle: Border = Border.Single
  ) extends Element {

    def render: String = {
      val labelRendered = label.render
      val contentRendered = content.render

      val labelLines = labelRendered.split('\n')
      val contentLines = contentRendered.split('\n')
      val allLines = labelLines ++ contentLines

      val maxTextLength =
        if (allLines.isEmpty) 0
        else allLines.map(line => realLength(line)).max
      val contentWidth = maxTextLength + Dimensions.MIN_CONTENT_PADDING

      val (tl, tr, bl, br) =
        (borderStyle.topLeft, borderStyle.topRight, borderStyle.bottomLeft, borderStyle.bottomRight)
      val (ht, hb, vl, vr) =
        (borderStyle.hTop, borderStyle.hBottom, borderStyle.vLeft, borderStyle.vRight)

      val topBorder = tl + ht * (contentWidth + 2) + tr
      val bottomBorder = bl + hb * (contentWidth + 2) + br

      val labelCardLines = labelLines.map { line =>
        val visibleLength = realLength(line)
        val padding = contentWidth - visibleLength
        s"$vl $line${" " * padding} $vr"
      }

      val contentCardLines = contentLines.map { line =>
        val visibleLength = realLength(line)
        val padding = contentWidth - visibleLength
        s"$vl $line${" " * padding} $vr"
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
      val displayValue = Option(value).filter(_.nonEmpty).getOrElse(placeholder)
      val cursor = if (active) "█" else ""
      val activeMarker = if (active) ">" else " "
      s"$activeMarker $label: $displayValue$cursor"
    }

  }

  /** Single choice selector - pick one option from a list */
  final case class SingleChoice(
      label: String,
      options: Seq[String],
      selected: Int = 0,
      active: Boolean = false
  ) extends Element {

    def render: String = {
      val header = if (active) s"> $label" else s"  $label"
      val optionLines = options.zipWithIndex.map { case (opt, idx) =>
        val marker = if (idx == selected) "●" else "○"
        val highlight =
          if (active && idx == selected) s"  ► $marker $opt"
          else s"    $marker $opt"
        highlight
      }
      (header +: optionLines).mkString("\n")
    }

  }

  /** Multi choice selector - pick multiple options from a list */
  final case class MultiChoice(
      label: String,
      options: Seq[String],
      selected: Set[Int] = Set.empty,
      cursor: Int = 0,
      active: Boolean = false
  ) extends Element {

    def render: String = {
      val header =
        if (active) s"> $label (space to toggle, enter to confirm)"
        else s"  $label"
      val optionLines = options.zipWithIndex.map { case (opt, idx) =>
        val marker = if (selected.contains(idx)) "☑" else "☐"
        val highlight =
          if (active && idx == cursor) s"  ► $marker $opt"
          else s"    $marker $opt"
        highlight
      }
      (header +: optionLines).mkString("\n")
    }

  }

  /** Form builder helper - combines multiple inputs with validation */
  final case class Form(
      title: String,
      fields: Seq[Element],
      activeField: Int = 0,
      showErrors: Boolean = false,
      errorMessage: Option[String] = None
  ) extends Element {

    def render: String = {
      val titleLine = s"=== $title ==="
      val fieldLines = fields.map(_.render)
      val errorLines = if (showErrors && errorMessage.isDefined) {
        Seq("", s"⚠ ${errorMessage.get}")
      } else Seq.empty

      (titleLine +: fieldLines ++: errorLines).mkString("\n")
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
      Option(label)
        .filter(_.nonEmpty)
        .fold(spinChar)(l => s"$spinChar $l")
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
      val frames = Array("🕐", "🕑", "🕒", "🕓", "🕔", "🕕", "🕖", "🕗", "🕘", "🕙", "🕚", "🕛")
    }

    case object Bounce extends SpinnerStyle {
      val frames = Array("⠁", "⠂", "⠄", "⠂")
    }

    case object Earth extends SpinnerStyle {
      val frames = Array("🌍", "🌎", "🌏")
    }

    case object Moon extends SpinnerStyle {
      val frames = Array("🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘")
    }

    case object Grow extends SpinnerStyle {
      val frames = Array("▏", "▎", "▍", "▌", "▋", "▊", "▉", "█", "▉", "▊", "▋", "▌", "▍", "▎")
    }

    case object Arrow extends SpinnerStyle {
      val frames = Array("←", "↖", "↑", "↗", "→", "↘", "↓", "↙")
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
              val visualLen = realLength(line)
              val padding = math.max(0, width - visualLen)
              line + (" " * padding)
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

  final case class Series(
      points: Seq[(Double, Double)],
      label: String = "",
      seriesColor: Color = Color.NoColor
  ) {
    def color(c: Color): Series = copy(seriesColor = c)
  }

  final case class Plot(
      series: Seq[Series],
      plotWidth: Int = 60,
      plotHeight: Int = 20,
      showAxes: Boolean = true,
      showOrigin: Boolean = true
  ) extends Element {

    def render: String = {
      if (series.isEmpty || series.forall(_.points.isEmpty)) return "No data"

      val allPoints = series.flatMap(_.points)
      val xMin = allPoints.map(_._1).min
      val xMax = allPoints.map(_._1).max
      val yMin = allPoints.map(_._2).min
      val yMax = allPoints.map(_._2).max

      val pixelWidth = plotWidth * 2
      val pixelHeight = plotHeight * 4
      val xRange = if (xMax == xMin) 1.0 else xMax - xMin
      val yRange = if (yMax == yMin) 1.0 else yMax - yMin

      val grid = Array.fill(plotHeight, plotWidth)(0)
      val seriesGrid = Array.fill(plotHeight, plotWidth)(-1)
      val dotCount = Array.fill(plotHeight, plotWidth, series.length)(0)

      for {
        (s, seriesIdx) <- series.zipWithIndex
        (x, y) <- s.points
      } {
        val px = ((x - xMin) / xRange * (pixelWidth - 1)).toInt.max(0).min(pixelWidth - 1)
        val py =
          ((yMax - y) / yRange * (pixelHeight - 1)).toInt.max(0).min(pixelHeight - 1) // Y inverted
        val cellX = px / 2
        val cellY = py / 4
        val dotX = px % 2
        val dotY = py % 4
        grid(cellY)(cellX) |= Glyphs.BRAILLE_DOTS(dotY)(dotX)
        dotCount(cellY)(cellX)(seriesIdx) += 1
      }

      /* Origin axes at x=0, y=0 */
      if (showOrigin) {
        if (yMin < 0 && yMax > 0) {
          val py = ((yMax - 0) / yRange * (pixelHeight - 1)).toInt
          val cellY = py / 4
          val dotY = py % 4
          if (cellY >= 0 && cellY < plotHeight) {
            for (px <- 0 until pixelWidth) {
              val cellX = px / 2
              val dotX = px % 2
              grid(cellY)(cellX) |= Glyphs.BRAILLE_DOTS(dotY)(dotX)
            }
          }
        }
        if (xMin < 0 && xMax > 0) {
          val px = ((0 - xMin) / xRange * (pixelWidth - 1)).toInt
          val cellX = px / 2
          val dotX = px % 2
          if (cellX >= 0 && cellX < plotWidth) {
            for (py <- 0 until pixelHeight) {
              val cellY = py / 4
              val dotY = py % 4
              grid(cellY)(cellX) |= Glyphs.BRAILLE_DOTS(dotY)(dotX)
            }
          }
        }
      }

      /* Dominant series per cell (most dots wins) */
      for {
        y <- 0 until plotHeight
        x <- 0 until plotWidth
      } {
        val counts = dotCount(y)(x)
        if (counts.exists(_ > 0)) {
          seriesGrid(y)(x) = counts.zipWithIndex.maxBy(_._1)._2
        }
      }

      /* Braille: U+2800 + bits */
      val plotLines = grid.zip(seriesGrid).map { case (row, seriesIndices) =>
        row.zip(seriesIndices).map { case (bits, seriesIdx) =>
          val ch = (0x2800 + bits).toChar.toString
          if (seriesIdx < 0) ch
          else {
            val s = series(seriesIdx)
            if (s.seriesColor == Color.NoColor) ch else wrapAnsi(s.seriesColor, ch)
          }
        }.mkString
      }

      if (showAxes) {
        val yMaxStr = formatNum(yMax)
        val yMinStr = formatNum(yMin)
        val xMinStr = formatNum(xMin)
        val xMaxStr = formatNum(xMax)
        val labelWidth = math.max(yMaxStr.length, yMinStr.length)
        val pad = " " * labelWidth

        val firstLine = yMaxStr.reverse.padTo(labelWidth, ' ').reverse + " ┤" + plotLines.head
        val middleLines = plotLines.slice(1, plotLines.length - 1).map(l => pad + " │" + l)
        val lastPlotLine = yMinStr.reverse.padTo(labelWidth, ' ').reverse + " ┤" + plotLines.last
        val axisLine = pad + " └" + "─" * plotWidth
        val xLabelLine = " " * (labelWidth + 2) + xMinStr +
          " " * (plotWidth - xMinStr.length - xMaxStr.length) + xMaxStr

        val labelsWithContent = series.filter(_.label.nonEmpty)
        val legend = if (labelsWithContent.length > 1) {
          val legendStr = labelsWithContent.map { s =>
            val marker = if (s.seriesColor == Color.NoColor) "─" else wrapAnsi(s.seriesColor, "─")
            s"$marker ${s.label}"
          }.mkString("  ")
          Seq(" " * (labelWidth + 2) + legendStr)
        } else Seq.empty

        (Seq(firstLine) ++ middleLines ++ Seq(lastPlotLine, axisLine, xLabelLine) ++ legend)
          .mkString("\n")
      } else {
        plotLines.mkString("\n")
      }
    }

    private def formatNum(d: Double): String = {
      val v = if (d == 0.0) 0.0 else d // Normalize -0.0 to 0.0
      if (v == v.toLong) v.toLong.toString
      else f"$v%.1f"
    }

  }

  /** Factory for plot */
  def plot(
      width: Int = 60,
      height: Int = 20,
      showAxes: Boolean = true,
      showOrigin: Boolean = true
  )(series: Series*): Plot =
    Plot(series, width, height, showAxes, showOrigin)

  final case class Slice(
      value: Double,
      label: String = "",
      sliceColor: Option[Color] = None
  ) {
    def color(c: Color): Slice = copy(sliceColor = Some(c))
  }

  final case class Pie(
      slices: Seq[Slice],
      pieWidth: Int = 40,
      pieHeight: Int = 12,
      showLegend: Boolean = true
  ) extends Element {

    def render: String = {
      if (slices.isEmpty || slices.forall(_.value <= 0)) return "No data"

      val total = slices.map(_.value).sum
      val pixelWidth = pieWidth * 2
      val pixelHeight = pieHeight * 4

      val centerX = pixelWidth / 2.0
      val centerY = pixelHeight / 2.0
      val radius = math.min(centerX, centerY * 1.5) - 1

      val coloredSlices = slices.zipWithIndex.map { case (s, i) =>
        (s, s.sliceColor.getOrElse(Palette.DEFAULT_COLORS(i % Palette.DEFAULT_COLORS.length)))
      }

      val angleRanges = {
        val angles = coloredSlices.map { case (s, _) => (s.value / total) * 2 * math.Pi }
        val cumulative = angles.scanLeft(0.0)(_ + _)
        coloredSlices.zip(cumulative.zip(cumulative.tail)).map {
          case ((_, c), (start, end)) => (start, end, c)
        }
      }

      val grid = Array.fill(pieHeight, pieWidth)(0)
      val sliceCount = Array.fill(pieHeight, pieWidth, slices.length)(0)

      /* Fill pie, scale dy for terminal aspect ratio */
      for {
        py <- 0 until pixelHeight
        px <- 0 until pixelWidth
      } {
        val dx = px - centerX
        val dy = (py - centerY) * 1.5
        val dist = math.sqrt(dx * dx + dy * dy)

        if (dist <= radius) {
          var angle = math.atan2(dy, dx)
          if (angle < 0) angle += 2 * math.Pi

          val sliceIdx = angleRanges.indexWhere { case (start, end, _) =>
            angle >= start && angle < end
          }

          val cellX = px / 2
          val cellY = py / 4
          val dotX = px % 2
          val dotY = py % 4
          grid(cellY)(cellX) |= Glyphs.BRAILLE_DOTS(dotY)(dotX)
          if (sliceIdx >= 0) sliceCount(cellY)(cellX)(sliceIdx) += 1
        }
      }

      /* Dominant slice per cell */
      val colorGrid = Array.tabulate(pieHeight, pieWidth) { (y, x) =>
        val counts = sliceCount(y)(x)
        if (counts.exists(_ > 0)) {
          val maxIdx = counts.zipWithIndex.maxBy(_._1)._2
          coloredSlices(maxIdx)._2
        } else Color.NoColor
      }

      /* Braille: U+2800 + bits */
      val pieLines = grid.zip(colorGrid).map { case (row, colors) =>
        row.zip(colors).map { case (bits, c) =>
          val ch = (0x2800 + bits).toChar.toString
          if (c == Color.NoColor) ch else wrapAnsi(c, ch)
        }.mkString
      }

      if (showLegend && slices.exists(_.label.nonEmpty)) {
        val legend = coloredSlices.filter(_._1.label.nonEmpty).map { case (s, c) =>
          val pct = (s.value / total * 100).toInt
          val marker = wrapAnsi(c, "●")
          s"$marker ${s.label} ($pct%)"
        }
        (pieLines ++ ("" +: legend)).mkString("\n")
      } else {
        pieLines.mkString("\n")
      }
    }
  }

  def pie(width: Int = 40, height: Int = 12, showLegend: Boolean = true)(slices: Slice*): Pie =
    Pie(slices, width, height, showLegend)

  final case class Bar(
      value: Double,
      label: String = "",
      barColor: Option[Color] = None
  ) {
    def color(c: Color): Bar = copy(barColor = Some(c))
  }

  final case class BarChart(
      bars: Seq[Bar],
      chartWidth: Int = 40,
      chartHeight: Int = 10,
      showLabels: Boolean = true,
      showAxis: Boolean = true
  ) extends Element {

    def render: String = {
      if (bars.isEmpty) return "No data"

      val maxVal = bars.map(_.value).max
      if (maxVal <= 0) return "No data"

      val numBars = bars.length
      val axisWidth = if (showAxis) maxVal.toInt.toString.length + 2 else 0
      val availableWidth = chartWidth - axisWidth
      val barWidth = math.max(1, (availableWidth - numBars + 1) / numBars)

      val coloredBars = bars.zipWithIndex.map { case (b, i) =>
        (b, b.barColor.getOrElse(Palette.DEFAULT_COLORS(i % Palette.DEFAULT_COLORS.length)))
      }

      val rows = (0 until chartHeight).map { row =>
        val threshold = (chartHeight - row - 1).toDouble / chartHeight
        val nextThreshold = (chartHeight - row).toDouble / chartHeight

        val axisLabel = if (showAxis) {
          /* Show tick value at each row */
          val tickValue = maxVal * (chartHeight - row).toDouble / chartHeight
          val tickStr =
            if (tickValue == tickValue.toLong) tickValue.toLong.toString else f"$tickValue%.0f"
          tickStr.reverse.padTo(axisWidth - 1, ' ').reverse + "┤"
        } else ""

        val barsStr = coloredBars.zipWithIndex.map { case ((b, c), i) =>
          val normalized = b.value / maxVal
          val barStr = if (normalized >= nextThreshold) {
            "█" * barWidth
          } else if (normalized > threshold) {
            val frac = (normalized - threshold) / (1.0 / chartHeight)
            val blockIdx = math.min(8, math.max(1, (frac * 8).toInt))
            Glyphs.BLOCK_CHARS(blockIdx).toString * barWidth
          } else {
            " " * barWidth
          }
          val sep = if (i < numBars - 1) " " else ""
          wrapAnsi(c, barStr) + sep
        }.mkString

        axisLabel + barsStr
      }

      val labelRow = if (showLabels && bars.exists(_.label.nonEmpty)) {
        val prefix = if (showAxis) " " * axisWidth else ""
        Some(prefix + coloredBars.map { case (b, _) =>
          val lbl = b.label.take(barWidth)
          val pad = barWidth - lbl.length
          val left = pad / 2
          val right = pad - left
          " " * left + lbl + " " * right
        }.mkString(" "))
      } else None

      (rows ++ labelRow).mkString("\n")
    }
  }

  def bar(width: Int = 40, height: Int = 10, showLabels: Boolean = true)(bars: Bar*): BarChart =
    BarChart(bars, width, height, showLabels)

  final case class StackedBar(
      segments: Seq[Bar],
      label: String = ""
  )

  final case class StackedBarChart(
      bars: Seq[StackedBar],
      chartWidth: Int = 40,
      chartHeight: Int = 10,
      showLabels: Boolean = true,
      showLegend: Boolean = true,
      showAxis: Boolean = true
  ) extends Element {

    def render: String = {
      if (bars.isEmpty) return "No data"

      val maxVal = bars.map(_.segments.map(_.value).sum).max
      if (maxVal <= 0) return "No data"

      val numBars = bars.length
      val axisWidth = if (showAxis) maxVal.toInt.toString.length + 2 else 0
      val availableWidth = chartWidth - axisWidth
      val barWidth = math.max(1, (availableWidth - numBars + 1) / numBars)

      val allSegments = bars.flatMap(_.segments).filter(_.label.nonEmpty)
      val uniqueLabels = allSegments.map(_.label).distinct
      val labelColors = uniqueLabels.zipWithIndex.map { case (lbl, i) =>
        lbl -> Palette.DEFAULT_COLORS(i % Palette.DEFAULT_COLORS.length)
      }.toMap

      val totalPixelHeight = chartHeight * 8
      val barSegments = bars.map { stackedBar =>
        val values = stackedBar.segments.map(_.value)
        val cumulative = values.scanLeft(0.0)(_ + _)
        stackedBar.segments.zip(cumulative.zip(cumulative.tail)).map {
          case (seg, (startVal, endVal)) =>
            val start = (startVal / maxVal * totalPixelHeight).toInt
            val end = (endVal / maxVal * totalPixelHeight).toInt
            val c = seg.barColor.getOrElse(
              if (seg.label.nonEmpty) labelColors.getOrElse(seg.label, Color.Blue)
              else Color.Blue
            )
            (start, end, c)
        }
      }

      val rows = (0 until chartHeight).map { row =>
        val rowPixelBottom = (chartHeight - row - 1) * 8
        val rowPixelTop = (chartHeight - row) * 8

        val axisLabel = if (showAxis) {
          /* Show tick value at each row */
          val tickValue = maxVal * (chartHeight - row).toDouble / chartHeight
          val tickStr =
            if (tickValue == tickValue.toLong) tickValue.toLong.toString else f"$tickValue%.0f"
          tickStr.reverse.padTo(axisWidth - 1, ' ').reverse + "┤"
        } else ""

        val barsStr = barSegments.zipWithIndex.map { case (segs, i) =>
          var filledEighths = 0
          var topColor: Color = Color.NoColor

          for (eighth <- 0 until 8) {
            val pixelY = rowPixelBottom + eighth
            segs.find { case (start, end, _) => pixelY >= start && pixelY < end } match {
              case Some((_, _, c)) =>
                filledEighths += 1
                topColor = c
              case None =>
            }
          }

          val barStr = if (filledEighths == 0) {
            " " * barWidth
          } else if (filledEighths == 8) {
            wrapAnsi(topColor, "█" * barWidth)
          } else {
            wrapAnsi(topColor, Glyphs.BLOCK_CHARS(filledEighths).toString * barWidth)
          }

          val sep = if (i < numBars - 1) " " else ""
          barStr + sep
        }.mkString

        axisLabel + barsStr
      }

      val labelRow = if (showLabels && bars.exists(_.label.nonEmpty)) {
        val prefix = if (showAxis) " " * axisWidth else ""
        Some(prefix + bars.map { b =>
          val lbl = b.label.take(barWidth)
          val pad = barWidth - lbl.length
          val left = pad / 2
          val right = pad - left
          " " * left + lbl + " " * right
        }.mkString(" "))
      } else None

      val legend = if (showLegend && uniqueLabels.nonEmpty) {
        Some(uniqueLabels.map { lbl =>
          val c = labelColors(lbl)
          s"${wrapAnsi(c, "█")} $lbl"
        }.mkString("  "))
      } else None

      (rows ++ labelRow ++ Seq("") ++ legend).mkString("\n")
    }
  }

  def stackedBar(
      width: Int = 40,
      height: Int = 10,
      showLabels: Boolean = true,
      showLegend: Boolean = true
  )(
      bars: StackedBar*
  ): StackedBarChart =
    StackedBarChart(bars, width, height, showLabels, showLegend)

  final case class Sparkline(values: Seq[Double], sparkColor: Color = Color.NoColor)
      extends Element {
    def render: String = {
      if (values.isEmpty) return ""
      val (lo, hi) = (values.min, values.max)
      val range = if (hi == lo) 1.0 else hi - lo
      val result = values.map { v =>
        Glyphs.BLOCK_CHARS(math.min(7, ((v - lo) / range * 8).toInt) + 1)
      }.mkString
      if (sparkColor == Color.NoColor) result else wrapAnsi(sparkColor, result)
    }
  }

  def sparkline(values: Seq[Double]): Sparkline = Sparkline(values)

  final case class BoxData(
      label: String,
      min: Double,
      q1: Double,
      median: Double,
      q3: Double,
      max: Double,
      boxColor: Option[Color] = None
  ) {
    def color(c: Color): BoxData = copy(boxColor = Some(c))
  }

  final case class BoxPlot(boxes: Seq[BoxData], plotHeight: Int = 15, showLabels: Boolean = true)
      extends Element {
    def render: String = {
      if (boxes.isEmpty) return "No data"

      val (lo, hi) = (boxes.map(_.min).min, boxes.map(_.max).max)
      val range = if (hi == lo) 1.0 else hi - lo
      def scale(v: Double): Int = ((v - lo) / range * (plotHeight - 1)).toInt

      val boxWidth = 5
      val spacing = 2
      val colored = boxes.zipWithIndex.map { case (b, i) =>
        (b, b.boxColor.getOrElse(Palette.DEFAULT_COLORS(i % Palette.DEFAULT_COLORS.length)))
      }

      val rows = (0 until plotHeight).reverse.map { row =>
        colored.map { case (b, c) =>
          val (rMin, rQ1, rMed, rQ3, rMax) =
            (scale(b.min), scale(b.q1), scale(b.median), scale(b.q3), scale(b.max))
          val cell =
            if (row == rMax) " ─┬─ "
            else if (row == rMin) " ─┴─ "
            else if (row > rQ3 && row < rMax) "  │  "
            else if (row > rMin && row < rQ1) "  │  "
            else if (row == rQ3) "┌───┐"
            else if (row == rQ1) "└───┘"
            else if (row > rQ1 && row < rQ3) { if (row == rMed) "├───┤" else "│   │" }
            else "     "
          if (c == Color.NoColor) cell else wrapAnsi(c, cell)
        }.mkString(" " * spacing)
      }

      val (lblTop, lblBot) = (f"$hi%.0f", f"$lo%.0f")
      val lblW = math.max(lblTop.length, lblBot.length)

      val labelRow = if (showLabels) Some(colored.map { case (b, _) =>
        val lbl = b.label.take(boxWidth)
        val pad = boxWidth - lbl.length
        " " * (pad / 2) + lbl + " " * (pad - pad / 2)
      }.mkString(" " * spacing))
      else None

      val axisRows = rows.zipWithIndex.map { case (row, i) =>
        val axis = if (i == 0) lblTop else if (i == plotHeight - 1) lblBot else " " * lblW
        axis.reverse.padTo(lblW, ' ').reverse + " │" + row
      }

      (axisRows ++ labelRow).mkString("\n")
    }
  }

  def boxPlot(height: Int = 15)(boxes: BoxData*): BoxPlot = BoxPlot(boxes, height)

  final case class Histogram(
      data: Seq[Double],
      bins: Int = 10,
      histWidth: Int = 40,
      histHeight: Int = 10
  ) extends Element {
    def render: String = {
      if (data.isEmpty) return "No data"

      val (lo, hi) = (data.min, data.max)
      val range = if (hi == lo) 1.0 else hi - lo
      val binW = range / bins
      val counts = (0 until bins).map { b =>
        data.count { v =>
          val idx = math.min(bins - 1, ((v - lo) / binW).toInt)
          idx == b
        }
      }
      val maxCount = counts.max.toDouble
      val barW = math.max(1, histWidth / bins)

      val rows = (0 until histHeight).reverse.map { row =>
        val thresh = ((row + 1).toDouble / histHeight) * maxCount
        counts.map(c => if (c >= thresh) "█" * barW else " " * barW).mkString
      }

      val w = barW * bins
      val axis = "─" * w
      val loS = f"$lo%.1f"
      val hiS = f"$hi%.1f"
      val labels = loS + " " * (w - loS.length - hiS.length).max(1) + hiS
      (rows :+ axis :+ labels).mkString("\n")
    }
  }

  def histogram(data: Seq[Double], bins: Int = 10): Histogram = Histogram(data, bins)

  final case class HeatmapData(
      rows: Seq[Seq[Double]],
      rowLabels: Seq[String] = Seq.empty,
      colLabels: Seq[String] = Seq.empty
  )

  final case class Heatmap(
      data: HeatmapData,
      cellWidth: Int = 2,
      cellHeight: Int = 1,
      showLegend: Boolean = true
  ) extends Element {
    private def colorCode(n: Double): Int = {
      val t = math.max(0.0, math.min(1.0, n))
      if (t < 0.25) (21 + (51 - 21) * t / 0.25).toInt
      else if (t < 0.5) (51 + (46 - 51) * (t - 0.25) / 0.25).toInt
      else if (t < 0.75) (46 + (226 - 46) * (t - 0.5) / 0.25).toInt
      else (226 + (196 - 226) * (t - 0.75) / 0.25).toInt
    }
    private def bg(code: Int): String = s"\u001b[48;5;${code}m"
    private val rst = "\u001b[0m"

    def render: String = {
      val rows = data.rows
      if (rows.isEmpty) return "No data"

      val all = rows.flatten
      val (lo, hi) = (all.min, all.max)
      val range = if (hi == lo) 1.0 else hi - lo
      val (nCols, nRows) = (rows.map(_.length).max, rows.length)

      val yLbls = if (data.rowLabels.nonEmpty) data.rowLabels else (0 until nRows).map(_.toString)
      val yW = yLbls.map(_.length).max + 1
      val xLbls = if (data.colLabels.nonEmpty) data.colLabels else (0 until nCols).map(_.toString)

      val header =
        " " * yW + xLbls.map(l => l.take(cellWidth).reverse.padTo(cellWidth, ' ').reverse).mkString
      val heatRows = rows.zipWithIndex.flatMap { case (row, ri) =>
        val yLbl = if (ri < yLbls.length) yLbls(ri).padTo(yW, ' ') else " " * yW
        val cells =
          row.map(v => s"${bg(colorCode((v - lo) / range))}${" " * cellWidth}$rst").mkString
        (0 until cellHeight).map(h => if (h == 0) yLbl + cells else " " * yW + cells)
      }
      val legend = if (showLegend) {
        val bar = (0 until 20).map(i => s"${bg(colorCode(i / 19.0))} $rst").mkString
        Seq("", f"$lo%.1f $bar $hi%.1f")
      } else Seq.empty

      (Seq(header) ++ heatRows ++ legend).mkString("\n")
    }
  }

  def heatmap(rows: Seq[Seq[Double]]): Heatmap = Heatmap(HeatmapData(rows))
  def heatmap(data: HeatmapData): Heatmap = Heatmap(data)

  /** Banner - decorative text in a box */
  final case class Banner(content: Element, borderStyle: Border = Border.Double)
      extends Element {

    def render: String = {
      val rendered = content.render
      val lines = if (rendered.isEmpty) Array("") else rendered.split('\n')
      val maxWidth =
        if (lines.isEmpty) 0
        else lines.map(line => realLength(line)).max
      val totalWidth = maxWidth + Dimensions.BOX_INNER_PADDING

      val (tl, tr, bl, br) =
        (borderStyle.topLeft, borderStyle.topRight, borderStyle.bottomLeft, borderStyle.bottomRight)
      val (ht, hb, vl, vr) =
        (borderStyle.hTop, borderStyle.hBottom, borderStyle.vLeft, borderStyle.vRight)

      val top = tl + ht * (totalWidth - Dimensions.BOX_BORDER_WIDTH) + tr
      val bottom = bl + hb * (totalWidth - Dimensions.BOX_BORDER_WIDTH) + br

      val contentLines = lines.map { line =>
        val visibleLength = realLength(line)
        val padding = maxWidth - visibleLength
        val actualPadding = " " * padding
        s"$vl $line$actualPadding $vr"
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

    /** Directional accessors - override for asymmetric borders (e.g. half-block) */
    def topLeft: String = chars._1
    def topRight: String = chars._2
    def bottomLeft: String = chars._3
    def bottomRight: String = chars._4
    def hTop: String = chars._5
    def hBottom: String = chars._5
    def vLeft: String = chars._6
    def vRight: String = chars._6

    /** Apply this border style to an element with HasBorder typeclass */
    def apply[T](element: T)(implicit ev: HasBorder[T]): T =
      ev.setBorder(element, this)

  }

  object Border {

    case object None extends Border {
      val chars = (" ", " ", " ", " ", " ", " ")
    }

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

    case object Ascii extends Border {
      val chars = ("+", "+", "+", "+", "-", "|")
    }

    case object Block extends Border {
      val chars = ("█", "█", "█", "█", "█", "█")
    }

    case object Dashed extends Border {
      val chars = ("┌", "┐", "└", "┘", "╌", "╎")
    }

    case object Dotted extends Border {
      val chars = ("┌", "┐", "└", "┘", "┈", "┊")
    }

    case object InnerHalfBlock extends Border {
      val chars = ("▗", "▖", "▝", "▘", "▄", "▐")
      override def hBottom: String = "▀"
      override def vRight: String = "▌"
    }

    case object OuterHalfBlock extends Border {
      val chars = ("▛", "▜", "▙", "▟", "▀", "▌")
      override def hBottom: String = "▄"
      override def vRight: String = "▐"
    }

    case object Markdown extends Border {
      val chars = ("|", "|", "|", "|", "-", "|")
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
    val None = Border.None
    val Single = Border.Single
    val Double = Border.Double
    val Thick = Border.Thick
    val Round = Border.Round
    val Ascii = Border.Ascii
    val Block = Border.Block
    val Dashed = Border.Dashed
    val Dotted = Border.Dotted
    val InnerHalfBlock = Border.InnerHalfBlock
    val OuterHalfBlock = Border.OuterHalfBlock
    val Markdown = Border.Markdown
  }

  object BorderStyle {
    val None = Border.None
    val Single = Border.Single
    val Double = Border.Double
    val Thick = Border.Thick
    val Round = Border.Round
    val Ascii = Border.Ascii
    val Block = Border.Block
    val Dashed = Border.Dashed
    val Dotted = Border.Dotted
    val InnerHalfBlock = Border.InnerHalfBlock
    val OuterHalfBlock = Border.OuterHalfBlock
    val Markdown = Border.Markdown
  }

  /** Typeclass for elements that have configurable borders */
  trait HasBorder[T] {
    def setBorder(element: T, newStyle: Border): T
  }

  object HasBorder {
    def apply[T](implicit ev: HasBorder[T]): HasBorder[T] = ev

    implicit val tableBorder: HasBorder[Table] = new HasBorder[Table] {
      def setBorder(element: Table, newStyle: Border): Table =
        element.copy(borderStyle = newStyle)
    }

    implicit val statusCardBorder: HasBorder[StatusCard] =
      new HasBorder[StatusCard] {
        def setBorder(element: StatusCard, newStyle: Border): StatusCard =
          element.copy(borderStyle = newStyle)
      }

    implicit val boxBorder: HasBorder[Box] = new HasBorder[Box] {
      def setBorder(element: Box, newStyle: Border): Box =
        element.copy(borderStyle = newStyle)
    }

    implicit val bannerBorder: HasBorder[Banner] = new HasBorder[Banner] {
      def setBorder(element: Banner, newStyle: Border): Banner =
        element.copy(borderStyle = newStyle)
    }

    implicit val styledBorder: HasBorder[Styled] = new HasBorder[Styled] {
      def setBorder(element: Styled, newStyle: Border): Styled =
        element.element match {
          case t: Table =>
            element.copy(element = tableBorder.setBorder(t, newStyle))
          case sc: StatusCard =>
            element.copy(element = statusCardBorder.setBorder(sc, newStyle))
          case b: Box =>
            element.copy(element = boxBorder.setBorder(b, newStyle))
          case bn: Banner =>
            element.copy(element = bannerBorder.setBorder(bn, newStyle))
          case s: Styled =>
            element.copy(element = styledBorder.setBorder(s, newStyle))
          case c: Colored =>
            element.copy(element = coloredBorder.setBorder(c, newStyle))
          case _ => element
        }
    }

    implicit val coloredBorder: HasBorder[Colored] = new HasBorder[Colored] {
      def setBorder(element: Colored, newStyle: Border): Colored =
        element.element match {
          case t: Table =>
            element.copy(element = tableBorder.setBorder(t, newStyle))
          case sc: StatusCard =>
            element.copy(element = statusCardBorder.setBorder(sc, newStyle))
          case b: Box =>
            element.copy(element = boxBorder.setBorder(b, newStyle))
          case bn: Banner =>
            element.copy(element = bannerBorder.setBorder(bn, newStyle))
          case s: Styled =>
            element.copy(element = styledBorder.setBorder(s, newStyle))
          case c: Colored =>
            element.copy(element = coloredBorder.setBorder(c, newStyle))
          case _ => element
        }
    }

  }

  implicit class BorderOps[T](val element: T) extends AnyVal {

    def border(style: Border)(implicit ev: HasBorder[T]): T =
      ev.setBorder(element, style)

  }

  /** Box - bordered container with optional title */
  final case class Box(
      title: String = "",
      elements: Seq[Element],
      borderStyle: Border = Border.Single
  ) extends Element {

    def render: String = {
      /* Combine all elements into a single layout */
      val content =
        if (elements.length == 1) elements.head else Layout(elements)
      val contentLines = content.render.split('\n')
      val contentWidth =
        if (contentLines.isEmpty) 0
        else contentLines.map(line => realLength(line)).max
      val titleWidth =
        if (title.nonEmpty) title.length + Dimensions.MIN_CONTENT_PADDING else 0
      val innerWidth = math.max(contentWidth, titleWidth)
      val totalWidth = innerWidth + Dimensions.BOX_INNER_PADDING

      val (tl, tr, bl, br) =
        (borderStyle.topLeft, borderStyle.topRight, borderStyle.bottomLeft, borderStyle.bottomRight)
      val (ht, hb, vl, vr) =
        (borderStyle.hTop, borderStyle.hBottom, borderStyle.vLeft, borderStyle.vRight)

      val topBorder = if (title.nonEmpty) {
        val titlePadding =
          totalWidth - title.length - Dimensions.BOX_BORDER_WIDTH
        val leftPad = titlePadding / 2
        val rightPad = titlePadding - leftPad
        s"$tl${ht * leftPad}$title${ht * rightPad}$tr"
      } else {
        s"$tl${ht * (totalWidth - Dimensions.BOX_BORDER_WIDTH)}$tr"
      }

      val bottomBorder =
        s"$bl${hb * (totalWidth - Dimensions.BOX_BORDER_WIDTH)}$br"

      val paddedContent = contentLines.map { line =>
        val padding = innerWidth - realLength(line)
        s"$vl $line${" " * padding} $vr"
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
    ): Seq[Array[String]] =
      renderedElements.zip(elementWidths).map { case (lines, width) =>
        val paddedLines = lines ++ Array.fill(maxHeight - lines.length)("")
        paddedLines.map(line => line.padTo(width, Glyphs.SPACE.head))
      }

  }

  final case class RowTight(elements: Seq[Element]) extends Element {
    def render: String = elements.map(_.render).mkString
  }

  /** Tree structure GADT for hierarchical data visualization. Supports both branch nodes (with
    * children) and leaf nodes (terminals).
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

    def render(node: TreeNode, prefix: String, isLast: Boolean): String =
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

  /** Root layout container */
  final case class Layout(elements: Seq[Element]) extends Element {

    def render: String = {
      /* Calculate layout max width for auto-centering */
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

  /** Create a titled section with custom separator and flanking character count.
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
  def kv(pairs: (Element, Element)*): KeyValue = KeyValue(pairs)

  implicit def stringPairToElementPair(p: (String, String)): (Element, Element) =
    (Text(p._1), Text(p._2))

  implicit def stringElementPairToElementPair(p: (String, Element)): (Element, Element) =
    (Text(p._1), p._2)

  implicit def elementStringPairToElementPair(p: (Element, String)): (Element, Element) =
    (p._1, Text(p._2))

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

  /** Row without spacing between elements */
  def rowTight(elements: Element*): RowTight = RowTight(elements)

  /** Tight row - concatenates elements horizontally without spacing */
  def tightRow(elements: Element*): Element = new Element {
    def render: String = {
      if (elements.isEmpty) return ""
      val renderedElements = elements.map(_.render.split('\n'))
      val maxHeight = renderedElements.map(_.length).max

      (0 until maxHeight)
        .map { lineIndex =>
          renderedElements
            .map { lines =>
              if (lineIndex < lines.length) lines(lineIndex) else ""
            }
            .mkString("")
        }
        .mkString("\n")
    }
  }

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

  /** Single line break */
  def br: LineBreak.type = LineBreak

  /** Multiple line breaks */
  def br(n: Int): Element =
    if (n <= 0) Text("")
    else if (n == 1) LineBreak
    else Layout(List.fill(n)(LineBreak))

  /** Single space */
  def space: Element = Text(" ")

  /** Spaces (default: 1) */
  def space(n: Int = 1): Element =
    if (n <= 0) Text("")
    else Text(" " * n)

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
          val visibleLength = realLength(line)
          if (visibleLength <= maxWidth) line
          else {
            val truncateAt = maxWidth - ellipsis.length
            if (truncateAt <= 0) ellipsis.take(maxWidth)
            else {
              /* ANSI escapes would break mid-truncation, strip first */
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

  /** Create a fluent horizontal rule builder. Use .width() and .char() to customize.
    *
    * @example
    *   {{{hr.width(40).char("═")}}}
    */
  def hr: HorizontalRuleBuilder = HorizontalRuleBuilder()

  /** Interactive text input field */
  def textInput(
      label: String,
      value: String = "",
      placeholder: String = "",
      active: Boolean = false
  ): TextInput = TextInput(label, value, placeholder, active)

  /** Single choice selector - pick one option from a list */
  def singleChoice(
      label: String,
      options: Seq[String],
      selected: Int = 0,
      active: Boolean = false
  ): SingleChoice = SingleChoice(label, options, selected, active)

  /** Multi choice selector - pick multiple options from a list */
  def multiChoice(
      label: String,
      options: Seq[String],
      selected: Set[Int] = Set.empty,
      cursor: Int = 0,
      active: Boolean = false
  ): MultiChoice = MultiChoice(label, options, selected, cursor, active)

  /** Form builder - combines multiple input fields */
  def form(
      title: String,
      fields: Element*
  ): Form = Form(title, fields)

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
    *   an empty Banner with default double-line borders (use .border() to change)
    */
  def banner(): Banner = Banner(Text(""), Border.Double)

  /* ===========================================================================
   * TEXT FORMATTING
   */

  /** Add underline to an element */
  def underline(
      char: Element = Text("─"),
      color: Color = Color.NoColor
  )(element: Element): Underline =
    Underline(element, char.render, if (color == Color.NoColor) None else Some(color))

  @deprecated("Use underline(char, color) instead", "0.6.0")
  def underlineColored(char: Element, color: Color)(element: Element): Underline =
    underline(char, color)(element)

  def style(s: Style)(element: Element): Styled = Styled(s, element)

  /** Ordered (numbered) list */
  def ol(items: Element*): OrderedList = OrderedList(items)

  /** Unordered (bulleted) list with default bullets */
  def ul(items: Element*): UnorderedList = UnorderedList(items)

  /** Unordered list with custom bullet character */
  def ul(bullet: String)(items: Element*): UnorderedList =
    UnorderedList(items, bullet)

  /* ===========================================================================
   * ALIGNMENT
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

  /* ===========================================================================
   * MARGINS
   */

  /** Add a prefix margin to elements */
  def margin(prefix: Element)(elements: Element*): Margin =
    Margin(prefix.render, elements, None)

  def marginColor(prefix: Element, color: Color)(elements: Element*): Margin =
    Margin(prefix.render, elements, Some(color))

  /* ===========================================================================
   * IMPLICITS
   */

  /** Automatic conversion from String to Text element. Allows using strings directly wherever
    * Elements are expected.
    *
    * @param s
    *   the string to convert
    * @return
    *   a Text element containing the string
    */
  implicit def stringToText(s: String): Text = Text(s)

  /** Automatic conversion from Seq[String] to Seq[Element]. Allows using string sequences directly
    * with varargs expansion.
    *
    * @param strings
    *   the sequence of strings to convert
    * @return
    *   a sequence of Text elements
    */
  implicit def stringSeqToElementSeq(strings: Seq[String]): Seq[Element] =
    strings.map(Text(_))

}
