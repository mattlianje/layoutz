/*
 * +==========================================================================+
 * |                                layoutz                                   |
 * |                  Friendly, expressive print-layout DSL                   |
 * |                            Version 0.0.1                                 |
 * |                 Compatible with Scala 2.12, 2.13, and 3                  |
 * |                                                                          |
 * | Copyright 2025 Matthieu Court (matthieu.court@protonmail.com)            |
 * | Apache License 2.0                                                       |
 * +==========================================================================+
 */
package object layoutz {
  import scala.language.implicitConversions

  /** Styling constants */
  private object Dimensions {
    val MinContentPadding = 2
    val BorderThickness = 2 // For "┌" and "┐"
    val SidePadding = 2 // For "│ "
    val ProgressBarWidth = 20
    val TreeIndentation = 4 // Standard tree level indentation
    val TreeConnectorSpacing = 3 // Spaces after "│" for tree continuation
  }

  private object Glyphs {

    /** Box drawing characters */
    val TopLeft = "┌"
    val TopRight = "┐"
    val BottomLeft = "└"
    val BottomRight = "┘"
    val Horizontal = "─"
    val Vertical = "│"
    val Cross = "┼"
    val TeeDown = "┬"
    val TeeUp = "┴"
    val TeeRight = "├"
    val TeeLeft = "┤"

    /** Content markers */
    val Bullet = "•"
    val Space = " "
    val BarFilled = "█"
    val BarEmpty = "─"

    /** Tree structure */
    val TreeBranch = "├──"
    val TreeLastBranch = "└──"
    val TreeVertical = "│"
    val TreeIndent = " " * Dimensions.TreeIndentation
  }

  /** Core ADT for renderable layout elements. All elements can be rendered to
    * ASCII strings with known dimensions.
    */
  sealed trait Element {
    def render: String

    final def width: Int = {
      val lines = render.split('\n')
      if (lines.isEmpty) 0 else lines.map(_.length).max
    }

    final def height: Int =
      render.split('\n').length
  }

  /** Atomic text content */
  final case class Text(content: String) extends Element {
    def render: String = content
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
  final case class Table(headers: Seq[String], rows: Seq[Seq[String]])
      extends Element {
    def render: String = {
      val allRows = headers +: rows
      val columnWidths = calculateColumnWidths(allRows)

      val borders = TableBorders(columnWidths)
      val headerRow = buildTableRow(headers, columnWidths)
      val dataRows = rows.map(row => buildTableRow(row, columnWidths))

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
      def apply(widths: Seq[Int]): TableBorders = {
        val segments = widths.map(Glyphs.Horizontal * _)
        TableBorders(
          top = segments.mkString(
            s"${Glyphs.TopLeft}${Glyphs.Horizontal}",
            s"${Glyphs.Horizontal}${Glyphs.TeeDown}${Glyphs.Horizontal}",
            s"${Glyphs.Horizontal}${Glyphs.TopRight}"
          ),
          separator = segments.mkString(
            s"${Glyphs.TeeRight}${Glyphs.Horizontal}",
            s"${Glyphs.Horizontal}${Glyphs.Cross}${Glyphs.Horizontal}",
            s"${Glyphs.Horizontal}${Glyphs.TeeLeft}"
          ),
          bottom = segments.mkString(
            s"${Glyphs.BottomLeft}${Glyphs.Horizontal}",
            s"${Glyphs.Horizontal}${Glyphs.TeeUp}${Glyphs.Horizontal}",
            s"${Glyphs.Horizontal}${Glyphs.BottomRight}"
          )
        )
      }
    }

    private def buildTableRow(cells: Seq[String], widths: Seq[Int]): String =
      cells
        .zip(widths)
        .map { case (cell, width) =>
          cell.padTo(width, Glyphs.Space.head)
        }
        .mkString(
          s"${Glyphs.Vertical} ",
          s" ${Glyphs.Vertical} ",
          s" ${Glyphs.Vertical}"
        )
  }

  /** Bulleted list of items */
  final case class Bullets(items: Seq[String]) extends Element {
    def render: String =
      items.map(item => s"${Glyphs.Bullet} $item").mkString("\n")
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

  /** Bordered container with title */
  final case class Box(title: String, content: Element) extends Element {
    def render: String = {
      val contentLines = content.render.split('\n')
      val dimensions = BoxDimensions.calculate(title, contentLines)
      val borders = BoxBorders(title, dimensions)
      val paddedContent =
        contentLines.map(line => formatBoxContent(line, dimensions))

      (borders.top +: paddedContent :+ borders.bottom).mkString("\n")
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
  final case class Section(title: String, content: Element) extends Element {
    def render: String = s"=== $title ===\n${content.render}"
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

  /** DSL constructors - the public API for layout composition */
  def layout(elements: Element*): Layout = Layout(elements)
  def section(title: String)(content: Element): Section =
    Section(title, content)
  def kv(pairs: (String, String)*): KeyValue = KeyValue(pairs)
  def table(headers: Seq[String], rows: Seq[Seq[String]]): Table =
    Table(headers, rows)
  def bullets(items: String*): Bullets = Bullets(items)
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
  def box(title: String)(content: Element): Box = Box(title, content)
  def row(elements: Element*): Row = Row(elements)
  def tree(title: String)(root: TreeNode): Tree = Tree(title, root)
  def branch(name: String, children: TreeNode*): TreeBranch =
    TreeBranch(name, children)
  def leaf(name: String): TreeLeaf = TreeLeaf(name)

  /** Implicit conversions for ergonomic DSL usage */
  implicit def stringToText(s: String): Text = Text(s)
}
