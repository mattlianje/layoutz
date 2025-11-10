/*
 * +==========================================================================+
 * |                                layoutz                                   |
 * |            Friendly, expressive print-layout DSL for JS                  |
 * |                            Version 0.0.2                                 |
 * |                    Compatible with Node.js and browsers                  |
 * |                                                                          |
 * | Copyright 2025 Matthieu Court (matthieu.court@protonmail.com)            |
 * | Apache License 2.0                                                       |
 * |                                                                          |
 * | TypeScript/JavaScript port of the Scala layoutz library                 |
 * +==========================================================================+
 */

const DIMENSIONS = {
  MIN_CONTENT_PADDING: 2,
  BORDER_THICKNESS: 2,
  SIDE_PADDING: 2,
  PROGRESS_BAR_WIDTH: 20,
  TREE_INDENTATION: 4,
  TREE_CONNECTOR_SPACING: 3,
  DEFAULT_RULE_WIDTH: 50,
  DEFAULT_CHART_WIDTH: 40,
  CHART_LABEL_MAX_WIDTH: 15,
  CHART_LABEL_SPACING: 15,
  BOX_INNER_PADDING: 4,
  BOX_BORDER_WIDTH: 2,
} as const;

const GLYPHS = {
  TOP_LEFT: "┌",
  TOP_RIGHT: "┐",
  BOTTOM_LEFT: "└",
  BOTTOM_RIGHT: "┘",
  HORIZONTAL: "─",
  VERTICAL: "│",
  CROSS: "┼",
  TEE_DOWN: "┬",
  TEE_UP: "┴",
  TEE_RIGHT: "├",
  TEE_LEFT: "┤",

  BULLET: "•",
  SPACE: " ",
  BAR_FILLED: "█",
  BAR_EMPTY: "─",

  TREE_BRANCH: "├──",
  TREE_LAST_BRANCH: "└──",
  TREE_VERTICAL: "│",
  TREE_INDENT: " ".repeat(DIMENSIONS.TREE_INDENTATION),
} as const;

// ANSI escape sequence regex for stripping color codes
const ANSI_ESCAPE_REGEX = /\u001b\[[0-9;]*m/g;

/**
 * Strip ANSI escape sequences to get visual width
 */
function stripAnsiCodes(text: string): string {
  return text.replace(ANSI_ESCAPE_REGEX, "");
}

/**
 * Get the display width of a single character
 * Accounts for wide characters (CJK, emoji) that take 2 columns
 */
function charWidth(char: string): number {
  const codePoint = char.codePointAt(0);
  if (!codePoint) return 0;

  if (codePoint < 0x0300) return 1; // Fast path for ASCII and common Latin
  if (codePoint >= 0x0300 && codePoint < 0x0370) return 0; // Combining diacriticals
  if (codePoint >= 0x1100 && codePoint < 0x1200) return 2; // Hangul Jamo
  if (codePoint >= 0x2e80 && codePoint < 0x9fff) return 2; // CJK
  if (codePoint >= 0xac00 && codePoint < 0xd7a4) return 2; // Hangul Syllables
  if (codePoint >= 0xf900 && codePoint < 0xfb00) return 2; // CJK Compatibility Ideographs
  if (codePoint >= 0xfe10 && codePoint < 0xfe20) return 2; // Vertical forms
  if (codePoint >= 0xfe30 && codePoint < 0xfe70) return 2; // CJK Compatibility Forms
  if (codePoint >= 0xff00 && codePoint < 0xff61) return 2; // Fullwidth Forms
  if (codePoint >= 0xffe0 && codePoint < 0xffe7) return 2; // Fullwidth symbols
  if (codePoint >= 0x1f000) return 2; // Emoji, symbols, supplementary ideographs
  if (codePoint >= 0x20000 && codePoint < 0x2ffff) return 2; // Supplementary ideographs
  if (codePoint >= 0x30000 && codePoint < 0x3ffff) return 2; // Tertiary ideographs
  return 1;
}

/**
 * Get the real display length of a string, accounting for:
 * - ANSI escape codes (removed)
 * - Wide characters (CJK, emoji) that take 2 columns
 * - Combining characters that take 0 columns
 */
function realLength(text: string): number {
  const stripped = stripAnsiCodes(text);
  let length = 0;
  for (const char of stripped) {
    length += charWidth(char);
  }
  return length;
}

/**
 * Flatten multiline elements to single line for components that need single-line content
 */
function flattenToSingleLine(element: Element): string {
  return element.render().split("\n").join(" ");
}

/**
 * Core layout element interface
 */
export interface Element {
  render(): string;
}

/**
 * Get the visual width of an element (longest line, excluding ANSI codes)
 */
export function getWidth(element: Element): number {
  const rendered = element.render();
  if (!rendered) return 0;

  const lines = rendered.split("\n");
  return Math.max(...lines.map((line) => stripAnsiCodes(line).length));
}

/**
 * Get the height of an element (number of lines)
 */
export function getHeight(element: Element): number {
  const rendered = element.render();
  if (!rendered) return 1;
  return rendered.split("\n").length;
}

/**
 * Simple text element
 */
export class Text implements Element {
  constructor(private content: string) {}

  render(): string {
    return this.content;
  }

  color(colorType: Color): Colored {
    return new Colored(this, colorType);
  }

  style(styleType: Style): Styled {
    return new Styled(this, styleType);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }
}

/**
 * Line break element
 */
export class LineBreak implements Element {
  render(): string {
    return "\n";
  }
}

/**
 * Colored element wrapper
 */
export class Colored implements Element {
  constructor(
    private element: Element,
    private colorType: Color
  ) {}

  render(): string {
    const code = getColorCode(this.colorType);
    const content = this.element.render();
    if (!code) return content;

    const lines = content.split("\n");
    return lines.map((line) => wrapAnsi(line, code)).join("\n");
  }

  color(newColor: Color): Colored {
    return new Colored(this.element, newColor);
  }

  style(styleParam: Style): Styled {
    return new Styled(this, styleParam);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }
}

/**
 * Styled element wrapper
 */
export class Styled implements Element {
  constructor(
    private element: Element,
    private styleType: Style
  ) {}

  render(): string {
    const code = getStyleCode(this.styleType);
    const content = this.element.render();
    if (!code) return content;

    const lines = content.split("\n");
    return lines.map((line) => wrapAnsi(line, code)).join("\n");
  }

  color(colorParam: Color): Colored {
    return new Colored(this, colorParam);
  }

  style(newStyle: Style): Styled {
    return new Styled(this, newStyle);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }
}

/**
 * Horizontal rule
 */
export class HorizontalRule implements Element {
  constructor(
    private char: string = "─",
    private ruleWidth?: number
  ) {}

  render(): string {
    const width = this.ruleWidth ?? DIMENSIONS.DEFAULT_RULE_WIDTH;
    return this.char.repeat(width);
  }
}

/**
 * Border styles for box-like elements
 */
export enum BorderStyle {
  Single = "single",
  Double = "double",
  Thick = "thick",
  Round = "round",
}

/**
 * Border enum for fluent API (Scala-style)
 */
export const Border = {
  Single: BorderStyle.Single,
  Double: BorderStyle.Double,
  Thick: BorderStyle.Thick,
  Round: BorderStyle.Round,
} as const;

export type BorderType = (typeof Border)[keyof typeof Border];

/**
 * ANSI Color codes
 */
export enum Color {
  Black = "black",
  Red = "red",
  Green = "green",
  Yellow = "yellow",
  Blue = "blue",
  Magenta = "magenta",
  Cyan = "cyan",
  White = "white",
  BrightBlack = "brightBlack",
  BrightRed = "brightRed",
  BrightGreen = "brightGreen",
  BrightYellow = "brightYellow",
  BrightBlue = "brightBlue",
  BrightMagenta = "brightMagenta",
  BrightCyan = "brightCyan",
  BrightWhite = "brightWhite",
  NoColor = "noColor",
}

function getColorCode(color: Color): string {
  const codes: Record<Color, string> = {
    [Color.Black]: "30",
    [Color.Red]: "31",
    [Color.Green]: "32",
    [Color.Yellow]: "33",
    [Color.Blue]: "34",
    [Color.Magenta]: "35",
    [Color.Cyan]: "36",
    [Color.White]: "37",
    [Color.BrightBlack]: "90",
    [Color.BrightRed]: "91",
    [Color.BrightGreen]: "92",
    [Color.BrightYellow]: "93",
    [Color.BrightBlue]: "94",
    [Color.BrightMagenta]: "95",
    [Color.BrightCyan]: "96",
    [Color.BrightWhite]: "97",
    [Color.NoColor]: "",
  };
  return codes[color];
}

/**
 * ANSI Style codes
 */
export enum Style {
  Bold = "bold",
  Dim = "dim",
  Italic = "italic",
  Underline = "underline",
  Blink = "blink",
  Reverse = "reverse",
  Hidden = "hidden",
  Strikethrough = "strikethrough",
  NoStyle = "noStyle",
}

function getStyleCode(style: Style): string {
  const codes: Record<Style, string> = {
    [Style.Bold]: "1",
    [Style.Dim]: "2",
    [Style.Italic]: "3",
    [Style.Underline]: "4",
    [Style.Blink]: "5",
    [Style.Reverse]: "7",
    [Style.Hidden]: "8",
    [Style.Strikethrough]: "9",
    [Style.NoStyle]: "",
  };
  return codes[style];
}

function wrapAnsi(content: string, code: string): string {
  if (!code) return content;
  return `\u001b[${code}m${content}\u001b[0m`;
}

export type BorderChars = {
  topLeft: string;
  topRight: string;
  bottomLeft: string;
  bottomRight: string;
  horizontal: string;
  vertical: string;
};

function getBorderChars(style: BorderStyle): BorderChars {
  switch (style) {
    case BorderStyle.Single:
      return {
        topLeft: "┌",
        topRight: "┐",
        bottomLeft: "└",
        bottomRight: "┘",
        horizontal: "─",
        vertical: "│",
      };
    case BorderStyle.Double:
      return {
        topLeft: "╔",
        topRight: "╗",
        bottomLeft: "╚",
        bottomRight: "╝",
        horizontal: "═",
        vertical: "║",
      };
    case BorderStyle.Thick:
      return {
        topLeft: "┏",
        topRight: "┓",
        bottomLeft: "┗",
        bottomRight: "┛",
        horizontal: "━",
        vertical: "┃",
      };
    case BorderStyle.Round:
      return {
        topLeft: "╭",
        topRight: "╮",
        bottomLeft: "╰",
        bottomRight: "╯",
        horizontal: "─",
        vertical: "│",
      };
  }
}

/**
 * Key-value pairs with aligned formatting
 */
export class KeyValue implements Element {
  constructor(private pairs: Array<[string, string]>) {}

  render(): string {
    if (this.pairs.length === 0) return "";

    const maxKeyLength = Math.max(
      ...this.pairs.map(([key]) => realLength(key))
    );
    const alignmentPosition = maxKeyLength + 2;

    return this.pairs
      .map(([key, value]) => {
        const keyWithColon = `${key}:`;
        const visualLength = realLength(keyWithColon);
        const spacesNeeded = alignmentPosition - visualLength;
        const padding = " ".repeat(Math.max(1, spacesNeeded));
        return `${keyWithColon}${padding}${value}`;
      })
      .join("\n");
  }
}

/**
 * Unordered list with bullet points
 */
export class UnorderedList implements Element {
  private static readonly BULLET_STYLES = ["•", "◦", "▪"];

  constructor(
    private items: Element[],
    private bullet: string = "•"
  ) {}

  render(): string {
    return this.renderAtLevel(0);
  }

  private renderAtLevel(level: number): string {
    if (this.items.length === 0) return "";

    const currentBullet =
      this.bullet === "•"
        ? UnorderedList.BULLET_STYLES[
            level % UnorderedList.BULLET_STYLES.length
          ]
        : this.bullet;

    const result: string[] = [];

    for (const item of this.items) {
      if (item instanceof UnorderedList) {
        const nestedContent = item.renderAtLevel(level + 1);
        if (nestedContent) {
          result.push(nestedContent);
        }
      } else {
        const content = item.render();
        const lines = content.split("\n");
        const indent = "  ".repeat(level);

        if (lines.length === 1) {
          result.push(`${indent}${currentBullet} ${lines[0]}`);
        } else {
          const firstLine = `${indent}${currentBullet} ${lines[0]}`;
          const lineIndent = indent + " ".repeat(currentBullet.length + 1);
          const remainingLines = lines
            .slice(1)
            .map((line) => `${lineIndent}${line}`);
          result.push([firstLine, ...remainingLines].join("\n"));
        }
      }
    }

    return result.join("\n");
  }
}

/**
 * Tree structure for hierarchical displays
 */
export class Tree implements Element {
  constructor(
    private label: string,
    private children: Tree[] = []
  ) {}

  color(colorType: Color): Colored {
    return new Colored(this, colorType);
  }

  style(styleType: Style): Styled {
    return new Styled(this, styleType);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }

  render(): string {
    return this.renderAtLevel(0, true, []);
  }

  private renderAtLevel(
    level: number,
    isLast: boolean,
    parentPrefixes: boolean[]
  ): string {
    if (level === 0 && this.children.length === 0) {
      return this.label;
    }

    let result = "";

    if (level === 0) {
      result += this.label;
    } else {
      const prefix = this.buildPrefix(parentPrefixes, isLast);
      const suffix = this.children.length > 0 ? "/" : "";
      result += prefix + this.label + suffix;
    }

    // Add children
    if (this.children.length > 0) {
      result += "\n";
      const newParentPrefixes = [...parentPrefixes];
      if (level > 0) {
        newParentPrefixes.push(!isLast);
      }

      this.children.forEach((child, index) => {
        const isChildLast = index === this.children.length - 1;
        // Create a new Tree instance to handle the child rendering properly
        const childTree = new Tree(child.label, child.children);
        result += childTree.renderAtLevel(
          level + 1,
          isChildLast,
          newParentPrefixes
        );

        // Add newline between children, but not after the last one
        if (!isChildLast) {
          result += "\n";
        }
      });
    }

    return result;
  }

  private buildPrefix(parentPrefixes: boolean[], isLast: boolean): string {
    let prefix = "";

    // Add parent connection lines
    for (const hasMore of parentPrefixes) {
      if (hasMore) {
        prefix += "│   ";
      } else {
        prefix += "    ";
      }
    }

    // Add current level connector
    if (isLast) {
      prefix += "└── ";
    } else {
      prefix += "├── ";
    }

    return prefix;
  }
}

/**
 * Ordered list with numbered items
 */
export class OrderedList implements Element {
  constructor(private items: Element[]) {}

  render(): string {
    return this.renderAtLevel(0);
  }

  private renderAtLevel(level: number): string {
    if (this.items.length === 0) return "";

    let itemNumber = 0;

    return this.items
      .map((item) => {
        if (item instanceof OrderedList) {
          return item.renderAtLevel(level + 1);
        } else {
          const number = this.getNumbering(itemNumber, level);
          itemNumber++;
          const content = item.render();
          const lines = content.split("\n");
          const indent = "  ".repeat(level);

          if (lines.length === 1) {
            return `${indent}${number}. ${lines[0]}`;
          } else {
            const firstLine = `${indent}${number}. ${lines[0]}`;
            const lineIndent = indent + " ".repeat(number.length + 2);
            const remainingLines = lines
              .slice(1)
              .map((line) => `${lineIndent}${line}`);
            return [firstLine, ...remainingLines].join("\n");
          }
        }
      })
      .join("\n");
  }

  private getNumbering(index: number, level: number): string {
    switch (level % 3) {
      case 0:
        return (index + 1).toString();
      case 1:
        return String.fromCharCode(97 + index); // a, b, c...
      case 2:
        return this.toRomanNumeral(index + 1);
      default:
        return (index + 1).toString();
    }
  }

  private toRomanNumeral(n: number): string {
    const mappings: Array<[number, string]> = [
      [10, "x"],
      [9, "ix"],
      [5, "v"],
      [4, "iv"],
      [1, "i"],
    ];

    let result = "";
    let num = n;

    for (const [value, symbol] of mappings) {
      while (num >= value) {
        result += symbol;
        num -= value;
      }
    }

    return result;
  }
}

/**
 * Box container with optional title and borders
 */
export class Box implements Element {
  constructor(
    private elements: Element[],
    private title: string = "",
    private borderStyle: BorderStyle = BorderStyle.Single
  ) {}

  /**
   * Set the border style (fluent API)
   */
  border(style: BorderType): Box {
    return new Box(this.elements, this.title, style as BorderStyle);
  }

  color(colorType: Color): Colored {
    return new Colored(this, colorType);
  }

  style(styleType: Style): Styled {
    return new Styled(this, styleType);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }

  render(): string {
    const content =
      this.elements.length === 1 ? this.elements[0] : new Layout(this.elements);

    const contentLines = content.render().split("\n");
    const contentWidth =
      contentLines.length === 0
        ? 0
        : Math.max(...contentLines.map((line) => stripAnsiCodes(line).length));

    const titleWidth = this.title
      ? this.title.length + DIMENSIONS.MIN_CONTENT_PADDING
      : 0;
    const innerWidth = Math.max(contentWidth, titleWidth);
    const totalWidth = innerWidth + DIMENSIONS.BOX_INNER_PADDING;

    const chars = getBorderChars(this.borderStyle);

    const topBorder = this.title
      ? (() => {
          const titlePadding =
            totalWidth - this.title.length - DIMENSIONS.BOX_BORDER_WIDTH;
          const leftPad = Math.floor(titlePadding / 2);
          const rightPad = titlePadding - leftPad;
          return `${chars.topLeft}${chars.horizontal.repeat(leftPad)}${this.title}${chars.horizontal.repeat(rightPad)}${chars.topRight}`;
        })()
      : `${chars.topLeft}${chars.horizontal.repeat(totalWidth - DIMENSIONS.BOX_BORDER_WIDTH)}${chars.topRight}`;

    const bottomBorder = `${chars.bottomLeft}${chars.horizontal.repeat(totalWidth - DIMENSIONS.BOX_BORDER_WIDTH)}${chars.bottomRight}`;

    const paddedContent = contentLines.map((line) => {
      const padding = innerWidth - stripAnsiCodes(line).length;
      return `${chars.vertical} ${line}${" ".repeat(padding)} ${chars.vertical}`;
    });

    return [topBorder, ...paddedContent, bottomBorder].join("\n");
  }
}

/**
 * Section with title header
 */
export class Section implements Element {
  constructor(
    private title: string,
    private content: Element,
    private glyph: string = "=",
    private flankingChars: number = 3
  ) {}

  render(): string {
    const header = `${this.glyph.repeat(this.flankingChars)} ${this.title} ${this.glyph.repeat(this.flankingChars)}`;
    return `${header}\n${this.content.render()}`;
  }
}

/**
 * Horizontal layout of elements
 */
export class Row implements Element {
  constructor(private elements: Element[]) {}

  render(): string {
    if (this.elements.length === 0) return "";

    const renderedElements = this.elements.map((el) => el.render().split("\n"));
    const maxHeight = Math.max(
      ...renderedElements.map((lines) => lines.length)
    );
    const elementWidths = this.elements.map((el) => getWidth(el));

    const paddedElements = renderedElements.map((lines, i) => {
      const width = elementWidths[i];
      const paddedLines = [...lines];

      // Pad with empty lines to match max height
      while (paddedLines.length < maxHeight) {
        paddedLines.push("");
      }

      // Pad each line to element width
      return paddedLines.map((line) => line.padEnd(width, " "));
    });

    const result: string[] = [];
    for (let row = 0; row < maxHeight; row++) {
      const rowContent = paddedElements.map((lines) => lines[row]).join(" ");
      result.push(rowContent.trimEnd());
    }

    return result.join("\n");
  }

  color(colorType: Color): Colored {
    return new Colored(this, colorType);
  }

  style(styleType: Style): Styled {
    return new Styled(this, styleType);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }
}

/**
 * Vertical layout container
 */
export class Layout implements Element {
  constructor(private elements: Element[]) {}

  render(): string {
    // Calculate layout max width for auto-centering
    const layoutWidth = this.calculateLayoutWidth();

    // Resolve AutoCentered elements to Centered with calculated width
    const resolvedElements = this.elements.map((el) => {
      if (el instanceof AutoCentered) {
        return new Center(el.element, layoutWidth);
      }
      return el;
    });

    return resolvedElements.map((el) => el.render()).join("\n");
  }

  private calculateLayoutWidth(): number {
    const widths = this.elements.map((el) => {
      if (el instanceof AutoCentered) {
        return getWidth(el.element);
      }
      return getWidth(el);
    });
    return widths.length > 0
      ? Math.max(...widths)
      : DIMENSIONS.DEFAULT_RULE_WIDTH;
  }

  color(colorType: Color): Colored {
    return new Colored(this, colorType);
  }

  style(styleType: Style): Styled {
    return new Styled(this, styleType);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }
}

/**
 * Progress bar element
 */
export class InlineBar implements Element {
  constructor(
    private label: Element,
    private progress: number
  ) {}

  render(): string {
    const clampedProgress = Math.max(0, Math.min(1, this.progress));
    const filledSegments = Math.floor(
      clampedProgress * DIMENSIONS.PROGRESS_BAR_WIDTH
    );
    const emptySegments = DIMENSIONS.PROGRESS_BAR_WIDTH - filledSegments;

    const bar =
      GLYPHS.BAR_FILLED.repeat(filledSegments) +
      GLYPHS.BAR_EMPTY.repeat(emptySegments);
    const percentage = Math.floor(clampedProgress * 100);

    return `${flattenToSingleLine(this.label)} [${bar}] ${percentage}%`;
  }
}

/**
 * Status card widget
 */
export class StatusCard implements Element {
  constructor(
    private label: Element,
    private content: Element,
    private borderStyle: BorderStyle = BorderStyle.Single
  ) {}

  /**
   * Set the border style (fluent API)
   */
  border(style: BorderType): StatusCard {
    return new StatusCard(this.label, this.content, style as BorderStyle);
  }

  color(colorType: Color): Colored {
    return new Colored(this, colorType);
  }

  style(styleType: Style): Styled {
    return new Styled(this, styleType);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }

  render(): string {
    const labelRendered = this.label.render();
    const contentRendered = this.content.render();

    const labelLines = labelRendered.split("\n");
    const contentLines = contentRendered.split("\n");
    const allLines = [...labelLines, ...contentLines];

    const maxTextLength =
      allLines.length === 0
        ? 0
        : Math.max(...allLines.map((line) => stripAnsiCodes(line).length));
    const contentWidth = maxTextLength + DIMENSIONS.MIN_CONTENT_PADDING;

    const chars = getBorderChars(this.borderStyle);

    const topBorder =
      chars.topLeft +
      chars.horizontal.repeat(contentWidth + 2) +
      chars.topRight;
    const bottomBorder =
      chars.bottomLeft +
      chars.horizontal.repeat(contentWidth + 2) +
      chars.bottomRight;

    const createCardLines = (lines: string[]) =>
      lines.map((line) => {
        const visibleLength = stripAnsiCodes(line).length;
        const padding = contentWidth - visibleLength;
        return `${chars.vertical} ${line}${" ".repeat(padding)} ${chars.vertical}`;
      });

    const labelCardLines = createCardLines(labelLines);
    const contentCardLines = createCardLines(contentLines);

    return [
      topBorder,
      ...labelCardLines,
      ...contentCardLines,
      bottomBorder,
    ].join("\n");
  }
}

/**
 * Table with headers and borders
 */
export class Table implements Element {
  constructor(
    private headers: Element[],
    private rows: Element[][],
    private borderStyle: BorderStyle = BorderStyle.Single
  ) {}

  /**
   * Set the border style (fluent API)
   */
  border(style: BorderType): Table {
    return new Table(this.headers, this.rows, style as BorderStyle);
  }

  render(): string {
    const headerLines = this.headers.map((h) => h.render().split("\n"));
    const rowLines = this.rows.map((row) =>
      row.map((cell) => cell.render().split("\n"))
    );
    const allRowLines = [headerLines, ...rowLines];

    const columnWidths = this.calculateColumnWidths(allRowLines);
    const chars = getBorderChars(this.borderStyle);

    const borders = this.createTableBorders(columnWidths, chars);

    const headerRowHeight = Math.max(
      ...headerLines.map((lines) => lines.length)
    );
    const headerRows = this.buildMultilineTableRows(
      headerLines,
      columnWidths,
      headerRowHeight,
      chars
    );

    const dataRows = rowLines.flatMap((row) => {
      const rowHeight = Math.max(...row.map((cellLines) => cellLines.length));
      return this.buildMultilineTableRows(row, columnWidths, rowHeight, chars);
    });

    return [
      borders.top,
      ...headerRows,
      borders.separator,
      ...dataRows,
      borders.bottom,
    ].join("\n");
  }

  private calculateColumnWidths(allRowLines: string[][][]): number[] {
    return this.headers.map((_, columnIndex) => {
      let maxWidth = 0;
      for (const row of allRowLines) {
        if (columnIndex < row.length) {
          for (const line of row[columnIndex]) {
            const width = stripAnsiCodes(line).length;
            if (width > maxWidth) maxWidth = width;
          }
        }
      }
      return maxWidth;
    });
  }

  private createTableBorders(widths: number[], chars: BorderChars) {
    const segments = widths.map((w) => chars.horizontal.repeat(w));

    const junctionChars = this.getJunctionChars(chars);

    return {
      top: segments
        .join(`${chars.horizontal}${junctionChars.teeDown}${chars.horizontal}`)
        .replace(/^/, `${chars.topLeft}${chars.horizontal}`)
        .replace(/$/, `${chars.horizontal}${chars.topRight}`),
      separator: segments
        .join(`${chars.horizontal}${junctionChars.cross}${chars.horizontal}`)
        .replace(/^/, `${junctionChars.teeRight}${chars.horizontal}`)
        .replace(/$/, `${chars.horizontal}${junctionChars.teeLeft}`),
      bottom: segments
        .join(`${chars.horizontal}${junctionChars.teeUp}${chars.horizontal}`)
        .replace(/^/, `${chars.bottomLeft}${chars.horizontal}`)
        .replace(/$/, `${chars.horizontal}${chars.bottomRight}`),
    };
  }

  private getJunctionChars(chars: BorderChars) {
    return {
      teeDown: "┬",
      teeUp: "┴",
      teeLeft: "┤",
      teeRight: "├",
      cross: "┼",
    };
  }

  private buildMultilineTableRows(
    cellLines: string[][],
    widths: number[],
    rowHeight: number,
    chars: BorderChars
  ): string[] {
    const result: string[] = [];

    for (let lineIndex = 0; lineIndex < rowHeight; lineIndex++) {
      const rowParts = cellLines.map((lines, colIndex) => {
        const line = lineIndex < lines.length ? lines[lineIndex] : "";
        const visibleLength = stripAnsiCodes(line).length;
        const padding = widths[colIndex] - visibleLength;
        return line + " ".repeat(Math.max(0, padding));
      });

      result.push(
        `${chars.vertical} ${rowParts.join(` ${chars.vertical} `)} ${chars.vertical}`
      );
    }

    return result;
  }
}

// DSL Constructor Functions

/**
 * Create a text element
 */
export function text(content: string): Text {
  return new Text(content);
}

/**
 * Apply color to an element
 */
export function color(colorType: Color) {
  return (element: string | Element): Colored =>
    new Colored(toElement(element), colorType);
}

/**
 * Apply style to an element
 */
export function style(styleType: Style) {
  return (element: string | Element): Styled =>
    new Styled(toElement(element), styleType);
}

/**
 * Create a vertical layout
 */
export function layout(...elements: (string | Element)[]): Layout {
  return new Layout(toElements(elements));
}

/**
 * Create a section with title
 */
export function section(title: string, glyph = "=", flankingChars = 3) {
  return (content: string | Element) =>
    new Section(title, toElement(content), glyph, flankingChars);
}

/**
 * Create key-value pairs
 */
export function kv(...pairs: Array<[string, string]>): KeyValue {
  return new KeyValue(pairs);
}

/**
 * Create an unordered list
 */
export function ul(...items: (string | Element)[]): UnorderedList {
  const elements = toElements(items);
  return new UnorderedList(elements);
}

/**
 * Create an ordered list
 */
export function ol(...items: (string | Element)[]): OrderedList {
  const elements = toElements(items);
  return new OrderedList(elements);
}

/**
 * Create a box container (non-curried API)
 */
export function box(title = "", style = BorderStyle.Single) {
  return (...elements: (string | Element)[]) =>
    new Box(toElements(elements), title, style);
}

/**
 * Create a horizontal row
 */
export function row(...elements: (string | Element)[]): Row {
  return new Row(toElements(elements));
}

/**
 * Create a horizontal rule
 */
export function hr(char = "─", width?: number): HorizontalRule {
  return new HorizontalRule(char, width);
}

/**
 * Create an inline bar
 */
export function inlineBar(
  label: string | Element,
  progress: number
): InlineBar {
  return new InlineBar(toElement(label), progress);
}

/**
 * Create a status card (non-curried API)
 */
export function statusCard(
  label: string | Element,
  content: string | Element,
  style?: BorderStyle
): StatusCard {
  const labelElement = toElement(label);
  const contentElement = toElement(content);
  return new StatusCard(
    labelElement,
    contentElement,
    style ?? BorderStyle.Single
  );
}

/**
 * Helper function to convert strings or elements to Elements
 */
function toElement(item: string | Element): Element {
  return typeof item === "string" ? new Text(item) : item;
}

/**
 * Helper function to convert array of strings or elements to Elements
 */
function toElements(items: (string | Element)[]): Element[] {
  return items.map(toElement);
}

/**
 * Create a table (non-curried API)
 */
export function table(
  headers: (string | Element)[],
  rows: (string | Element)[][],
  style?: BorderStyle
): Table {
  const headerElements = toElements(headers);
  const rowElements = rows.map((row) => toElements(row));
  return new Table(headerElements, rowElements, style ?? BorderStyle.Single);
}

/**
 * Line break
 */
export function br(count = 1): LineBreak | Layout {
  if (count === 1) {
    return new LineBreak();
  }
  return new Layout(Array(count).fill(new LineBreak()));
}

/**
 * Center element within specified width
 */
export class Center implements Element {
  constructor(
    private element: Element,
    private width?: number
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");
    const defaultWidth =
      typeof process !== "undefined" && process.stdout?.columns
        ? process.stdout.columns
        : 80;
    const targetWidth = this.width ?? defaultWidth;

    // Find the longest line to center the whole block as a unit
    const maxLineLength =
      lines.length === 0
        ? 0
        : Math.max(...lines.map((line) => stripAnsiCodes(line).length));

    if (maxLineLength >= targetWidth) {
      // Content already wider than target - don't modify
      return content;
    }

    // Center the whole block - all lines get the same left padding
    const totalPadding = targetWidth - maxLineLength;
    const leftPadding = Math.floor((totalPadding + 1) / 2);

    return lines
      .map((line) => {
        const lineLength = stripAnsiCodes(line).length;
        const rightPadding = targetWidth - lineLength - leftPadding;
        return (
          " ".repeat(leftPadding) + line + " ".repeat(Math.max(0, rightPadding))
        );
      })
      .join("\n");
  }
}

/**
 * Center an element
 */
export function center(
  element: string | Element,
  width?: number
): Center | AutoCentered {
  if (width === undefined) {
    return new AutoCentered(toElement(element));
  }
  return new Center(toElement(element), width);
}

/**
 * Add underline to an element with custom character
 */
export function underline(char = "─") {
  return (element: string | Element) => new Underline(toElement(element), char);
}

/**
 * Add colored underline to an element
 */
export function underlineColored(char: string, colorType: Color) {
  return (element: string | Element) =>
    new Underline(toElement(element), char, colorType);
}

/**
 * Add a prefix margin to elements
 */
export function margin(prefix: string) {
  return (...elements: (string | Element)[]) =>
    new Margin(prefix, toElements(elements));
}

/**
 * Add a colored prefix margin to elements
 */
export function marginColored(prefix: string, colorType: Color) {
  return (...elements: (string | Element)[]) => {
    const coloredPrefix = wrapAnsi(prefix, getColorCode(colorType));
    return new Margin(coloredPrefix, toElements(elements));
  };
}

/**
 * Create a tree structure - can be used as both a Tree and a function
 */
export function tree(label: string): any {
  const baseTree = new Tree(label, []);

  const result = function (...children: Tree[]) {
    return new Tree(label, children);
  } as any;

  // Copy all Tree methods to the function
  result.render = () => baseTree.render();
  result.color = (colorType: Color) => baseTree.color(colorType);
  result.style = (styleType: Style) => baseTree.style(styleType);
  result.margin = (prefix: string) => baseTree.margin(prefix);
  result.label = label;
  result.children = [];

  return result;
}

/**
 * Create a horizontal bar chart
 */
export function chart(...data: Array<[string | Element, number]>): Chart {
  const chartData = data.map(
    ([label, value]) => [toElement(label), value] as [Element, number]
  );
  return new Chart(chartData);
}

/**
 * Create a decorative banner
 */
export function banner(content: string | Element = ""): Banner {
  return new Banner(toElement(content), BorderStyle.Double);
}

/**
 * Arrange elements in columns with spacing
 */
export function columns(...elements: (string | Element)[]): Columns;
export function columns(
  spacing: number,
  ...elements: (string | Element)[]
): Columns;
export function columns(
  spacingOrFirstElement: number | string | Element,
  ...elements: (string | Element)[]
): Columns {
  if (typeof spacingOrFirstElement === "number") {
    return new Columns(toElements(elements), spacingOrFirstElement);
  } else {
    const allElements = [spacingOrFirstElement, ...elements];
    return new Columns(toElements(allElements), 2);
  }
}

/**
 * Add padding around an element
 */
export function pad(padding: number) {
  return (element: string | Element) => new Padded(toElement(element), padding);
}

/**
 * Truncate text with ellipsis if it exceeds max width
 */
export function truncate(maxWidth: number, ellipsis = "...") {
  return (element: string | Element) =>
    new Truncated(toElement(element), maxWidth, ellipsis);
}

/**
 * Create vertical separator
 */
export function vr(lineCount: number, char = "│"): VerticalRule {
  return new VerticalRule(char, lineCount);
}

/**
 * Wrap text at word boundaries within specified width
 */
export function wrap(maxWidth: number) {
  return (element: string | Element) =>
    new Wrapped(toElement(element), maxWidth);
}

/**
 * Justify text to exact width by distributing spaces
 */
export function justify(targetWidth: number, justifyLastLine = false) {
  return (element: string | Element) =>
    new Justified(toElement(element), targetWidth, justifyLastLine);
}

/**
 * Left-align element within specified width
 */
export function leftAlign(targetWidth: number) {
  return (element: string | Element) =>
    new LeftAligned(toElement(element), targetWidth);
}

/**
 * Right-align element within specified width
 */
export function rightAlign(targetWidth: number) {
  return (element: string | Element) =>
    new RightAligned(toElement(element), targetWidth);
}

/**
 * Auto-center element within layout context
 */
export function autoCenter(element: string | Element): AutoCentered {
  return new AutoCentered(toElement(element));
}

/**
 * Empty element for conditional rendering
 */
export function empty(): Empty {
  return new Empty();
}

/**
 * Predefined status margins with color coding
 */
export const margins = {
  error: (...elements: (string | Element)[]) =>
    new Margin("[\u001b[31merror\u001b[0m]", toElements(elements)),
  warn: (...elements: (string | Element)[]) =>
    new Margin("[\u001b[33mwarn\u001b[0m]", toElements(elements)),
  success: (...elements: (string | Element)[]) =>
    new Margin("[\u001b[32msuccess\u001b[0m]", toElements(elements)),
  info: (...elements: (string | Element)[]) =>
    new Margin("[\u001b[36minfo\u001b[0m]", toElements(elements)),
};

/**
 * Underline element that adds underline below content
 */
export class Underline implements Element {
  constructor(
    private element: Element,
    private char: string = "─",
    private underlineColor?: Color
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");
    const maxWidth = Math.max(...lines.map((line) => realLength(line)));

    // Create underline by repeating the pattern to match the width
    let underlineStr = "";
    while (underlineStr.length < maxWidth) {
      underlineStr += this.char;
    }
    underlineStr = underlineStr.substring(0, maxWidth);

    // Apply color only to the underline, not the content
    if (this.underlineColor) {
      const code = getColorCode(this.underlineColor);
      if (code) {
        underlineStr = wrapAnsi(underlineStr, code);
      }
    }

    return content + "\n" + underlineStr;
  }

  color(colorType: Color): Colored {
    return new Colored(this, colorType);
  }

  style(styleType: Style): Styled {
    return new Styled(this, styleType);
  }

  margin(prefix: string): Margin {
    return new Margin(prefix, [this]);
  }
}

/**
 * Margin element that adds prefix to each line
 */
export class Margin implements Element {
  constructor(
    private prefix: string,
    private elements: Element[]
  ) {}

  render(): string {
    const content =
      this.elements.length === 1 ? this.elements[0] : new Layout(this.elements);

    const lines = content.render().split("\n");
    return lines.map((line) => `${this.prefix} ${line}`).join("\n");
  }
}

/**
 * Chart - horizontal bar chart component
 */
export class Chart implements Element {
  constructor(private data: Array<[Element, number]>) {}

  render(): string {
    if (this.data.length === 0) return "";

    // Find max value for scaling
    const maxValue = Math.max(...this.data.map(([, value]) => Math.abs(value)));
    if (maxValue === 0)
      return this.data.map(([label]) => label.render()).join("\n");

    // Calculate label width for alignment
    const maxLabelWidth = Math.max(
      ...this.data.map(([label]) =>
        Math.max(
          ...label
            .render()
            .split("\n")
            .map((line) => stripAnsiCodes(line).length)
        )
      )
    );
    const labelWidth = Math.min(
      maxLabelWidth,
      DIMENSIONS.CHART_LABEL_MAX_WIDTH
    );

    return this.data
      .map(([label, value]) => {
        const labelText = flattenToSingleLine(label);
        const truncatedLabel =
          stripAnsiCodes(labelText).length > labelWidth
            ? stripAnsiCodes(labelText).substring(0, labelWidth - 3) + "..."
            : labelText;
        const paddedLabel = truncatedLabel.padEnd(labelWidth, " ");

        // Calculate bar length (percentage of chart width)
        const percentage = Math.abs(value) / maxValue;
        const barLength = Math.floor(
          percentage * DIMENSIONS.DEFAULT_CHART_WIDTH
        );
        const bar = GLYPHS.BAR_FILLED.repeat(barLength);
        const emptyBar = GLYPHS.BAR_EMPTY.repeat(
          DIMENSIONS.DEFAULT_CHART_WIDTH - barLength
        );

        // Format value display
        const valueStr =
          typeof value === "number" && value % 1 === 0
            ? value.toString()
            : value.toFixed(1);

        return `${paddedLabel} │${bar}${emptyBar}│ ${valueStr}`;
      })
      .join("\n");
  }
}

/**
 * Banner - decorative text in a box with fluent API
 */
export class Banner implements Element {
  constructor(
    private content: Element,
    private borderStyle: BorderStyle = BorderStyle.Double
  ) {}

  /**
   * Set the border style (fluent API)
   */
  border(style: BorderType): Banner {
    return new Banner(this.content, style as BorderStyle);
  }

  render(): string {
    const rendered = this.content.render();
    const lines = rendered.split("\n");
    const maxWidth =
      lines.length === 0
        ? 0
        : Math.max(...lines.map((line) => stripAnsiCodes(line).length));
    const contentWidth = maxWidth + DIMENSIONS.MIN_CONTENT_PADDING;

    const chars = getBorderChars(this.borderStyle);
    const topBorder = `${chars.topLeft}${chars.horizontal.repeat(contentWidth + 2)}${chars.topRight}`;
    const bottomBorder = `${chars.bottomLeft}${chars.horizontal.repeat(contentWidth + 2)}${chars.bottomRight}`;

    const contentLines = lines.map((line) => {
      const padding = contentWidth - stripAnsiCodes(line).length;
      return `${chars.vertical} ${line}${" ".repeat(padding)} ${chars.vertical}`;
    });

    return [topBorder, ...contentLines, bottomBorder].join("\n");
  }
}

/**
 * Columns - arrange elements in columns with spacing
 */
export class Columns implements Element {
  constructor(
    private elements: Element[],
    private spacing: number = 2
  ) {}

  render(): string {
    if (this.elements.length === 0) return "";

    const renderedElements = this.elements.map((el) => el.render().split("\n"));
    const maxHeight = Math.max(
      ...renderedElements.map((lines) => lines.length)
    );
    const elementWidths = this.elements.map((el) => getWidth(el));

    const paddedElements = renderedElements.map((lines, i) => {
      const width = elementWidths[i];
      const paddedLines = [...lines];

      // Pad with empty lines to match max height
      while (paddedLines.length < maxHeight) {
        paddedLines.push("");
      }

      // Pad each line to element width
      return paddedLines.map((line) => line.padEnd(width, " "));
    });

    const spacer = " ".repeat(this.spacing);
    const result: string[] = [];
    for (let row = 0; row < maxHeight; row++) {
      const rowContent = paddedElements.map((lines) => lines[row]).join(spacer);
      result.push(rowContent.trimEnd());
    }

    return result.join("\n");
  }
}

/**
 * Padded - add padding around an element
 */
export class Padded implements Element {
  constructor(
    private element: Element,
    private padding: number
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");
    const maxWidth =
      lines.length === 0
        ? 0
        : Math.max(...lines.map((line) => stripAnsiCodes(line).length));

    const horizontalPad = " ".repeat(this.padding);
    const verticalPad = " ".repeat(maxWidth + this.padding * 2);

    const paddedLines = lines.map((line) => {
      const linePadding = maxWidth - stripAnsiCodes(line).length;
      return `${horizontalPad}${line}${" ".repeat(linePadding)}${horizontalPad}`;
    });

    const verticalLines = Array(this.padding).fill(verticalPad);

    return [...verticalLines, ...paddedLines, ...verticalLines].join("\n");
  }
}

/**
 * Truncated - truncate text with ellipsis if it exceeds max width
 */
export class Truncated implements Element {
  constructor(
    private element: Element,
    private maxWidth: number,
    private ellipsis: string = "..."
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");

    return lines
      .map((line) => {
        const visibleLength = stripAnsiCodes(line).length;
        if (visibleLength <= this.maxWidth) {
          return line;
        }

        const truncateLength = this.maxWidth - this.ellipsis.length;
        if (truncateLength <= 0) {
          return this.ellipsis.substring(0, this.maxWidth);
        }

        // Simple truncation - in a full implementation you'd want to handle ANSI codes properly
        const truncated = stripAnsiCodes(line).substring(0, truncateLength);
        return truncated + this.ellipsis;
      })
      .join("\n");
  }
}

/**
 * VerticalRule - vertical separator line
 */
export class VerticalRule implements Element {
  constructor(
    private char: string = "│",
    private lineCount: number
  ) {}

  render(): string {
    const count = Math.max(1, this.lineCount);
    return Array(count).fill(this.char).join("\n");
  }
}

/**
 * Wrapped - text wrapping at word boundaries
 */
export class Wrapped implements Element {
  constructor(
    private element: Element,
    private maxWidth: number
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");

    return lines
      .flatMap((line) => {
        const visibleLength = stripAnsiCodes(line).length;
        if (visibleLength <= this.maxWidth) {
          return [line];
        }

        // Simple word wrapping - split on spaces and wrap
        const plainLine = stripAnsiCodes(line);
        const words = plainLine.split(" ");
        const wrappedLines: string[] = [];
        let currentLine = "";

        for (const word of words) {
          const testLine = currentLine ? `${currentLine} ${word}` : word;
          if (testLine.length <= this.maxWidth) {
            currentLine = testLine;
          } else {
            if (currentLine) {
              wrappedLines.push(currentLine);
              currentLine = word;
            } else {
              // Word itself is too long, break it
              wrappedLines.push(word.substring(0, this.maxWidth));
              currentLine = word.substring(this.maxWidth);
            }
          }
        }

        if (currentLine) {
          wrappedLines.push(currentLine);
        }

        return wrappedLines.length > 0 ? wrappedLines : [""];
      })
      .join("\n");
  }
}

/**
 * Justified - justify text to exact width by distributing spaces
 */
export class Justified implements Element {
  constructor(
    private element: Element,
    private targetWidth: number,
    private justifyLastLine: boolean = false
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");

    return lines
      .map((line, index) => {
        const plainLine = stripAnsiCodes(line).trim();
        const visibleLength = plainLine.length;

        if (visibleLength >= this.targetWidth) {
          return plainLine.substring(0, this.targetWidth);
        }

        const isLastLine = index === lines.length - 1;
        if (isLastLine && !this.justifyLastLine) {
          return line; // Don't justify last line unless explicitly requested
        }

        const words = plainLine.split(" ").filter((word) => word.length > 0);
        if (words.length <= 1) {
          return plainLine.padEnd(this.targetWidth, " ");
        }

        const totalWordLength = words.join("").length;
        const totalSpaceNeeded = this.targetWidth - totalWordLength;
        const gaps = words.length - 1;

        if (gaps === 0) {
          return plainLine.padEnd(this.targetWidth, " ");
        }

        const spacePerGap = Math.floor(totalSpaceNeeded / gaps);
        const extraSpaces = totalSpaceNeeded % gaps;

        let result = "";
        for (let i = 0; i < words.length; i++) {
          result += words[i];
          if (i < words.length - 1) {
            const spaces = spacePerGap + (i < extraSpaces ? 1 : 0);
            result += " ".repeat(spaces);
          }
        }

        return result;
      })
      .join("\n");
  }
}

/**
 * LeftAligned - left align element within specified width
 */
export class LeftAligned implements Element {
  constructor(
    private element: Element,
    private targetWidth: number
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");

    return lines
      .map((line) => {
        const visibleLength = stripAnsiCodes(line).length;
        if (visibleLength >= this.targetWidth) {
          return stripAnsiCodes(line).substring(0, this.targetWidth);
        }

        return line + " ".repeat(this.targetWidth - visibleLength);
      })
      .join("\n");
  }
}

/**
 * RightAligned - right align element within specified width
 */
export class RightAligned implements Element {
  constructor(
    private element: Element,
    private targetWidth: number
  ) {}

  render(): string {
    const content = this.element.render();
    const lines = content.split("\n");

    return lines
      .map((line) => {
        const visibleLength = stripAnsiCodes(line).length;
        if (visibleLength >= this.targetWidth) {
          return stripAnsiCodes(line).substring(0, this.targetWidth);
        }

        const padding = this.targetWidth - visibleLength;
        return " ".repeat(padding) + line;
      })
      .join("\n");
  }
}

/**
 * AutoCentered - auto center based on layout context
 */
export class AutoCentered implements Element {
  constructor(public element: Element) {}

  render(): string {
    return this.element.render();
  }
}

/**
 * Empty - empty element for conditional rendering
 */
export class Empty implements Element {
  render(): string {
    return "";
  }
}

// Default export with all main functions for convenience
const layoutz = {
  // Core elements
  layout,
  text,

  // Colors and Styles
  color,
  style,
  Color,
  Style,
  underlineColored,
  marginColored,

  // Containers
  box,
  section,
  banner,

  // Layout
  row,
  center,
  autoCenter,
  margin,
  columns,

  // Lists and data
  ul,
  ol,
  kv,
  table,
  tree,
  chart,

  // Widgets
  statusCard,
  inlineBar,
  hr,
  vr,
  underline,
  br,

  // Text formatting
  pad,
  truncate,
  wrap,
  justify,
  leftAlign,
  rightAlign,

  // Utilities
  empty,

  // Constants
  Border,
  BorderStyle,
  margins,

  // Utilities
  getWidth,
  getHeight,
  DIMENSIONS,
  GLYPHS,
  stripAnsiCodes,
};

export default layoutz;

// Named export for convenience - contains all main DSL functions
export const dsl = {
  layout,
  text,
  box,
  section,
  banner,
  row,
  center,
  autoCenter,
  margin,
  columns,
  ul,
  ol,
  kv,
  table,
  tree,
  chart,
  statusCard,
  inlineBar,
  hr,
  vr,
  underline,
  br,
  pad,
  truncate,
  wrap,
  justify,
  leftAlign,
  rightAlign,
  empty,
  Border,
  BorderStyle,
  margins,
};

// Export additional utilities for convenience
export { DIMENSIONS, GLYPHS, stripAnsiCodes };
