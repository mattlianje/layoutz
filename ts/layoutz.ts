/**
 * layoutz - Friendly, expressive print-layout DSL for JavaScript/TypeScript
 * Port of the Scala layoutz library focused on string rendering
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
 * Flatten multiline elements to single line for components that need single-line content
 */
function flattenToSingleLine(element: Element): string {
  return element.render().split('\n').join(" ");
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
  
  const lines = rendered.split('\n');
  return Math.max(...lines.map(line => stripAnsiCodes(line).length));
}

/**
 * Get the height of an element (number of lines)
 */
export function getHeight(element: Element): number {
  const rendered = element.render();
  if (!rendered) return 1;
  return rendered.split('\n').length;
}

/**
 * Simple text element
 */
export class Text implements Element {
  constructor(private content: string) {}
  
  render(): string {
    return this.content;
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
  Round = "round"
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
      return { topLeft: "┌", topRight: "┐", bottomLeft: "└", bottomRight: "┘", horizontal: "─", vertical: "│" };
    case BorderStyle.Double:
      return { topLeft: "╔", topRight: "╗", bottomLeft: "╚", bottomRight: "╝", horizontal: "═", vertical: "║" };
    case BorderStyle.Thick:
      return { topLeft: "┏", topRight: "┓", bottomLeft: "┗", bottomRight: "┛", horizontal: "━", vertical: "┃" };
    case BorderStyle.Round:
      return { topLeft: "╭", topRight: "╮", bottomLeft: "╰", bottomRight: "╯", horizontal: "─", vertical: "│" };
  }
}

/**
 * Key-value pairs with aligned formatting
 */
export class KeyValue implements Element {
  constructor(private pairs: Array<[string, string]>) {}
  
  render(): string {
    if (this.pairs.length === 0) return "";
    
    const maxKeyLength = Math.max(...this.pairs.map(([key]) => key.length));
    const alignmentPosition = maxKeyLength + 2;
    
    return this.pairs.map(([key, value]) => {
      const keyWithColon = `${key}:`;
      const spacesNeeded = alignmentPosition - keyWithColon.length;
      const padding = " ".repeat(Math.max(1, spacesNeeded));
      return `${keyWithColon}${padding}${value}`;
    }).join("\n");
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
    
    const currentBullet = this.bullet === "•" 
      ? UnorderedList.BULLET_STYLES[level % UnorderedList.BULLET_STYLES.length]
      : this.bullet;
    
    return this.items.map(item => {
      if (item instanceof UnorderedList) {
        return item.renderAtLevel(level + 1);
      } else {
        const content = item.render();
        const lines = content.split('\n');
        const indent = "  ".repeat(level);
        
        if (lines.length === 1) {
          return `${indent}${currentBullet} ${lines[0]}`;
        } else {
          const firstLine = `${indent}${currentBullet} ${lines[0]}`;
          const lineIndent = indent + " ".repeat(currentBullet.length + 1);
          const remainingLines = lines.slice(1).map(line => `${lineIndent}${line}`);
          return [firstLine, ...remainingLines].join("\n");
        }
      }
    }).join("\n");
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
  
  render(): string {
    return this.renderAtLevel(0, true, []);
  }
  
  private renderAtLevel(level: number, isLast: boolean, parentPrefixes: boolean[]): string {
    if (level === 0 && this.children.length === 0) {
      // Leaf node at root level
      return this.label;
    }
    
    let result = "";
    
    // Add the current node
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
        result += childTree.renderAtLevel(level + 1, isChildLast, newParentPrefixes);
        
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
    
    return this.items.map(item => {
      if (item instanceof OrderedList) {
        return item.renderAtLevel(level + 1);
      } else {
        const number = this.getNumbering(itemNumber, level);
        itemNumber++;
        const content = item.render();
        const lines = content.split('\n');
        const indent = "  ".repeat(level);
        
        if (lines.length === 1) {
          return `${indent}${number}. ${lines[0]}`;
        } else {
          const firstLine = `${indent}${number}. ${lines[0]}`;
          const lineIndent = indent + " ".repeat(number.length + 2);
          const remainingLines = lines.slice(1).map(line => `${lineIndent}${line}`);
          return [firstLine, ...remainingLines].join("\n");
        }
      }
    }).join("\n");
  }
  
  private getNumbering(index: number, level: number): string {
    switch (level % 3) {
      case 0: return (index + 1).toString();
      case 1: return String.fromCharCode(97 + index); // a, b, c...
      case 2: return this.toRomanNumeral(index + 1);
      default: return (index + 1).toString();
    }
  }
  
  private toRomanNumeral(n: number): string {
    const mappings: Array<[number, string]> = [
      [10, "x"], [9, "ix"], [5, "v"], [4, "iv"], [1, "i"]
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
    private style: BorderStyle = BorderStyle.Single
  ) {}
  
  render(): string {
    const content = this.elements.length === 1 
      ? this.elements[0] 
      : new Layout(this.elements);
    
    const contentLines = content.render().split('\n');
    const contentWidth = contentLines.length === 0 
      ? 0 
      : Math.max(...contentLines.map(line => stripAnsiCodes(line).length));
    
    const titleWidth = this.title ? this.title.length + DIMENSIONS.MIN_CONTENT_PADDING : 0;
    const innerWidth = Math.max(contentWidth, titleWidth);
    const totalWidth = innerWidth + DIMENSIONS.BOX_INNER_PADDING;
    
    const chars = getBorderChars(this.style);
    
    const topBorder = this.title 
      ? (() => {
          const titlePadding = totalWidth - this.title.length - DIMENSIONS.BOX_BORDER_WIDTH;
          const leftPad = Math.floor(titlePadding / 2);
          const rightPad = titlePadding - leftPad;
          return `${chars.topLeft}${chars.horizontal.repeat(leftPad)}${this.title}${chars.horizontal.repeat(rightPad)}${chars.topRight}`;
        })()
      : `${chars.topLeft}${chars.horizontal.repeat(totalWidth - DIMENSIONS.BOX_BORDER_WIDTH)}${chars.topRight}`;
    
    const bottomBorder = `${chars.bottomLeft}${chars.horizontal.repeat(totalWidth - DIMENSIONS.BOX_BORDER_WIDTH)}${chars.bottomRight}`;
    
    const paddedContent = contentLines.map(line => {
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
    
    const renderedElements = this.elements.map(el => el.render().split('\n'));
    const maxHeight = Math.max(...renderedElements.map(lines => lines.length));
    const elementWidths = this.elements.map(el => getWidth(el));
    
    const paddedElements = renderedElements.map((lines, i) => {
      const width = elementWidths[i];
      const paddedLines = [...lines];
      
      // Pad with empty lines to match max height
      while (paddedLines.length < maxHeight) {
        paddedLines.push("");
      }
      
      // Pad each line to element width
      return paddedLines.map(line => line.padEnd(width, " "));
    });
    
    const result: string[] = [];
    for (let row = 0; row < maxHeight; row++) {
      const rowContent = paddedElements.map(lines => lines[row]).join(" ");
      result.push(rowContent.trimEnd());
    }
    
    return result.join("\n");
  }
}

/**
 * Vertical layout container
 */
export class Layout implements Element {
  constructor(private elements: Element[]) {}
  
  render(): string {
    return this.elements.map(el => el.render()).join("\n");
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
    const filledSegments = Math.floor(clampedProgress * DIMENSIONS.PROGRESS_BAR_WIDTH);
    const emptySegments = DIMENSIONS.PROGRESS_BAR_WIDTH - filledSegments;
    
    const bar = GLYPHS.BAR_FILLED.repeat(filledSegments) + GLYPHS.BAR_EMPTY.repeat(emptySegments);
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
    private style: BorderStyle = BorderStyle.Single
  ) {}
  
  render(): string {
    const labelRendered = this.label.render();
    const contentRendered = this.content.render();
    
    const labelLines = labelRendered.split('\n');
    const contentLines = contentRendered.split('\n');
    const allLines = [...labelLines, ...contentLines];
    
    const maxTextLength = allLines.length === 0 
      ? 0 
      : Math.max(...allLines.map(line => stripAnsiCodes(line).length));
    const contentWidth = maxTextLength + DIMENSIONS.MIN_CONTENT_PADDING;
    
    const chars = getBorderChars(this.style);
    
    const topBorder = chars.topLeft + chars.horizontal.repeat(contentWidth + 2) + chars.topRight;
    const bottomBorder = chars.bottomLeft + chars.horizontal.repeat(contentWidth + 2) + chars.bottomRight;
    
    const createCardLines = (lines: string[]) => lines.map(line => {
      const visibleLength = stripAnsiCodes(line).length;
      const padding = contentWidth - visibleLength;
      return `${chars.vertical} ${line}${" ".repeat(padding)} ${chars.vertical}`;
    });
    
    const labelCardLines = createCardLines(labelLines);
    const contentCardLines = createCardLines(contentLines);
    
    return [topBorder, ...labelCardLines, ...contentCardLines, bottomBorder].join("\n");
  }
}

/**
 * Table with headers and borders
 */
export class Table implements Element {
  constructor(
    private headers: Element[],
    private rows: Element[][],
    private style: BorderStyle = BorderStyle.Single
  ) {}
  
  render(): string {
    const headerLines = this.headers.map(h => h.render().split('\n'));
    const rowLines = this.rows.map(row => row.map(cell => cell.render().split('\n')));
    const allRowLines = [headerLines, ...rowLines];
    
    const columnWidths = this.calculateColumnWidths(allRowLines);
    const chars = getBorderChars(this.style);
    
    const borders = this.createTableBorders(columnWidths, chars);
    
    const headerRowHeight = Math.max(...headerLines.map(lines => lines.length));
    const headerRows = this.buildMultilineTableRows(headerLines, columnWidths, headerRowHeight, chars);
    
    const dataRows = rowLines.flatMap(row => {
      const rowHeight = Math.max(...row.map(cellLines => cellLines.length));
      return this.buildMultilineTableRows(row, columnWidths, rowHeight, chars);
    });
    
    return [borders.top, ...headerRows, borders.separator, ...dataRows, borders.bottom].join("\n");
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
    const segments = widths.map(w => chars.horizontal.repeat(w));
    
    const junctionChars = this.getJunctionChars(chars);
    
    return {
      top: segments.join(`${chars.horizontal}${junctionChars.teeDown}${chars.horizontal}`).replace(/^/, `${chars.topLeft}${chars.horizontal}`).replace(/$/, `${chars.horizontal}${chars.topRight}`),
      separator: segments.join(`${chars.horizontal}${junctionChars.cross}${chars.horizontal}`).replace(/^/, `${junctionChars.teeRight}${chars.horizontal}`).replace(/$/, `${chars.horizontal}${junctionChars.teeLeft}`),
      bottom: segments.join(`${chars.horizontal}${junctionChars.teeUp}${chars.horizontal}`).replace(/^/, `${chars.bottomLeft}${chars.horizontal}`).replace(/$/, `${chars.horizontal}${chars.bottomRight}`)
    };
  }
  
  private getJunctionChars(chars: BorderChars) {
    // Simplified junction logic - in a full implementation you'd want proper junction characters per style
    return {
      teeDown: "┬",
      teeUp: "┴", 
      teeLeft: "┤",
      teeRight: "├",
      cross: "┼"
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
      
      result.push(`${chars.vertical} ${rowParts.join(` ${chars.vertical} `)} ${chars.vertical}`);
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
 * Create a vertical layout
 */
export function layout(...elements: Element[]): Layout {
  return new Layout(elements);
}

/**
 * Create a section with title
 */
export function section(title: string, glyph = "=", flankingChars = 3) {
  return (content: Element) => new Section(title, content, glyph, flankingChars);
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
export function ul(): (...items: Element[]) => UnorderedList;
export function ul(...items: Element[]): UnorderedList;
export function ul(bullet: string): (...items: Element[]) => UnorderedList;
export function ul(bulletOrFirstItem?: string | Element, ...restItems: Element[]): UnorderedList | ((...items: Element[]) => UnorderedList) {
  if (arguments.length === 0) {
    // Called with no arguments - return a function
    return (...items: Element[]) => new UnorderedList(items);
  } else if (typeof bulletOrFirstItem === 'string' && restItems.length === 0) {
    // Called with bullet parameter
    return (...items: Element[]) => new UnorderedList(items, bulletOrFirstItem);
  } else {
    // Called with items
    const items = bulletOrFirstItem ? [bulletOrFirstItem as Element, ...restItems] : restItems;
    return new UnorderedList(items);
  }
}

/**
 * Create an ordered list
 */
export function ol(...items: Element[]): OrderedList {
  return new OrderedList(items);
}

/**
 * Create a box container
 */
export function box(title = "", style = BorderStyle.Single) {
  return (...elements: Element[]) => new Box(elements, title, style);
}

/**
 * Create a box container with style first (Scala-style API)
 */
export function boxWithStyle(style: BorderStyle): (title: string) => (...elements: Element[]) => Box {
  return (title: string = "") => (...elements: Element[]) => new Box(elements, title, style);
}

/**
 * Create a horizontal row
 */
export function row(...elements: Element[]): Row {
  return new Row(elements);
}

/**
 * Create a horizontal rule
 */
export function hr(char = "─", width?: number): HorizontalRule {
  return new HorizontalRule(char, width);
}

/**
 * Create a progress bar
 */
export function progressBar(label: Element, progress: number): InlineBar {
  return new InlineBar(label, progress);
}

/**
 * Create an inline bar (alias for progressBar to match Scala API)
 */
export function inlineBar(label: Element, progress: number): InlineBar {
  return new InlineBar(label, progress);
}

/**
 * Create a status card
 */
export function statusCard(label: Element, content: Element, style?: BorderStyle): StatusCard;
export function statusCard(style: BorderStyle): (label: Element, content: Element) => StatusCard;
export function statusCard(
  labelOrStyle: Element | BorderStyle, 
  content?: Element, 
  style?: BorderStyle
): StatusCard | ((label: Element, content: Element) => StatusCard) {
  if (typeof labelOrStyle === 'object' && 'render' in labelOrStyle) {
    // Called with label first
    return new StatusCard(labelOrStyle, content!, style ?? BorderStyle.Single);
  } else {
    // Called with style first - return curried function
    return (label: Element, content: Element) => new StatusCard(label, content, labelOrStyle as BorderStyle);
  }
}

/**
 * Create a table
 */
export function table(headers: Element[], rows: Element[][], style?: BorderStyle): Table;
export function table(style: BorderStyle): (headers: Element[], rows: Element[][]) => Table;
export function table(
  headersOrStyle: Element[] | BorderStyle, 
  rows?: Element[][], 
  style?: BorderStyle
): Table | ((headers: Element[], rows: Element[][]) => Table) {
  if (Array.isArray(headersOrStyle)) {
    // Called with headers first
    return new Table(headersOrStyle, rows!, style ?? BorderStyle.Single);
  } else {
    // Called with style first - return curried function
    return (headers: Element[], rows: Element[][]) => new Table(headers, rows, headersOrStyle);
  }
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
    const lines = content.split('\n');
    const targetWidth = this.width ?? 80; // Default width if not specified
    
    return lines.map(line => {
      const visibleLength = stripAnsiCodes(line).length;
      if (visibleLength >= targetWidth) {
        return line; // Don't center if already too long
      }
      
      const padding = targetWidth - visibleLength;
      const leftPad = Math.floor(padding / 2);
      const rightPad = padding - leftPad;
      return " ".repeat(leftPad) + line + " ".repeat(rightPad);
    }).join('\n');
  }
}

/**
 * Center an element
 */
export function center(element: Element, width?: number): Center {
  return new Center(element, width);
}

/**
 * Add underline to an element with custom character
 */
export function underline(char = "─") {
  return (element: Element) => new Underline(element, char);
}

/**
 * Add underline to an element (direct usage)
 */
export function underlined(element: Element, char = "─"): Underline {
  return new Underline(element, char);
}

/**
 * Add a prefix margin to elements
 */
export function margin(prefix: string) {
  return (...elements: Element[]) => new Margin(prefix, elements);
}

/**
 * Create a tree structure - can be used as both a Tree and a function
 */
export function tree(label: string): any {
  const baseTree = new Tree(label, []);
  
  const result = function(...children: Tree[]) {
    return new Tree(label, children);
  } as any;
  
  // Copy all Tree methods to the function
  result.render = () => baseTree.render();
  result.label = label;
  result.children = [];
  
  return result;
}

/**
 * Predefined status margins with color coding
 */
export const margins = {
  error: (...elements: Element[]) => new Margin("[\u001b[31merror\u001b[0m]", elements),
  warn: (...elements: Element[]) => new Margin("[\u001b[33mwarn\u001b[0m]", elements),
  success: (...elements: Element[]) => new Margin("[\u001b[32msuccess\u001b[0m]", elements),
  info: (...elements: Element[]) => new Margin("[\u001b[36minfo\u001b[0m]", elements),
};

/**
 * Underline element that adds underline below content
 */
export class Underline implements Element {
  constructor(
    private element: Element,
    private char: string = "─"
  ) {}
  
  render(): string {
    const content = this.element.render();
    const lines = content.split('\n');
    const maxWidth = Math.max(...lines.map(line => stripAnsiCodes(line).length));
    
    // Create underline by repeating the pattern to match the width
    let underlineStr = "";
    while (underlineStr.length < maxWidth) {
      underlineStr += this.char;
    }
    underlineStr = underlineStr.substring(0, maxWidth);
    
    return content + "\n" + underlineStr;
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
    const content = this.elements.length === 1 
      ? this.elements[0] 
      : new Layout(this.elements);
    
    const lines = content.render().split('\n');
    return lines.map(line => `${this.prefix} ${line}`).join('\n');
  }
}

// Export additional utilities for convenience
export {
  DIMENSIONS,
  GLYPHS,
  stripAnsiCodes
};