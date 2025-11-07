{-# LANGUAGE OverloadedStrings, ExistentialQuantification, FlexibleInstances #-}

{- | 
Module      : Layoutz
Description : Friendly, expressive print-layout DSL for Haskell
Copyright   : (c) 2025 Matthieu Court
License     : Apache-2.0

A simple Haskell port of the layoutz library for creating structured terminal layouts.
-}

module Layoutz
  ( -- * Core Types
    Element(..)
  , Border(..)
  , HasBorder(..)
  , Color(..)
  , L
  , Tree(..)
    -- * Basic Elements
  , layout
  , text
  , br
    -- * Layout Functions  
  , center, center'
  , row
  , underline, underline', underlineColored
  , alignLeft, alignRight, alignCenter, justify, wrap
    -- * Containers
  , box
  , statusCard
    -- * Widgets
  , ul
  , ol
  , inlineBar
  , table
  , section, section', section''
  , kv
  , tree, leaf, branch
    -- * Visual Elements
  , margin
  , hr, hr', hr''
  , vr, vr', vr''
  , pad
  , chart
    -- * Border utilities
  , withBorder
    -- * Color utilities
  , withColor
    -- * Rendering
  , render
  ) where

import Data.List (intercalate, transpose)
import Data.String (IsString(..))
import Text.Printf (printf)

-- | Strip ANSI escape codes from a string for accurate width calculation
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':rest) = stripAnsi (dropAfterM rest)
  where 
    dropAfterM [] = []
    dropAfterM ('m':xs) = xs
    dropAfterM (_:xs) = dropAfterM xs
stripAnsi (c:rest) = c : stripAnsi rest

-- | Returns width of a character in a monospace terminal: 0 for combining
-- characters, 1 for regular characters, 2 for East Asian wide and emoji.
charWidth :: Char -> Int
charWidth c
  | c < '\x0300' = 1  -- Fast path for ASCII and common Latin
  | c >= '\x0300' && c < '\x0370' = 0  -- Combining diacriticals
  | c >= '\x1100' && c < '\x1200' = 2  -- Hangul Jamo
  | c >= '\x2E80' && c < '\x9FFF' = 2  -- CJK
  | c >= '\xAC00' && c < '\xD7A4' = 2  -- Hangul Syllables
  | c >= '\xF900' && c < '\xFB00' = 2  -- CJK Compatibility Ideographs
  | c >= '\xFE10' && c < '\xFE20' = 2  -- Vertical forms
  | c >= '\xFE30' && c < '\xFE70' = 2  -- CJK Compatibility Forms
  | c >= '\xFF00' && c < '\xFF61' = 2  -- Fullwidth Forms
  | c >= '\xFFE0' && c < '\xFFE7' = 2  -- Fullwidth symbols
  | c >= '\x1F000' = 2  -- Emoji, symbols, supplementary ideographs
  | c >= '\x20000' && c < '\x2FFFF' = 2  -- Supplementary ideographs
  | c >= '\x30000' && c < '\x3FFFF' = 2  -- Tertiary ideographs
  | otherwise = 1

-- | Calculate real terminal width of string (handles ANSI, emoji, CJK)
realLength :: String -> Int
realLength = sum . map charWidth . stripAnsi

-- | Calculate visible width (ignoring ANSI codes, handling wide chars)
visibleLength :: String -> Int
visibleLength = realLength

-- | Helper: pad a string to a target width on the right (ANSI-aware)
padRight :: Int -> String -> String
padRight targetWidth str = str ++ replicate (max 0 (targetWidth - visibleLength str)) ' '

-- | Helper: pad a string to a target width on the left (ANSI-aware)
padLeft :: Int -> String -> String
padLeft targetWidth str = replicate (max 0 (targetWidth - visibleLength str)) ' ' ++ str

-- | Helper: center a string within a target width (ANSI-aware)
centerString :: Int -> String -> String
centerString targetWidth str
  | len >= targetWidth = str
  | otherwise = leftPad ++ str ++ rightPad
  where
    len = visibleLength str
    totalPadding = targetWidth - len
    leftPad = replicate (totalPadding `div` 2) ' '
    rightPad = replicate (totalPadding - length leftPad) ' '

-- | Helper: justify text (spread words evenly to fill width)
justifyString :: Int -> String -> String
justifyString targetWidth str
  | len >= targetWidth = str
  | length ws <= 1 = str  -- Can't justify single word
  | otherwise = intercalate "" $ zipWith (++) ws spaces
  where
    ws = words str
    len = length str
    wordLengths = sum (map length ws)
    totalSpaces = targetWidth - wordLengths
    gaps = length ws - 1
    baseSpaces = totalSpaces `div` gaps
    extraSpaces = totalSpaces `mod` gaps
    spaces = replicate extraSpaces (replicate (baseSpaces + 1) ' ') 
             ++ replicate (gaps - extraSpaces) (replicate baseSpaces ' ')
             ++ [""]  -- No space after last word

-- Core Element typeclass
class Element a where
  renderElement :: a -> String
  
  -- Calculate element width (longest line)
  width :: a -> Int
  width element = 
    let rendered = renderElement element
        renderedLines = lines rendered
    in if null renderedLines then 0
       else maximum $ 0 : map visibleLength renderedLines
  
  -- Calculate element height (number of lines)
  height :: a -> Int
  height element =
    let rendered = renderElement element
    in if null rendered then 1
       else length (lines rendered)

render :: Element a => a -> String
render = renderElement

-- | L is the universal layout element type - a type-erased wrapper for the DSL.
-- 
-- This allows mixing different element types in layouts while providing a common interface.
-- Uses existential quantification to store any Element type inside L.
--
-- Constructors:
--   * L a          - Wraps any Element (Text, Box, Table, etc.)
--   * UL [L]       - Special case for unordered lists (allows nesting)  
--   * AutoCenter L - Smart centering that adapts to layout context width
--   * LBox, LStatusCard, LTable - Specialized constructors for bordered elements
--
-- Example usage:
--   layout [text "title", box "content" [...], center (text "footer")]
--   All different types unified as L, so they can be composed together.
data L = forall a. Element a => L a 
       | UL [L] 
       | OL [L]
       | AutoCenter L
       | Colored Color L
       | LBox String [L] Border
       | LStatusCard String String Border
       | LTable [String] [[L]] Border

instance Element L where
  renderElement (L x) = render x
  renderElement (UL items) = render (UnorderedList items)
  renderElement (OL items) = render (OrderedList items)
  renderElement (AutoCenter element) = render element  -- Will be handled by Layout
  renderElement (Colored color element) = 
    let rendered = render element
        renderedLines = lines rendered
        coloredLines = map (wrapAnsi color) renderedLines
        -- Preserve whether original had trailing newline
        hasTrailingNewline = not (null rendered) && last rendered == '\n'
    in if hasTrailingNewline 
       then unlines coloredLines 
       else intercalate "\n" coloredLines
  renderElement (LBox title elements border) = render (Box title elements border)
  renderElement (LStatusCard label content border) = render (StatusCard label content border)
  renderElement (LTable headers rows border) = render (Table headers rows border)
  
  width (L x) = width x
  width (UL items) = width (UnorderedList items)
  width (OL items) = width (OrderedList items)
  width (AutoCenter element) = width element
  width (Colored _ element) = width element  -- Width ignores color
  width (LBox title elements border) = width (Box title elements border)
  width (LStatusCard label content border) = width (StatusCard label content border)
  width (LTable headers rows border) = width (Table headers rows border)
  
  height (L x) = height x  
  height (UL items) = height (UnorderedList items)
  height (OL items) = height (OrderedList items)
  height (AutoCenter element) = height element
  height (Colored _ element) = height element  -- Height ignores color
  height (LBox title elements border) = height (Box title elements border)
  height (LStatusCard label content border) = height (StatusCard label content border)
  height (LTable headers rows border) = height (Table headers rows border)

instance Show L where
  show = render

-- | Enable string literals to be used directly as elements with OverloadedStrings
-- 
-- With OverloadedStrings enabled, you can write:
--   layout ["Hello", "World"]  instead of  layout [text "Hello", text "World"]
instance IsString L where
  fromString = text

-- Border styles
data Border = BorderNormal | BorderDouble | BorderThick | BorderRound | BorderNone
  deriving (Show, Eq)

-- | Typeclass for elements that support customizable borders
class HasBorder a where
  -- | Set the border style for an element
  setBorder :: Border -> a -> a

instance HasBorder L where
  setBorder border (LBox title elements _) = LBox title elements border
  setBorder border (LStatusCard label content _) = LStatusCard label content border
  setBorder border (LTable headers rows _) = LTable headers rows border
  setBorder border (Colored color element) = Colored color (setBorder border element)
  setBorder _ other = other  -- Non-bordered elements remain unchanged

-- Color support with ANSI codes
data Color = ColorBlack | ColorRed | ColorGreen | ColorYellow 
           | ColorBlue | ColorMagenta | ColorCyan | ColorWhite
           | ColorBrightBlack | ColorBrightRed | ColorBrightGreen | ColorBrightYellow 
           | ColorBrightBlue | ColorBrightMagenta | ColorBrightCyan | ColorBrightWhite
  deriving (Show, Eq)

-- | Get ANSI foreground color code
colorCode :: Color -> String
colorCode ColorBlack         = "30"
colorCode ColorRed           = "31"
colorCode ColorGreen         = "32"
colorCode ColorYellow        = "33"
colorCode ColorBlue          = "34"
colorCode ColorMagenta       = "35"
colorCode ColorCyan          = "36"
colorCode ColorWhite         = "37"
colorCode ColorBrightBlack   = "90"
colorCode ColorBrightRed     = "91"
colorCode ColorBrightGreen   = "92"
colorCode ColorBrightYellow  = "93"
colorCode ColorBrightBlue    = "94"
colorCode ColorBrightMagenta = "95"
colorCode ColorBrightCyan    = "96"
colorCode ColorBrightWhite   = "97"

-- | Wrap text with ANSI color codes
wrapAnsi :: Color -> String -> String
wrapAnsi color str = "\ESC[" ++ colorCode color ++ "m" ++ str ++ "\ESC[0m"

borderChars :: Border -> (String, String, String, String, String, String, String, String, String)
borderChars BorderNormal = ("┌", "┐", "└", "┘", "─", "│", "├", "┤", "┼")
borderChars BorderDouble = ("╔", "╗", "╚", "╝", "═", "║", "╠", "╣", "╬") 
borderChars BorderThick  = ("┏", "┓", "┗", "┛", "━", "┃", "┣", "┫", "╋")
borderChars BorderRound  = ("╭", "╮", "╰", "╯", "─", "│", "├", "┤", "┼")
borderChars BorderNone   = (" ", " ", " ", " ", " ", " ", " ", " ", " ")

-- Elements
newtype Text = Text String
instance Element Text where renderElement (Text s) = s

newtype LineBreak = LineBreak ()  
instance Element LineBreak where renderElement _ = ""

data Layout = Layout [L]
instance Element Layout where
  renderElement (Layout elements) = 
    let -- Calculate max width of all non-AutoCenter elements
        nonAutoCenterElements = [e | e <- elements, not (isAutoCenter e)]
        maxWidth = if null nonAutoCenterElements 
                  then 80  -- fallback
                  else maximum (0 : map width nonAutoCenterElements)
        
        -- Render elements, providing context width to AutoCenter elements
        renderedElements = map (renderWithContext maxWidth) elements
    in intercalate "\n" renderedElements
    where
      isAutoCenter (AutoCenter _) = True
      isAutoCenter _ = False
      
      renderWithContext contextWidth (AutoCenter element) = 
        render (Centered (render element) contextWidth)
      renderWithContext _ element = render element

-- | Centered element with custom width
data Centered = Centered String Int  -- content, target_width
instance Element Centered where
  renderElement (Centered content targetWidth) = 
    intercalate "\n" $ map (centerString targetWidth) (lines content)

-- | Underlined element with custom character
data Underlined = Underlined String String (Maybe Color)  -- content, underline_char, optional color
instance Element Underlined where
  renderElement (Underlined content underlineChar maybeColor) = 
    let contentLines = lines content
        maxWidth = if null contentLines then 0 
                   else maximum (map visibleLength contentLines)
        underlinePattern = underlineChar
        underlinePart = if length underlinePattern >= maxWidth
                   then take maxWidth underlinePattern
                   else let repeats = maxWidth `div` length underlinePattern
                            remainder = maxWidth `mod` length underlinePattern
                        in concat (replicate repeats underlinePattern) ++ take remainder underlinePattern
        coloredUnderline = case maybeColor of
          Nothing -> underlinePart
          Just color -> wrapAnsi color underlinePart
    in content ++ "\n" ++ coloredUnderline

data Row = Row [L]
instance Element Row where  
  renderElement (Row elements) 
    | null elements = ""
    | otherwise = intercalate "\n" $ map (intercalate " ") (transpose paddedElements)
    where
      elementStrings = map render elements
      elementLines = map lines elementStrings
      maxHeight = maximum (map length elementLines)
      elementWidths = map (maximum . map visibleLength) elementLines
      paddedElements = zipWith padElement elementWidths elementLines
      
      padElement :: Int -> [String] -> [String]
      padElement cellWidth linesList = 
        let currentLines = linesList ++ replicate (maxHeight - length linesList) ""
        in map (padRight cellWidth) currentLines

-- | Text alignment options
data Alignment = AlignLeft | AlignRight | AlignCenter | Justify
  deriving (Show, Eq)

-- | Aligned text with specified width and alignment
data AlignedText = AlignedText String Int Alignment  -- content, width, alignment
instance Element AlignedText where
  renderElement (AlignedText content targetWidth alignment) = 
    let alignFn = case alignment of
          AlignLeft   -> padRight targetWidth
          AlignRight  -> padLeft targetWidth
          AlignCenter -> centerString targetWidth
          Justify     -> justifyString targetWidth
    in intercalate "\n" $ map alignFn (lines content)

data Box = Box String [L] Border

instance HasBorder Box where
  setBorder border (Box title elements _) = Box title elements border

instance Element Box where
  renderElement (Box title elements border) =
    let elementStrings = map render elements
        content = intercalate "\n" elementStrings
        contentLines = if null content then [""] else lines content  
        contentWidth = maximum (0 : map length contentLines)
        titleWidth = if null title then 0 else length title + 2
        innerWidth = max contentWidth titleWidth
        totalWidth = innerWidth + 4
        (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical, _, _, _) = borderChars border
        hChar = head horizontal
        
        topBorder 
          | null title = topLeft ++ replicate (totalWidth - 2) hChar ++ topRight
          | otherwise  = let titlePadding = totalWidth - length title - 2
                             leftPad = titlePadding `div` 2  
                             rightPad = titlePadding - leftPad
                         in topLeft ++ replicate leftPad hChar ++ title ++ replicate rightPad hChar ++ topRight
               
        bottomBorder = bottomLeft ++ replicate (totalWidth - 2) hChar ++ bottomRight
        paddedContent = map (\line -> vertical ++ " " ++ padRight innerWidth line ++ " " ++ vertical) contentLines
          
    in intercalate "\n" (topBorder : paddedContent ++ [bottomBorder])

data StatusCard = StatusCard String String Border

instance HasBorder StatusCard where
  setBorder border (StatusCard label content _) = StatusCard label content border

instance Element StatusCard where
  renderElement (StatusCard label content border) =
    let labelLines = lines label
        contentLines = lines content
        allLines = labelLines ++ contentLines
        maxWidth = maximum (0 : map length allLines)
        contentWidth = maxWidth + 2
        (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical, _, _, _) = borderChars border
        hChar = head horizontal
        
        topBorder = topLeft ++ replicate (contentWidth + 2) hChar ++ topRight
        bottomBorder = bottomLeft ++ replicate (contentWidth + 2) hChar ++ bottomRight
        createCardLine line = vertical ++ " " ++ padRight contentWidth line ++ " " ++ vertical
        
    in intercalate "\n" $ [topBorder] ++ map createCardLine allLines ++ [bottomBorder]

-- | Margin element that adds prefix to each line
data Margin = Margin String [L]  -- prefix, elements
instance Element Margin where
  renderElement (Margin prefix elements) = 
    let content = case elements of
                    [single] -> render single
                    _        -> render (Layout elements)
    in intercalate "\n" $ map ((prefix ++ " ") ++) (lines content)

-- | Horizontal rule with custom character and width  
data HorizontalRule = HorizontalRule String Int  -- char, width
instance Element HorizontalRule where
  renderElement (HorizontalRule char ruleWidth) = concat (replicate ruleWidth char)

-- | Vertical rule with custom character and height
data VerticalRule = VerticalRule String Int  -- char, height
instance Element VerticalRule where
  renderElement (VerticalRule char ruleHeight) = intercalate "\n" (replicate ruleHeight char)

-- | Padded element with padding around all sides
data Padded = Padded String Int  -- content, padding
instance Element Padded where
  renderElement (Padded content padding) = 
    let contentLines = lines content
        maxWidth = maximum (0 : map length contentLines)
        horizontalPad = replicate padding ' '
        totalWidth = maxWidth + padding * 2
        verticalPad = replicate totalWidth ' '
        paddedLines = map (\line -> horizontalPad ++ padRight maxWidth line ++ horizontalPad) contentLines
        verticalLines = replicate padding verticalPad
    in intercalate "\n" (verticalLines ++ paddedLines ++ verticalLines)

-- | Chart for data visualization
data Chart = Chart [(String, Double)]  -- (label, value) pairs
instance Element Chart where
  renderElement (Chart dataPoints) 
    | null dataPoints = "No data"
    | otherwise = intercalate "\n" $ map renderBar dataPoints
    where
      maxValue = maximum (0 : map snd dataPoints)
      maxLabelWidth = minimum [15, maximum (0 : map (length . fst) dataPoints)]
      chartWidth = 40
      
      renderBar :: (String, Double) -> String
      renderBar (label, value) = 
        let truncatedLabel 
              | length label > maxLabelWidth = take (maxLabelWidth - 3) label ++ "..."
              | otherwise = label
            paddedLabel = padRight maxLabelWidth truncatedLabel
            percentage = value / maxValue
            barLength = floor (percentage * fromIntegral chartWidth)
            bar = replicate barLength '█' ++ replicate (chartWidth - barLength) '─'
            valueStr 
              | value == fromInteger (round value) = show (round value :: Integer)
              | otherwise = printf "%.1f" value
        in paddedLabel ++ " │" ++ bar ++ "│ " ++ valueStr

-- | Table with headers and borders (fixed alignment)
data Table = Table [String] [[L]] Border  -- headers, rows, border

instance HasBorder Table where
  setBorder border (Table headers rows _) = Table headers rows border

instance Element Table where
  renderElement (Table headers rows border) = 
    let normalizedRows = map (normalizeRow (length headers)) rows
        columnWidths = calculateColumnWidths headers normalizedRows
        (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical, leftTee, rightTee, cross) = borderChars border
        hChar = head horizontal
        
        -- Fixed border construction with proper connectors
        topConnector = case border of
          BorderRound -> "┬"
          BorderNormal -> "┬"
          BorderDouble -> "╦" 
          BorderThick -> "┳"
          BorderNone -> " "
        topParts = map (\w -> replicate w hChar) columnWidths
        topBorder = topLeft ++ [hChar] ++ intercalate ([hChar] ++ topConnector ++ [hChar]) topParts ++ [hChar] ++ topRight
        
        -- Create proper separator with tee connectors
        separatorParts = map (\w -> replicate w hChar) columnWidths
        separatorBorder = leftTee ++ [hChar] ++ intercalate ([hChar] ++ cross ++ [hChar]) separatorParts ++ [hChar] ++ rightTee
        
        -- Create proper bottom border with bottom connectors
        bottomConnector = case border of
          BorderRound -> "┴"  -- Special case for round borders
          BorderNormal -> "┴"
          BorderDouble -> "╩" 
          BorderThick -> "┻"
          BorderNone -> " "
        bottomParts = map (\w -> replicate w hChar) columnWidths  
        bottomBorder = bottomLeft ++ [hChar] ++ intercalate ([hChar] ++ bottomConnector ++ [hChar]) bottomParts ++ [hChar] ++ bottomRight
        
        -- Create header row
        headerCells = zipWith padRight columnWidths headers
        headerRow = vertical ++ " " ++ intercalate (" " ++ vertical ++ " ") headerCells ++ " " ++ vertical
        
        -- Create data rows
        dataRows = concatMap (renderTableRow columnWidths vertical) normalizedRows
        
    in intercalate "\n" ([topBorder, headerRow, separatorBorder] ++ dataRows ++ [bottomBorder])
    where
      normalizeRow :: Int -> [L] -> [L]
      normalizeRow expectedLen rowData 
        | currentLen >= expectedLen = take expectedLen rowData
        | otherwise = rowData ++ replicate (expectedLen - currentLen) (text "")
        where currentLen = length rowData
      
      calculateColumnWidths :: [String] -> [[L]] -> [Int]
      calculateColumnWidths hdrs rws = 
        let headerWidths = map visibleLength hdrs
            rowWidths = map (map (maximum . (0:) . map visibleLength . lines . render)) rws
            allWidths = headerWidths : rowWidths
        in map (maximum . (0:)) (transpose allWidths)
      
      renderTableRow :: [Int] -> String -> [L] -> [String]
      renderTableRow widths vChars rowData = 
        let cellContents = map render rowData
            cellLines = map lines cellContents
            maxCellHeight = maximum (1 : map length cellLines)
            paddedCells = zipWith (padCell maxCellHeight) widths cellLines
            tableRows = transpose paddedCells
        in map (\rowCells -> vChars ++ " " ++ intercalate (" " ++ vChars ++ " ") rowCells ++ " " ++ vChars) tableRows
      
      padCell :: Int -> Int -> [String] -> [String]
      padCell cellHeight cellWidth cellLines =
        let paddedLines = cellLines ++ replicate (cellHeight - length cellLines) ""
        in map (padRight cellWidth) paddedLines

-- | Section with decorative header
data Section = Section String [L] String Int  -- title, content, glyph, flanking_chars
instance Element Section where
  renderElement (Section title content glyph flankingChars) = 
    let header = replicate flankingChars (head glyph) ++ " " ++ title ++ " " ++ replicate flankingChars (head glyph)
        body = render (Layout content)
    in header ++ "\n" ++ body

-- | Key-value pairs with alignment
data KeyValue = KeyValue [(String, String)]
instance Element KeyValue where
  renderElement (KeyValue pairs) = 
    if null pairs then ""
    else let maxKeyLength = maximum (map (realLength . fst) pairs)
             alignmentPosition = maxKeyLength + 2
         in intercalate "\n" $ map (renderPair alignmentPosition) pairs
    where
      renderPair alignPos (key, value) = 
        let keyWithColon = key ++ ":"
            spacesNeeded = alignPos - realLength keyWithColon
            padding = replicate (max 1 spacesNeeded) ' '
        in keyWithColon ++ padding ++ value

-- | Tree structure for hierarchical data
data Tree = Tree String [Tree]

instance Element Tree where
  renderElement treeData = renderTree treeData "" True []
    where
      renderTree (Tree name children) prefix isLast parentPrefixes =
        let nodeLine = if null parentPrefixes 
                      then name
                      else prefix ++ (if isLast then "└── " else "├── ") ++ name
            childPrefix = if null parentPrefixes
                         then ""
                         else prefix ++ (if isLast then "    " else "│   ")
            childLines = zipWith (\child idx -> 
                          renderTree child childPrefix (idx == length children - 1) (parentPrefixes ++ [not isLast])
                        ) children [0..]
        in if null children
           then nodeLine
           else nodeLine ++ "\n" ++ intercalate "\n" childLines


isUnorderedList :: L -> Bool
isUnorderedList (UL _) = True
isUnorderedList _ = False

getUnorderedListItems :: L -> [L]
getUnorderedListItems (UL items) = items
getUnorderedListItems _ = []

newtype UnorderedList = UnorderedList [L]
instance Element UnorderedList where
  renderElement (UnorderedList items) = renderAtLevel 0 items
    where
      bulletStyles = ["•", "◦", "▪"]
      
      renderAtLevel level itemList = 
        let currentBullet = bulletStyles !! (level `mod` length bulletStyles)
            indent = replicate (level * 2) ' '
        in intercalate "\n" $ map (renderItem level indent currentBullet) itemList
      
      renderItem level indent bullet item = 
        if isUnorderedList item
        then renderAtLevel (level + 1) (getUnorderedListItems item)
        else
          let content = render item
              contentLines = lines content
          in case contentLines of
            [singleLine] -> indent ++ bullet ++ " " ++ singleLine
            (firstLine:restLines) -> 
              let firstOutput = indent ++ bullet ++ " " ++ firstLine
                  restIndent = replicate (length indent + length bullet + 1) ' '
                  restOutput = map (restIndent ++) restLines
              in intercalate "\n" (firstOutput : restOutput)
            [] -> indent ++ bullet ++ " "

newtype OrderedList = OrderedList [L]
instance Element OrderedList where
  renderElement (OrderedList items) = renderAtLevel 1 0 items
    where
      renderAtLevel startNum level itemList = 
        let indent = replicate (level * 2) ' '
            numbered = zip [startNum..] itemList
        in intercalate "\n" $ map (renderItem level indent) numbered
      
      renderItem level indent (num, item) = 
        if isOrderedList item
        then renderAtLevel 1 (level + 1) (getOrderedListItems item)
        else
          let numStr = formatNumber level num ++ ". "
              content = render item
              contentLines = lines content
          in case contentLines of
            [singleLine] -> indent ++ numStr ++ singleLine
            (firstLine:restLines) -> 
              let firstOutput = indent ++ numStr ++ firstLine
                  restIndent = replicate (length numStr) ' '
                  restOutput = map ((indent ++ restIndent) ++) restLines
              in intercalate "\n" (firstOutput : restOutput)
            [] -> indent ++ numStr
      
      formatNumber :: Int -> Int -> String
      formatNumber level num = 
        case level `mod` 3 of
          0 -> show num                    -- 1, 2, 3
          1 -> [toEnum (96 + num)]        -- a, b, c
          _ -> toRoman num                 -- i, ii, iii
      
      toRoman :: Int -> String
      toRoman n = case n of
        1 -> "i"
        2 -> "ii"
        3 -> "iii"
        4 -> "iv"
        5 -> "v"
        6 -> "vi"
        7 -> "vii"
        8 -> "viii"
        9 -> "ix"
        10 -> "x"
        _ -> show n  -- fallback for numbers > 10
      
      isOrderedList :: L -> Bool
      isOrderedList (OL _) = True
      isOrderedList _ = False
      
      getOrderedListItems :: L -> [L]
      getOrderedListItems (OL xs) = xs
      getOrderedListItems _ = []

data InlineBar = InlineBar String Double
instance Element InlineBar where
  renderElement (InlineBar label progress) =
    let clampedProgress = max 0.0 (min 1.0 progress)
        barWidth = 20
        filledSegments = floor (clampedProgress * fromIntegral barWidth)
        emptySegments = barWidth - filledSegments
        bar = replicate filledSegments '█' ++ replicate emptySegments '─'
        percentage = floor (clampedProgress * 100) :: Int
    in printf "%s [%s] %d%%" label bar percentage

-- Smart constructors and automatic conversions
text :: String -> L
text s = L (Text s)

br :: L  
br = L (LineBreak ())

center :: Element a => a -> L
center element = AutoCenter (L element)

-- | Center element within specified width
center' :: Element a => Int -> a -> L
center' targetWidth element = L (Centered (render element) targetWidth)

underline :: Element a => a -> L
underline element = L (Underlined (render element) "─" Nothing)

-- | Add underline with custom character
underline' :: Element a => String -> a -> L
underline' char element = L (Underlined (render element) char Nothing)

-- | Add colored underline with custom character and color
-- 
-- Example usage:
--   underlineColored "=" ColorRed $ text "Error Section"
--   underlineColored "~" ColorGreen $ text "Success"
--   underlineColored "─" ColorBrightCyan $ text "Info"
underlineColored :: Element a => String -> Color -> a -> L
underlineColored char color element = L (Underlined (render element) char (Just color))

ul :: [L] -> L
ul items = UL items

ol :: [L] -> L
ol items = OL items

inlineBar :: String -> Double -> L
inlineBar label progress = L (InlineBar label progress)

statusCard :: String -> String -> L
statusCard label content = LStatusCard label content BorderNormal

layout :: [L] -> L
layout elements = L (Layout elements)

row :: [L] -> L  
row elements = L (Row elements)

-- | Align text to the left within specified width
alignLeft :: Int -> String -> L
alignLeft targetWidth content = L (AlignedText content targetWidth AlignLeft)

-- | Align text to the right within specified width
alignRight :: Int -> String -> L
alignRight targetWidth content = L (AlignedText content targetWidth AlignRight)

-- | Align text to the center within specified width
alignCenter :: Int -> String -> L
alignCenter targetWidth content = L (AlignedText content targetWidth AlignCenter)

-- | Justify text (spread words evenly to fill width)
justify :: Int -> String -> L
justify targetWidth content = L (AlignedText content targetWidth Justify)

-- | Wrap text to multiple lines with specified width
wrap :: Int -> String -> L
wrap targetWidth content = 
  let ws = words content
      wrappedLines = wrapWords targetWidth ws
  in layout (map text wrappedLines)
  where
    wrapWords :: Int -> [String] -> [String]
    wrapWords _ [] = []
    wrapWords maxWidth wordsList = 
      let (line, rest) = takeLine maxWidth wordsList
      in line : wrapWords maxWidth rest
    
    takeLine :: Int -> [String] -> (String, [String])
    takeLine _ [] = ("", [])
    takeLine maxWidth (firstWord:restWords)
      | length firstWord > maxWidth = (firstWord, restWords)  -- Word too long, put it on its own line
      | otherwise = go (length firstWord) [firstWord] restWords
      where
        go _ acc [] = (unwords (reverse acc), [])
        go currentLen acc (nextWord:remainingWords)
          | currentLen + 1 + length nextWord <= maxWidth = go (currentLen + 1 + length nextWord) (nextWord:acc) remainingWords
          | otherwise = (unwords (reverse acc), nextWord:remainingWords)

box :: String -> [L] -> L
box title elements = LBox title elements BorderNormal

-- | Create margin with custom prefix
-- 
-- Example usage:
--   margin "[error]" [text "Something went wrong"]
--   margin "[info]" [text "FYI: Check the logs"]
margin :: String -> [L] -> L
margin prefix elements = L (Margin prefix elements)

-- | Horizontal rule with default character and width
hr :: L
hr = L (HorizontalRule "─" 50)

-- | Horizontal rule with custom character  
hr' :: String -> L
hr' char = L (HorizontalRule char 50)

-- | Horizontal rule with custom character and width
hr'' :: String -> Int -> L
hr'' char ruleWidth = L (HorizontalRule char ruleWidth)

-- | Vertical rule with default character and height
vr :: L
vr = L (VerticalRule "│" 10)

-- | Vertical rule with custom character
vr' :: String -> L
vr' char = L (VerticalRule char 10)

-- | Vertical rule with custom character and height
vr'' :: String -> Int -> L
vr'' char ruleHeight = L (VerticalRule char ruleHeight)

-- | Add padding around element
pad :: Element a => Int -> a -> L  
pad padding element = L (Padded (render element) padding)

-- | Create horizontal bar chart
chart :: [(String, Double)] -> L
chart dataPoints = L (Chart dataPoints)

-- | Create table with headers and rows
table :: [String] -> [[L]] -> L
table headers rows = LTable headers rows BorderNormal

-- | Create section with title and content
section :: String -> [L] -> L
section title content = L (Section title content "=" 3)

-- | Create section with custom glyph
section' :: String -> String -> [L] -> L
section' glyph title content = L (Section title content glyph 3)

-- | Create section with custom glyph and flanking chars
section'' :: String -> String -> Int -> [L] -> L
section'' glyph title flanking content = L (Section title content glyph flanking)

-- | Create key-value pairs
kv :: [(String, String)] -> L
kv pairs = L (KeyValue pairs)

-- | Apply a border style to elements that support borders
-- 
-- Elements that support borders: box, statusCard, table
-- Other elements are returned unchanged
--
-- Example usage:
--   withBorder BorderThick $ statusCard "API" "UP"
--   withBorder BorderDouble $ table ["Name"] [[text "Alice"]]
withBorder :: Border -> L -> L
withBorder = setBorder

-- | Apply a color to an element
-- 
-- Example usage:
--   withColor ColorRed $ text "Error!"
--   withColor ColorGreen $ statusCard "Status" "OK"
--   withColor ColorBrightYellow $ box "Warning" [text "Check logs"]
withColor :: Color -> L -> L
withColor color element = Colored color element

-- | Create tree structure
tree :: String -> [Tree] -> L
tree name children = L (Tree name children)

-- | Create leaf tree node (no children)
leaf :: String -> Tree
leaf name = Tree name []

-- | Create branch tree node with children
branch :: String -> [Tree] -> Tree
branch name children = Tree name children

