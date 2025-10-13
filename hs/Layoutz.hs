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
  , L
  , Tree(..)
    -- * Basic Elements
  , layout
  , text
  , br
    -- * Layout Functions  
  , center, center'
  , row
  , underline, underline'
    -- * Containers
  , box, box'
  , statusCard, statusCard'
    -- * Widgets
  , ul
  , inlineBar
  , table, table'
  , section, section', section''
  , kv
  , tree, leaf, branch
    -- * Visual Elements
  , margin, marginError, marginWarn, marginSuccess, marginInfo
  , hr, hr', hr''
  , pad
  , chart
    -- * Rendering
  , render
  ) where

import Data.List (intercalate, transpose)
import Text.Printf (printf)

-- Core Element typeclass
class Element a where
  renderElement :: a -> String
  
  -- Calculate element width (longest line)
  width :: a -> Int
  width element = 
    let rendered = renderElement element
        renderedLines = lines rendered
    in if null renderedLines then 0
       else maximum $ 0 : map length renderedLines
  
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
--
-- Example usage:
--   layout [text "title", box "content" [...], center (text "footer")]
--   All different types unified as L, so they can be composed together.
data L = forall a. Element a => L a | UL [L] | AutoCenter L

instance Element L where
  renderElement (L x) = render x
  renderElement (UL items) = render (UnorderedList items)
  renderElement (AutoCenter element) = render element  -- Will be handled by Layout
  
  width (L x) = width x
  width (UL items) = width (UnorderedList items)
  width (AutoCenter element) = width element
  
  height (L x) = height x  
  height (UL items) = height (UnorderedList items)
  height (AutoCenter element) = height element

instance Show L where
  show = render

-- Border styles
data Border = NormalBorder | DoubleBorder | ThickBorder | RoundBorder
  deriving (Show, Eq)

borderChars :: Border -> (String, String, String, String, String, String, String, String, String)
borderChars NormalBorder = ("┌", "┐", "└", "┘", "─", "│", "├", "┤", "┼")
borderChars DoubleBorder = ("╔", "╗", "╚", "╝", "═", "║", "╠", "╣", "╬") 
borderChars ThickBorder  = ("┏", "┓", "┗", "┛", "━", "┃", "┣", "┫", "╋")
borderChars RoundBorder  = ("╭", "╮", "╰", "╯", "─", "│", "├", "┤", "┼")

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
    let contentLines = lines content
    in intercalate "\n" $ map centerLine contentLines
    where
      centerLine line = 
        let lineLength = length line
        in if lineLength >= targetWidth
           then line
           else let totalPadding = targetWidth - lineLength
                    leftPadding = totalPadding `div` 2
                    rightPadding = totalPadding - leftPadding
                in replicate leftPadding ' ' ++ line ++ replicate rightPadding ' '

-- | Underlined element with custom character
data Underlined = Underlined String String  -- content, underline_char
instance Element Underlined where
  renderElement (Underlined content underlineChar) = 
    let contentLines = lines content
        maxWidth = if null contentLines then 0 
                   else maximum (map length contentLines)
        underlinePattern = underlineChar
        underline = if length underlinePattern >= maxWidth
                   then take maxWidth underlinePattern
                   else let repeats = maxWidth `div` length underlinePattern
                            remainder = maxWidth `mod` length underlinePattern
                        in concat (replicate repeats underlinePattern) ++ take remainder underlinePattern
    in content ++ "\n" ++ underline

data Row = Row [L]
instance Element Row where  
  renderElement (Row elements) = 
    if null elements then ""
    else let elementStrings = map render elements
             elementLines = map lines elementStrings
             maxHeight = maximum (map length elementLines)
             elementWidths = map (maximum . map length) elementLines
            -- Pad each element to maxHeight and proper width, then align at top
             paddedElements = zipWith (padElementToSize maxHeight) elementWidths elementLines
             -- Transpose to get rows, then join each row with spaces
         in intercalate "\n" $ map (intercalate " ") (transpose paddedElements)
    where
      padElementToSize maxH width linesList = 
        let currentLines = linesList ++ replicate (maxH - length linesList) ""
            paddedLines = map (\line -> line ++ replicate (max 0 (width - length line)) ' ') currentLines
        in paddedLines

data Box = Box String [L] Border
instance Element Box where
  renderElement (Box title elements border) =
    let elementStrings = map render elements
        content = intercalate "\n" elementStrings
        contentLines = if null content then [""] else lines content  
        contentWidth = if null contentLines then 0 else maximum (map length contentLines)
        titleWidth = if null title then 0 else length title + 2
        innerWidth = max contentWidth titleWidth
        totalWidth = innerWidth + 4
        (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical, _, _, _) = borderChars border
        
        topBorder = if null title
          then topLeft ++ replicate (totalWidth - 2) (head horizontal) ++ topRight
          else let titlePadding = totalWidth - length title - 2
                   leftPad = titlePadding `div` 2  
                   rightPad = titlePadding - leftPad
               in topLeft ++ replicate leftPad (head horizontal) ++ title ++ replicate rightPad (head horizontal) ++ topRight
               
        bottomBorder = bottomLeft ++ replicate (totalWidth - 2) (head horizontal) ++ bottomRight
        
        paddedContent = map (\line -> 
          vertical ++ " " ++ line ++ replicate (innerWidth - length line) ' ' ++ " " ++ vertical) contentLines
          
    in intercalate "\n" (topBorder : paddedContent ++ [bottomBorder])

data StatusCard = StatusCard String String Border
instance Element StatusCard where
  renderElement (StatusCard label content border) =
    let labelLines = lines label
        contentLines = lines content
        allLines = labelLines ++ contentLines
        maxWidth = if null allLines then 0 else maximum (map length allLines)
        contentWidth = maxWidth + 2
        (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical, _, _, _) = borderChars border
        
        topBorder = topLeft ++ replicate (contentWidth + 2) (head horizontal) ++ topRight
        bottomBorder = bottomLeft ++ replicate (contentWidth + 2) (head horizontal) ++ bottomRight
        
        createCardLines ls = map (\line ->
          vertical ++ " " ++ line ++ replicate (contentWidth - length line) ' ' ++ " " ++ vertical) ls
        labelCardLines = createCardLines labelLines  
        contentCardLines = createCardLines contentLines
        
    in intercalate "\n" ([topBorder] ++ labelCardLines ++ contentCardLines ++ [bottomBorder])

-- | Margin element that adds prefix to each line
data Margin = Margin String [L]  -- prefix, elements
instance Element Margin where
  renderElement (Margin prefix elements) = 
    let content = if length elements == 1 
                  then render (head elements)
                  else render (Layout elements)
        contentLines = lines content
    in intercalate "\n" $ map (\line -> prefix ++ " " ++ line) contentLines

-- | Horizontal rule with custom character and width  
data HorizontalRule = HorizontalRule String Int  -- char, width
instance Element HorizontalRule where
  renderElement (HorizontalRule char width) = concat (replicate width char)


-- | Padded element with padding around all sides
data Padded = Padded String Int  -- content, padding
instance Element Padded where
  renderElement (Padded content padding) = 
    let contentLines = lines content
        maxWidth = if null contentLines then 0 else maximum (0 : map length contentLines)
        horizontalPad = replicate padding ' '
        verticalPad = replicate (maxWidth + padding * 2) ' '
        paddedLines = map (\line -> horizontalPad ++ line ++ replicate (maxWidth - length line) ' ' ++ horizontalPad) contentLines
        verticalLines = replicate padding verticalPad
    in intercalate "\n" (verticalLines ++ paddedLines ++ verticalLines)

-- | Chart for data visualization
data Chart = Chart [(String, Double)]  -- (label, value) pairs
instance Element Chart where
  renderElement (Chart dataPoints) = 
    if null dataPoints then "No data"
    else let maxValue = maximum (0 : map snd dataPoints)
             maxLabelWidth = minimum [15, maximum (0 : map (length . fst) dataPoints)]
             chartWidth = 40
         in intercalate "\n" $ map (renderBar maxValue maxLabelWidth chartWidth) dataPoints
    where
      renderBar maxVal labelWidth barWidth (label, value) = 
        let truncatedLabel = if length label > labelWidth 
                            then take (labelWidth - 3) label ++ "..."
                            else label
            paddedLabel = truncatedLabel ++ replicate (labelWidth - length truncatedLabel) ' '
            percentage = value / maxVal
            barLength = floor (percentage * fromIntegral barWidth)
            bar = replicate barLength '█'
            emptyBar = replicate (barWidth - barLength) '─'
            valueStr = if value == fromInteger (round value) 
                      then show (round value)
                      else printf "%.1f" value
        in paddedLabel ++ " │" ++ bar ++ emptyBar ++ "│ " ++ valueStr

-- | Table with headers and borders (fixed alignment)
data Table = Table [String] [[L]] Border  -- headers, rows, border
instance Element Table where
  renderElement (Table headers rows border) = 
    let normalizedRows = map (normalizeRow (length headers)) rows
        columnWidths = calculateColumnWidths headers normalizedRows
        (topLeft, topRight, bottomLeft, bottomRight, horizontal, vertical, leftTee, rightTee, cross) = borderChars border
        
        -- Calculate actual table width based on content and separators
        totalContentWidth = sum columnWidths
        totalSeparatorWidth = (length columnWidths - 1) * 3  -- " | " between columns
        totalWidth = totalContentWidth + totalSeparatorWidth + 4  -- 4 for outer borders and padding
        hChar = head horizontal
        
        -- Fixed border construction with proper connectors
        topConnector = case border of
          RoundBorder -> "┬"
          NormalBorder -> "┬"
          DoubleBorder -> "╦" 
          ThickBorder -> "┳"
        topParts = map (\w -> replicate w hChar) columnWidths
        topBorder = topLeft ++ [hChar] ++ intercalate ([hChar] ++ topConnector ++ [hChar]) topParts ++ [hChar] ++ topRight
        
        -- Create proper separator with tee connectors
        separatorParts = map (\w -> replicate w hChar) columnWidths
        separatorBorder = leftTee ++ [hChar] ++ intercalate ([hChar] ++ cross ++ [hChar]) separatorParts ++ [hChar] ++ rightTee
        
        -- Create proper bottom border with bottom connectors
        bottomConnector = case border of
          RoundBorder -> "┴"  -- Special case for round borders
          NormalBorder -> "┴"
          DoubleBorder -> "╩" 
          ThickBorder -> "┻"
        bottomParts = map (\w -> replicate w hChar) columnWidths  
        bottomBorder = bottomLeft ++ [hChar] ++ intercalate ([hChar] ++ bottomConnector ++ [hChar]) bottomParts ++ [hChar] ++ bottomRight
        
        -- Create header row
        headerCells = zipWith padToWidth columnWidths headers
        headerRow = vertical ++ " " ++ intercalate (" " ++ vertical ++ " ") headerCells ++ " " ++ vertical
        
        -- Create data rows
        dataRows = concatMap (renderTableRow columnWidths vertical) normalizedRows
        
    in intercalate "\n" ([topBorder, headerRow, separatorBorder] ++ dataRows ++ [bottomBorder])
    where
      normalizeRow expectedLen row = 
        let currentLen = length row
        in if currentLen >= expectedLen 
           then take expectedLen row
           else row ++ replicate (expectedLen - currentLen) (text "")
      
      calculateColumnWidths hdrs rws = 
        let headerWidths = map length hdrs
            rowWidths = map (map (safeMaxWidth . lines . render)) rws
            allWidths = headerWidths : rowWidths
        in map (maximum . (0:)) (transpose allWidths)
        where
          safeMaxWidth [] = 0
          safeMaxWidth linesList = maximum (0 : map length linesList)
      
      padToWidth width str = str ++ replicate (max 0 (width - length str)) ' '
      
      renderTableRow widths vChars row = 
        let cellContents = map render row
            cellLines = map lines cellContents
            maxCellHeight = if null cellLines then 1 else maximum (1 : map length cellLines)
            paddedCells = zipWith (padCellToSize maxCellHeight) widths cellLines
            tableRows = [[paddedCells !! j !! i | j <- [0..length paddedCells - 1]] | i <- [0..maxCellHeight - 1]]
        in map (\rowCells -> vChars ++ " " ++ intercalate (" " ++ vChars ++ " ") rowCells ++ " " ++ vChars) tableRows
      
      padCellToSize height width cellLines =
        let paddedLines = cellLines ++ replicate (height - length cellLines) ""
        in map (\line -> line ++ replicate (max 0 (width - length line)) ' ') paddedLines

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
    else let maxKeyLength = maximum (map (length . fst) pairs)
             alignmentPosition = maxKeyLength + 2
         in intercalate "\n" $ map (renderPair alignmentPosition) pairs
    where
      renderPair alignPos (key, value) = 
        let keyWithColon = key ++ ":"
            spacesNeeded = alignPos - length keyWithColon
            padding = replicate (max 1 spacesNeeded) ' '
        in keyWithColon ++ padding ++ value

-- | Tree structure for hierarchical data
data Tree = Tree String [Tree]
instance Element Tree where
  renderElement tree = renderTree tree "" True []
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
center' width element = L (Centered (render element) width)

underline :: Element a => a -> L
underline element = L (Underlined (render element) "─")

-- | Add underline with custom character
underline' :: Element a => String -> a -> L
underline' char element = L (Underlined (render element) char)

ul :: [L] -> L
ul items = UL items

inlineBar :: String -> Double -> L
inlineBar label progress = L (InlineBar label progress)

statusCard :: String -> String -> L
statusCard label content = L (StatusCard label content NormalBorder)

-- | Status card with custom border
statusCard' :: Border -> String -> String -> L
statusCard' border label content = L (StatusCard label content border)

layout :: [L] -> L
layout elements = L (Layout elements)

row :: [L] -> L  
row elements = L (Row elements)

box :: String -> [L] -> L
box title elements = L (Box title elements NormalBorder)

-- | Box with custom border  
box' :: Border -> String -> [L] -> L
box' border title elements = L (Box title elements border)

-- | Create margin with custom prefix
margin :: String -> [L] -> L
margin prefix elements = L (Margin prefix elements)

-- | Predefined status margins
marginError, marginWarn, marginSuccess, marginInfo :: [L] -> L
marginError elements = L (Margin "[error]" elements)
marginWarn elements = L (Margin "[warn]" elements)  
marginSuccess elements = L (Margin "[success]" elements)
marginInfo elements = L (Margin "[info]" elements)

-- | Horizontal rule with default character and width
hr :: L
hr = L (HorizontalRule "─" 50)

-- | Horizontal rule with custom character  
hr' :: String -> L
hr' char = L (HorizontalRule char 50)

-- | Horizontal rule with custom character and width
hr'' :: String -> Int -> L
hr'' char width = L (HorizontalRule char width)

-- | Add padding around element
pad :: Element a => Int -> a -> L  
pad padding element = L (Padded (render element) padding)

-- | Create horizontal bar chart
chart :: [(String, Double)] -> L
chart dataPoints = L (Chart dataPoints)

-- | Create table with headers and rows
table :: [String] -> [[L]] -> L
table headers rows = L (Table headers rows NormalBorder)

-- | Create table with custom border
table' :: Border -> [String] -> [[L]] -> L
table' border headers rows = L (Table headers rows border)

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

-- | Create tree structure
tree :: String -> [Tree] -> L
tree name children = L (Tree name children)

-- | Create leaf tree node (no children)
leaf :: String -> Tree
leaf name = Tree name []

-- | Create branch tree node with children
branch :: String -> [Tree] -> Tree
branch name children = Tree name children

