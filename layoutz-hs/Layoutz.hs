{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Layoutz
Description : Friendly, expressive print-layout DSL for Haskell
Copyright   : (c) 2026 Matthieu Court
License     : Apache-2.0

A simple Haskell port of the layoutz library for creating structured terminal layouts.
-}

module Layoutz
  ( -- * Core Types
    Element(..)
  , Border(..)
  , HasBorder(..)
  , Color(..)
  , Style(..)
  , L
  , Tree(..)
    -- * Basic Elements
  , layout
  , text
  , br
    -- * Layout Functions
  , center, center'
  , row, tightRow
  , columns, columns'
  , underline, underline', underlineColored
  , alignLeft, alignRight, alignCenter, justify, wrap
  , truncate', truncate''
    -- * Containers
  , box
  , banner
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
    -- * Spinners
  , spinner
  , SpinnerStyle(..)
    -- * Visualizations
  , plotSparkline
  , Series(..), plotLine
  , Slice(..), plotPie
  , BarItem(..), plotBar
  , StackedBarGroup(..), plotStackedBar
  , HeatmapData(..), plotHeatmap, plotHeatmap'
    -- * Border utilities
  , withBorder
    -- * Color utilities
  , withColor
    -- * Style utilities
  , withStyle
    -- * Rendering
  , render
  , renderText
    -- * TUI Runtime
  , LayoutzApp(..)
  , Key(..)
  , Cmd(..)
  , cmdFire
  , cmdTask
  , cmdAfterMs
  , executeCmd
  , cmdIsExit
  , Sub(..)
  , AppOptions(..)
  , defaultAppOptions
  , AppAlignment(..)
  , runApp
  , runAppFinal
  , runAppWith
  , runAppWithFinal
  , runInline
    -- * Subscriptions
  , subKeyPress
  , subEveryMs
  , subBatch
    -- * Kitty graphics
  , KittyImage(..)
  , kittyImage
  , kittyRGB
  , kittyRGBA
  , kittyImageFile
    -- * Progress loaders
  , LoaderStyle(..)
  , styleBlocks, styleBar, styleAscii, styleDots, styleLine, stylePipes, loaderStyles
  , loader, loaderStyled, loaderStream, loaderStreamStyled
    -- * Interactive prompts (Ask)
  , askInput
  , askConfirm
  , askChoose
  , askChooseMany
  , askSpin
  , askWrite
  , askFilter
  , askFile
  , askPager
  ) where

import Data.List (intercalate, transpose, nub, sortOn, isPrefixOf)
import qualified Data.Text
import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR)
import Data.Word (Word8)
import Data.String (IsString(..))
import Data.Char (ord, chr, toLower, isDigit)
import Text.Printf (printf)
import System.IO
import Control.Exception (catch, finally, throwIO, AsyncException(..), IOException, SomeException)
import System.Timeout (timeout)
import Control.Monad (when, unless, forever, forM)
import Control.Concurrent (forkIO, threadDelay, killThread, newChan, writeChan, readChan)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef')
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (listDirectory, doesDirectoryExist, doesPathExist, canonicalizePath)

-- | Strip ANSI escape codes from a string for accurate width calculation.
-- Also drops kitty graphics APC escapes (@ESC _ G ... ESC \\@) so an inline
-- image's transmit block doesn't get counted as visible width.
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':rest) = stripAnsi (dropAfterM rest)
  where
    dropAfterM [] = []
    dropAfterM ('m':xs) = xs
    dropAfterM (_:xs) = dropAfterM xs
stripAnsi ('\ESC':'_':rest) = stripAnsi (dropAfterST rest)
  where
    -- APC string terminates at ST (ESC \)
    dropAfterST [] = []
    dropAfterST ('\ESC':'\\':xs) = xs
    dropAfterST (_:xs) = dropAfterST xs
stripAnsi (c:rest) = c : stripAnsi rest

-- | Returns width of a character in a monospace terminal: 0 for combining
-- characters, 1 for regular characters, 2 for East Asian wide and emoji.
charWidth :: Char -> Int
charWidth c
  | c < '\x0300' = 1                     -- Fast path for ASCII and common Latin
  | c == kittyPlaceholderChar = 1        -- Kitty Unicode placeholder (one cell)
  | isRowColumnDiacritic c = 0           -- Kitty row/column combining diacritics
  | c >= '\x0300' && c < '\x0370' = 0    -- Combining diacriticals
  | c >= '\x1100' && c < '\x1200' = 2    -- Hangul Jamo
  | c >= '\x2E80' && c < '\x9FFF' = 2    -- CJK
  | c >= '\xAC00' && c < '\xD7A4' = 2    -- Hangul Syllables
  | c >= '\xF900' && c < '\xFB00' = 2    -- CJK Compatibility Ideographs
  | c >= '\xFE10' && c < '\xFE20' = 2    -- Vertical forms
  | c >= '\xFE30' && c < '\xFE70' = 2    -- CJK Compatibility Forms
  | c >= '\xFF00' && c < '\xFF61' = 2    -- Fullwidth Forms
  | c >= '\xFFE0' && c < '\xFFE7' = 2    -- Fullwidth symbols
  | c >= '\x1F000' = 2                   -- Emoji, symbols, supplementary ideographs
  | c >= '\x20000' && c < '\x2FFFF' = 2  -- Supplementary ideographs
  | c >= '\x30000' && c < '\x3FFFF' = 2  -- Tertiary ideographs
  | otherwise = 1

-- | Calculate visible width of string (handles ANSI codes, emoji, CJK)
visibleLength :: String -> Int
visibleLength = sum . map charWidth . stripAnsi

-- | The Unicode placeholder code point (U+10EEEE) the kitty graphics protocol
-- uses to reserve one terminal cell per pixel-block of an inline image.
kittyPlaceholderChar :: Char
kittyPlaceholderChar = '\x10EEEE'

-- | Row/column combining diacritics used by the kitty placeholder protocol to
-- encode a cell's (row, column) into the placeholder glyph. Each renders with
-- zero width. The table's length also caps the maximum image dimensions.
rowColumnDiacritics :: [Int]
rowColumnDiacritics =
  [ 0x0305, 0x030d, 0x030e, 0x0310, 0x0312, 0x033d, 0x033e, 0x033f, 0x0346
  , 0x034a, 0x034b, 0x034c, 0x0350, 0x0351, 0x0352, 0x0357, 0x035b, 0x0363
  , 0x0364, 0x0365, 0x0366, 0x0367, 0x0368, 0x0369, 0x036a, 0x036b, 0x036c
  , 0x036d, 0x036e, 0x036f, 0x0483, 0x0484, 0x0485, 0x0486, 0x0487, 0x0592
  , 0x0593, 0x0594, 0x0595, 0x0597, 0x0598, 0x0599, 0x059c, 0x059d, 0x059e
  , 0x059f, 0x05a0, 0x05a1, 0x05a8, 0x05a9, 0x05ab, 0x05ac, 0x05af, 0x05c4
  , 0x0610, 0x0611, 0x0612, 0x0613, 0x0614, 0x0615, 0x0616, 0x0617, 0x0657
  , 0x0658, 0x0659, 0x065a, 0x065b, 0x065d, 0x065e, 0x06d6, 0x06d7, 0x06d8
  , 0x06d9, 0x06da, 0x06db, 0x06dc, 0x06df, 0x06e0, 0x06e1, 0x06e2, 0x06e4
  , 0x06e7, 0x06e8, 0x06eb, 0x06ec, 0x0730, 0x0732, 0x0733, 0x0735, 0x0736
  , 0x073a, 0x073d, 0x073f, 0x0740, 0x0741, 0x0743, 0x0745, 0x0747, 0x0749
  , 0x074a, 0x07eb, 0x07ec, 0x07ed, 0x07ee, 0x07ef, 0x07f0, 0x07f1, 0x07f3
  , 0x0816, 0x0817, 0x0818, 0x0819, 0x081b, 0x081c, 0x081d, 0x081e, 0x081f
  , 0x0820, 0x0821, 0x0822, 0x0823, 0x0825, 0x0826, 0x0827, 0x0829, 0x082a
  , 0x082b, 0x082c, 0x082d, 0x0951, 0x0953, 0x0954, 0x0f82, 0x0f83, 0x0f86
  , 0x0f87, 0x135d, 0x135e, 0x135f, 0x17dd, 0x193a, 0x1a17, 0x1a75, 0x1a76
  , 0x1a77, 0x1a78, 0x1a79, 0x1a7a, 0x1a7b, 0x1a7c, 0x1b6b, 0x1b6d, 0x1b6e
  , 0x1b6f, 0x1b70, 0x1b71, 0x1b72, 0x1b73, 0x1cd0, 0x1cd1, 0x1cd2, 0x1cda
  , 0x1cdb, 0x1ce0, 0x1dc0, 0x1dc1, 0x1dc3, 0x1dc4, 0x1dc5, 0x1dc6, 0x1dc7
  , 0x1dc8, 0x1dc9, 0x1dcb, 0x1dcc, 0x1dd1, 0x1dd2, 0x1dd3, 0x1dd4, 0x1dd5
  , 0x1dd6, 0x1dd7, 0x1dd8, 0x1dd9, 0x1dda, 0x1ddb, 0x1ddc, 0x1ddd, 0x1dde
  , 0x1ddf, 0x1de0, 0x1de1, 0x1de2, 0x1de3, 0x1de4, 0x1de5, 0x1de6, 0x1dfe
  , 0x20d0, 0x20d1, 0x20d4, 0x20d5, 0x20d6, 0x20d7, 0x20db, 0x20dc, 0x20e1
  , 0x20e7, 0x20e9, 0x20f0, 0x2cef, 0x2cf0, 0x2cf1, 0x2de0, 0x2de1, 0x2de2
  , 0x2de3, 0x2de4, 0x2de5, 0x2de6, 0x2de7, 0x2de8, 0x2de9, 0x2dea, 0x2deb
  , 0x2dec, 0x2ded, 0x2dee, 0x2def, 0x2df0, 0x2df1, 0x2df2, 0x2df3, 0x2df4
  , 0x2df5, 0x2df6, 0x2df7, 0x2df8, 0x2df9, 0x2dfa, 0x2dfb, 0x2dfc, 0x2dfd
  , 0x2dfe, 0x2dff, 0xa66f, 0xa67c, 0xa67d, 0xa6f0, 0xa6f1, 0xa8e0, 0xa8e1
  , 0xa8e2, 0xa8e3, 0xa8e4, 0xa8e5, 0xa8e6, 0xa8e7, 0xa8e8, 0xa8e9, 0xa8ea
  , 0xa8eb, 0xa8ec, 0xa8ed, 0xa8ee, 0xa8ef, 0xa8f0, 0xa8f1, 0xaab0, 0xaab2
  , 0xaab3, 0xaab7, 0xaab8, 0xaabe, 0xaabf, 0xaac1, 0xfe20, 0xfe21, 0xfe22
  , 0xfe23, 0xfe24, 0xfe25, 0xfe26, 0x10a0f, 0x10a38, 0x1d185, 0x1d186
  , 0x1d187, 0x1d188, 0x1d189, 0x1d1aa, 0x1d1ab, 0x1d1ac, 0x1d1ad, 0x1d242
  , 0x1d243, 0x1d244
  ]

-- | O(1)-ish membership test for the zero-width kitty diacritics above.
rowColumnDiacriticsSet :: IntSet.IntSet
rowColumnDiacriticsSet = IntSet.fromList rowColumnDiacritics

isRowColumnDiacritic :: Char -> Bool
isRowColumnDiacritic c = IntSet.member (ord c) rowColumnDiacriticsSet

-- | Apply a function to each line, preserving trailing newlines
mapLines :: (String -> String) -> String -> String
mapLines f str
  | null str  = str
  | otherwise = let ls = lines str
                    hasTrailingNewline = last str == '\n'
                in if hasTrailingNewline
                   then unlines (map f ls)
                   else intercalate "\n" (map f ls)

-- | Helper: pad a string to a target width on the right (ANSI-aware)
padRight :: Int -> String -> String
padRight targetWidth str = str <> replicate (max 0 (targetWidth - visibleLength str)) ' '

-- | Helper: pad a string to a target width on the left (ANSI-aware)
padLeft :: Int -> String -> String
padLeft targetWidth str = replicate (max 0 (targetWidth - visibleLength str)) ' ' <> str

-- | Helper: center a string within a target width (ANSI-aware)
centerString :: Int -> String -> String
centerString targetWidth str
  | len >= targetWidth = str
  | otherwise = leftPad <> str <> rightPad
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
  | otherwise = intercalate "" $ zipWith (<>) ws spaces
  where
    ws = words str
    len = length str
    wordLengths = sum (map length ws)
    totalSpaces = targetWidth - wordLengths
    gaps = length ws - 1
    baseSpaces = totalSpaces `div` gaps
    extraSpaces = totalSpaces `mod` gaps
    spaces = replicate extraSpaces (replicate (baseSpaces + 1) ' ')
             <> replicate (gaps - extraSpaces) (replicate baseSpaces ' ')
             <> [""]  -- No space after last word

-- | Core Element typeclass
class Element a where
  renderElement :: a -> String

  -- | Calculate element width (longest line)
  width :: a -> Int
  width element =
    let rendered = renderElement element
        renderedLines = lines rendered
    in if null renderedLines then 0
       else maximum $ 0 : map visibleLength renderedLines

  -- | Calculate element height (number of lines)
  height :: a -> Int
  height element =
    let rendered = renderElement element
    in if null rendered then 1
       else length (lines rendered)

render :: Element a => a -> String
render = renderElement

renderText :: Element a => a -> Data.Text.Text
renderText = T.pack . renderElement

-- | L is the universal layout element type - a type-erased wrapper for the DSL.
--
-- This allows mixing different element types in layouts while providing a common interface.
-- Uses existential quantification to store any Element type inside L.
--
-- Constructors:
--
-- * @L a@          - Wraps any Element (Text, Box, Table, etc.)
-- * @UL [L]@       - Special case for unordered lists (allows nesting)
-- * @AutoCenter L@ - Smart centering that adapts to layout context width
-- * @LBox@, @LStatusCard@, @LTable@ - Specialized constructors for bordered elements
--
-- Example usage:
--
-- @
-- 'layout' ['text' "title", 'box' "content" [...], 'center' ('text' "footer")]
-- @
--
-- All different types unified as L, so they can be composed together.
data L = forall a. Element a => L a
       | UL [L]
       | OL [L]
       | AutoCenter L
       | Colored Color L
       | Styled Style L
       | LBox String [L] Border
       | LStatusCard String String Border
       | LTable [String] [[L]] Border

instance Element L where
  renderElement (L x) = render x
  renderElement (UL items) = render (UnorderedList items)
  renderElement (OL items) = render (OrderedList items)
  renderElement (AutoCenter element) = render element  -- Will be handled by Layout
  renderElement (Colored color element) = mapLines (wrapAnsi color) (render element)
  renderElement (Styled style element) = mapLines (wrapStyle style) (render element)
  renderElement (LBox title elements border) = render (Box title elements border)
  renderElement (LStatusCard label content border) = render (StatusCard label content border)
  renderElement (LTable headers rows border) = render (Table headers rows border)

  width (L x) = width x
  width (UL items) = width (UnorderedList items)
  width (OL items) = width (OrderedList items)
  width (AutoCenter element) = width element
  width (Colored _ element) = width element  -- Width ignores color
  width (Styled _ element) = width element  -- Width ignores style
  width (LBox title elements border) = width (Box title elements border)
  width (LStatusCard label content border) = width (StatusCard label content border)
  width (LTable headers rows border) = width (Table headers rows border)

  height (L x) = height x
  height (UL items) = height (UnorderedList items)
  height (OL items) = height (OrderedList items)
  height (AutoCenter element) = height element
  height (Colored _ element) = height element  -- Height ignores color
  height (Styled _ element) = height element  -- Height ignores style
  height (LBox title elements border) = height (Box title elements border)
  height (LStatusCard label content border) = height (StatusCard label content border)
  height (LTable headers rows border) = height (Table headers rows border)

instance Show L where
  show = render

-- | Enable string literals to be used directly as elements with OverloadedStrings
--
-- With OverloadedStrings enabled, you can write @layout [\"Hello\", \"World\"]@
-- instead of @layout [text \"Hello\", text \"World\"]@.
instance IsString L where
  fromString = text

-- | Border styles
data Border
  = BorderNormal
  | BorderDouble
  | BorderThick
  | BorderRound
  | BorderAscii
  | BorderBlock
  | BorderDashed
  | BorderDotted
  | BorderInnerHalfBlock
  | BorderOuterHalfBlock
  | BorderMarkdown
  | BorderCustom String String String  -- ^ corner, horizontal, vertical
  | BorderNone
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
  setBorder border (Styled style element) = Styled style (setBorder border element)
  setBorder _ other = other  -- Non-bordered elements remain unchanged

-- | Color support with ANSI codes
data Color = ColorDefault | ColorBlack | ColorRed | ColorGreen | ColorYellow
           | ColorBlue | ColorMagenta | ColorCyan | ColorWhite
           | ColorBrightBlack | ColorBrightRed | ColorBrightGreen | ColorBrightYellow
           | ColorBrightBlue | ColorBrightMagenta | ColorBrightCyan | ColorBrightWhite
           | ColorFull Int           -- ^ 256-color palette (0-255)
           | ColorTrue Int Int Int   -- ^ 24-bit RGB true color (r, g, b)
  deriving (Show, Eq)

-- | Get ANSI foreground color code
colorCode :: Color -> String
colorCode ColorDefault       = ""
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
colorCode (ColorFull n)      = "38;5;" <> show (clamp n)
colorCode (ColorTrue r g b)  = "38;2;" <> show (clamp r) <> ";" <> show (clamp g) <> ";" <> show (clamp b)

-- | Clamp value to 0-255 range for color codes
clamp :: Int -> Int
clamp = max 0 . min 255

-- | Wrap text with ANSI color codes
wrapAnsi :: Color -> String -> String
wrapAnsi color str
  | null (colorCode color) = str
  | otherwise = "\ESC[" <> colorCode color <> "m" <> str <> "\ESC[0m"

-- | Style support with ANSI codes
data Style = StyleDefault | StyleBold | StyleDim | StyleItalic | StyleUnderline
           | StyleBlink | StyleReverse | StyleHidden | StyleStrikethrough
           | StyleCombined [Style]  -- ^ Combine multiple styles
  deriving (Show, Eq)

-- | Combine styles using @<>@
instance Semigroup Style where
  StyleDefault <> other = other
  other <> StyleDefault = other
  StyleCombined styles1 <> StyleCombined styles2 = StyleCombined (styles1 <> styles2)
  StyleCombined styles <> style = StyleCombined (styles <> [style])
  style <> StyleCombined styles = StyleCombined (style : styles)
  style1 <> style2 = StyleCombined [style1, style2]

instance Monoid Style where
  mempty = StyleDefault

-- | Get ANSI style code
styleCode :: Style -> String
styleCode StyleDefault       = ""
styleCode StyleBold          = "1"
styleCode StyleDim           = "2"
styleCode StyleItalic        = "3"
styleCode StyleUnderline     = "4"
styleCode StyleBlink         = "5"
styleCode StyleReverse       = "7"
styleCode StyleHidden        = "8"
styleCode StyleStrikethrough = "9"
styleCode (StyleCombined styles) =
  let codes = filter (not . null) (map styleCode styles)
  in if null codes then "" else intercalate ";" codes

-- | Wrap text with ANSI style codes
wrapStyle :: Style -> String -> String
wrapStyle style str
  | null (styleCode style) = str
  | otherwise = "\ESC[" <> styleCode style <> "m" <> str <> "\ESC[0m"

-- | Border character set supporting asymmetric borders (e.g. half-block styles)
data BorderChars = BorderChars
  { bcTL, bcTR, bcBL, bcBR              :: String   -- corners
  , bcHTop, bcHBottom                   :: String   -- horizontal (top vs bottom)
  , bcVLeft, bcVRight                   :: String   -- vertical (left vs right)
  , bcLeftTee, bcRightTee, bcCross      :: String   -- separator connectors
  , bcTopTee, bcBottomTee               :: String   -- top/bottom column connectors
  }

-- | Helper for symmetric borders (hTop == hBottom, vLeft == vRight)
mkSymmetric :: String -> String -> String -> String -> String -> String
            -> String -> String -> String -> String -> String -> BorderChars
mkSymmetric tl tr bl br' h v lt rt cross tt bt = BorderChars
  { bcTL = tl, bcTR = tr, bcBL = bl, bcBR = br'
  , bcHTop = h, bcHBottom = h
  , bcVLeft = v, bcVRight = v
  , bcLeftTee = lt, bcRightTee = rt, bcCross = cross
  , bcTopTee = tt, bcBottomTee = bt
  }

borderChars :: Border -> BorderChars
borderChars BorderNormal = mkSymmetric "┌" "┐" "└" "┘" "─" "│" "├" "┤" "┼" "┬" "┴"
borderChars BorderDouble = mkSymmetric "╔" "╗" "╚" "╝" "═" "║" "╠" "╣" "╬" "╦" "╩"
borderChars BorderThick  = mkSymmetric "┏" "┓" "┗" "┛" "━" "┃" "┣" "┫" "╋" "┳" "┻"
borderChars BorderRound  = mkSymmetric "╭" "╮" "╰" "╯" "─" "│" "├" "┤" "┼" "┬" "┴"
borderChars BorderAscii  = mkSymmetric "+" "+" "+" "+" "-" "|" "+" "+" "+" "+" "+"
borderChars BorderBlock  = mkSymmetric "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█"
borderChars BorderDashed = mkSymmetric "┌" "┐" "└" "┘" "╌" "╎" "├" "┤" "┼" "┬" "┴"
borderChars BorderDotted = mkSymmetric "┌" "┐" "└" "┘" "┈" "┊" "├" "┤" "┼" "┬" "┴"
borderChars BorderInnerHalfBlock = BorderChars
  { bcTL = "▗", bcTR = "▖", bcBL = "▝", bcBR = "▘"
  , bcHTop = "▄", bcHBottom = "▀"
  , bcVLeft = "▐", bcVRight = "▌"
  , bcLeftTee = "▐", bcRightTee = "▌", bcCross = "▄"
  , bcTopTee = "▄", bcBottomTee = "▀"
  }
borderChars BorderOuterHalfBlock = BorderChars
  { bcTL = "▛", bcTR = "▜", bcBL = "▙", bcBR = "▟"
  , bcHTop = "▀", bcHBottom = "▄"
  , bcVLeft = "▌", bcVRight = "▐"
  , bcLeftTee = "▌", bcRightTee = "▐", bcCross = "▀"
  , bcTopTee = "▀", bcBottomTee = "▄"
  }
borderChars BorderMarkdown = mkSymmetric "|" "|" "|" "|" "-" "|" "|" "|" "|" "|" "|"
borderChars (BorderCustom corner h v) = mkSymmetric corner corner corner corner h v corner corner corner corner corner
borderChars BorderNone = mkSymmetric " " " " " " " " " " " " " " " " " " " " " "

-- Elements
newtype Text = Text String
instance Element Text where renderElement (Text s) = s

data LineBreak = LineBreak
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
        repeats = maxWidth `div` length underlineChar
        remainder = maxWidth `mod` length underlineChar
        underlinePart = concat (replicate repeats underlineChar) <> take remainder underlineChar
        coloredUnderline = maybe underlinePart (`wrapAnsi` underlinePart) maybeColor
    in content <> "\n" <> coloredUnderline

data Row = Row [L] Bool  -- elements, tight (no spacing)
instance Element Row where
  renderElement (Row elements tight)
    | null elements = ""
    | otherwise = intercalate "\n" $ map (intercalate separator) (transpose paddedElements)
    where
      separator = if tight then "" else " "
      elementStrings = map render elements
      elementLines = map lines elementStrings
      maxHeight = maximum (map length elementLines)
      elementWidths = map (maximum . map visibleLength) elementLines
      paddedElements = zipWith padElement elementWidths elementLines

      padElement :: Int -> [String] -> [String]
      padElement cellWidth linesList =
        let currentLines = linesList <> replicate (maxHeight - length linesList) ""
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
        contentWidth = maximum (0 : map visibleLength contentLines)
        titleWidth = if null title then 0 else visibleLength title + 2
        innerWidth = max contentWidth titleWidth
        totalWidth = innerWidth + 4
        BorderChars{..} = borderChars border
        hTopChar = head bcHTop
        hBottomChar = head bcHBottom

        topBorder
          | null title = bcTL <> replicate (totalWidth - 2) hTopChar <> bcTR
          | otherwise  = let titlePadding = totalWidth - visibleLength title - 2
                             leftPad = titlePadding `div` 2
                             rightPad = titlePadding - leftPad
                         in bcTL <> replicate leftPad hTopChar <> title <> replicate rightPad hTopChar <> bcTR

        bottomBorder = bcBL <> replicate (totalWidth - 2) hBottomChar <> bcBR
        paddedContent = map (\line -> bcVLeft <> " " <> padRight innerWidth line <> " " <> bcVRight) contentLines

    in intercalate "\n" (topBorder : paddedContent <> [bottomBorder])

data StatusCard = StatusCard String String Border

instance HasBorder StatusCard where
  setBorder border (StatusCard label content _) = StatusCard label content border

instance Element StatusCard where
  renderElement (StatusCard label content border) =
    let labelLines = lines label
        contentLines = lines content
        allLines = labelLines <> contentLines
        maxWidth = maximum (0 : map visibleLength allLines)
        contentWidth = maxWidth + 2
        BorderChars{..} = borderChars border
        hTopChar = head bcHTop
        hBottomChar = head bcHBottom

        topBorder = bcTL <> replicate (contentWidth + 2) hTopChar <> bcTR
        bottomBorder = bcBL <> replicate (contentWidth + 2) hBottomChar <> bcBR
        createCardLine line = bcVLeft <> " " <> padRight contentWidth line <> " " <> bcVRight

    in intercalate "\n" $ [topBorder] <> map createCardLine allLines <> [bottomBorder]

-- | Margin element that adds prefix to each line
data Margin = Margin String [L]  -- prefix, elements
instance Element Margin where
  renderElement (Margin prefix elements) =
    let content = case elements of
                    [single] -> render single
                    _        -> render (Layout elements)
    in intercalate "\n" $ map ((prefix <> " ") <>) (lines content)

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
        paddedLines = map (\line -> horizontalPad <> padRight maxWidth line <> horizontalPad) contentLines
        verticalLines = replicate padding verticalPad
    in intercalate "\n" (verticalLines <> paddedLines <> verticalLines)

-- | Columns: place elements side by side, each padded to its own width
data Columns = Columns [L] Int  -- elements, spacing between columns
instance Element Columns where
  renderElement (Columns elements spacing)
    | null elements = ""
    | otherwise = intercalate "\n" $ map (intercalate separator) (transpose paddedElements)
    where
      separator = replicate spacing ' '
      elementLines = map (lines . render) elements
      maxHeight = maximum (map length elementLines)
      elementWidths = map (maximum . (0 :) . map visibleLength) elementLines
      paddedElements = zipWith padColumn elementWidths elementLines

      padColumn :: Int -> [String] -> [String]
      padColumn cellWidth linesList =
        let filled = linesList <> replicate (maxHeight - length linesList) ""
        in map (padRight cellWidth) filled

-- | Truncate each line with an ellipsis when it exceeds the max width
data Truncated = Truncated String Int String  -- content, maxWidth, ellipsis
instance Element Truncated where
  renderElement (Truncated content maxWidth ellipsis) =
    intercalate "\n" $ map truncateLine (lines content)
    where
      truncateLine line
        | visibleLength line <= maxWidth = line
        | truncateAt <= 0                = take maxWidth ellipsis
        | otherwise                      = take truncateAt (stripAnsi line) <> ellipsis
        where truncateAt = maxWidth - length ellipsis

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
              | length label > maxLabelWidth = take (maxLabelWidth - 3) label <> "..."
              | otherwise = label
            paddedLabel = padRight maxLabelWidth truncatedLabel
            percentage = value / maxValue
            barLength = floor (percentage * fromIntegral chartWidth)
            bar = replicate barLength '█' <> replicate (chartWidth - barLength) '─'
            valueStr
              | value == fromInteger (round value) = show (round value :: Integer)
              | otherwise = printf "%.1f" value
        in paddedLabel <> " │" <> bar <> "│ " <> valueStr

-- | Table with headers and borders (fixed alignment)
data Table = Table [String] [[L]] Border  -- headers, rows, border

instance HasBorder Table where
  setBorder border (Table headers rows _) = Table headers rows border

instance Element Table where
  renderElement (Table headers rows border) =
    let normalizedRows = map (normalizeRow (length headers)) rows
        columnWidths = calculateColumnWidths headers normalizedRows
        BorderChars{..} = borderChars border
        hTopChar = head bcHTop
        hBottomChar = head bcHBottom

        -- Top border
        topParts = map (\w -> replicate w hTopChar) columnWidths
        topBorder = bcTL <> [hTopChar] <> intercalate ([hTopChar] <> bcTopTee <> [hTopChar]) topParts <> [hTopChar] <> bcTR

        -- Separator (between header and data)
        separatorParts = map (\w -> replicate w hTopChar) columnWidths
        separatorBorder = bcLeftTee <> [hTopChar] <> intercalate ([hTopChar] <> bcCross <> [hTopChar]) separatorParts <> [hTopChar] <> bcRightTee

        -- Bottom border
        bottomParts = map (\w -> replicate w hBottomChar) columnWidths
        bottomBorder = bcBL <> [hBottomChar] <> intercalate ([hBottomChar] <> bcBottomTee <> [hBottomChar]) bottomParts <> [hBottomChar] <> bcBR

        -- Header row
        headerCells = zipWith padRight columnWidths headers
        headerRow = bcVLeft <> " " <> intercalate (" " <> bcVLeft <> " ") headerCells <> " " <> bcVRight

        -- Data rows
        dataRows = concatMap (renderTableRow columnWidths bcVLeft bcVRight) normalizedRows

    in intercalate "\n" ([topBorder, headerRow, separatorBorder] <> dataRows <> [bottomBorder])
    where
      normalizeRow :: Int -> [L] -> [L]
      normalizeRow expectedLen rowData
        | currentLen >= expectedLen = take expectedLen rowData
        | otherwise = rowData <> replicate (expectedLen - currentLen) (text "")
        where currentLen = length rowData

      calculateColumnWidths :: [String] -> [[L]] -> [Int]
      calculateColumnWidths hdrs rws =
        let headerWidths = map visibleLength hdrs
            rowWidths = map (map (maximum . (0:) . map visibleLength . lines . render)) rws
            allWidths = headerWidths : rowWidths
        in map (maximum . (0:)) (transpose allWidths)

      renderTableRow :: [Int] -> String -> String -> [L] -> [String]
      renderTableRow widths vLeft vRight rowData =
        let cellContents = map render rowData
            cellLines = map lines cellContents
            maxCellHeight = maximum (1 : map length cellLines)
            paddedCells = zipWith (padCell maxCellHeight) widths cellLines
            tableRows = transpose paddedCells
        in map (\rowCells -> vLeft <> " " <> intercalate (" " <> vLeft <> " ") rowCells <> " " <> vRight) tableRows

      padCell :: Int -> Int -> [String] -> [String]
      padCell cellHeight cellWidth cellLines =
        let paddedLines = cellLines <> replicate (cellHeight - length cellLines) ""
        in map (padRight cellWidth) paddedLines

-- | Section with decorative header
data Section = Section String [L] String Int  -- title, content, glyph, flanking_chars
instance Element Section where
  renderElement (Section title content glyph flankingChars) =
    let header = replicate flankingChars (head glyph) <> " " <> title <> " " <> replicate flankingChars (head glyph)
        body = render (Layout content)
    in header <> "\n" <> body

-- | Key-value pairs with alignment
data KeyValue = KeyValue [(String, String)]
instance Element KeyValue where
  renderElement (KeyValue pairs) =
    if null pairs then ""
    else let maxKeyLength = maximum (map (visibleLength . fst) pairs)
             alignmentPosition = maxKeyLength + 2
         in intercalate "\n" $ map (renderPair alignmentPosition) pairs
    where
      renderPair alignPos (key, value) =
        let keyWithColon = key <> ":"
            spacesNeeded = alignPos - visibleLength keyWithColon
            padding = replicate (max 1 spacesNeeded) ' '
        in keyWithColon <> padding <> value

-- | Tree structure for hierarchical data
data Tree = Tree String [Tree]

instance Element Tree where
  renderElement treeData = renderTree treeData "" True []
    where
      renderTree (Tree name children) prefix isLast parentPrefixes =
        let nodeLine = if null parentPrefixes
                      then name
                      else prefix <> (if isLast then "└── " else "├── ") <> name
            childPrefix = if null parentPrefixes
                         then ""
                         else prefix <> (if isLast then "    " else "│   ")
            childLines = zipWith (\child idx ->
                          renderTree child childPrefix (idx == length children - 1) (parentPrefixes <> [not isLast])
                        ) children [0..]
        in if null children
           then nodeLine
           else nodeLine <> "\n" <> intercalate "\n" childLines


newtype UnorderedList = UnorderedList [L]
instance Element UnorderedList where
  renderElement (UnorderedList items) = renderAtLevel 0 items
    where
      bulletStyles = ["•", "◦", "▪"]

      renderAtLevel level itemList =
        let currentBullet = bulletStyles !! (level `mod` length bulletStyles)
            indent = replicate (level * 2) ' '
        in intercalate "\n" $ map (renderItem level indent currentBullet) itemList

      renderItem level indent bullet item = case item of
        UL nested -> renderAtLevel (level + 1) nested
        _ -> let content = render item
                 contentLines = lines content
             in case contentLines of
               [singleLine] -> indent <> bullet <> " " <> singleLine
               (firstLine:restLines) ->
                 let firstOutput = indent <> bullet <> " " <> firstLine
                     restIndent = replicate (length indent + length bullet + 1) ' '
                     restOutput = map (restIndent <>) restLines
                 in intercalate "\n" (firstOutput : restOutput)
               [] -> indent <> bullet <> " "

newtype OrderedList = OrderedList [L]
instance Element OrderedList where
  renderElement (OrderedList items) = renderAtLevel 1 0 items
    where
      renderAtLevel startNum level itemList =
        let indent = replicate (level * 2) ' '
            numbered = zip [startNum..] itemList
        in intercalate "\n" $ map (renderItem level indent) numbered

      renderItem level indent (num, item) = case item of
        OL nested -> renderAtLevel 1 (level + 1) nested
        _ -> let numStr = formatNumber level num <> ". "
                 content = render item
                 contentLines = lines content
             in case contentLines of
               [singleLine] -> indent <> numStr <> singleLine
               (firstLine:restLines) ->
                 let firstOutput = indent <> numStr <> firstLine
                     restIndent = replicate (length numStr) ' '
                     restOutput = map ((indent <> restIndent) <>) restLines
                 in intercalate "\n" (firstOutput : restOutput)
               [] -> indent <> numStr

      formatNumber :: Int -> Int -> String
      formatNumber lvl num = case lvl `mod` 3 of
        0 -> show num              -- 1, 2, 3
        1 -> [toEnum (96 + num)]   -- a, b, c
        _ -> toRoman num           -- i, ii, iii

      toRoman :: Int -> String
      toRoman = \case
        1  -> "i";   2  -> "ii";  3  -> "iii"; 4  -> "iv"; 5  -> "v"
        6  -> "vi";  7  -> "vii"; 8  -> "viii"; 9  -> "ix"; 10 -> "x"
        n  -> show n

data InlineBar = InlineBar String Double
instance Element InlineBar where
  renderElement (InlineBar label progress) =
    let clampedProgress = max 0.0 (min 1.0 progress)
        barWidth = 20
        filledSegments = floor (clampedProgress * fromIntegral barWidth)
        emptySegments = barWidth - filledSegments
        bar = replicate filledSegments '█' <> replicate emptySegments '─'
        percentage = floor (clampedProgress * 100) :: Int
    in printf "%s [%s] %d%%" label bar percentage

-- Smart constructors and automatic conversions
text :: String -> L
text s = L (Text s)

br :: L
br = L LineBreak

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
-- @
-- 'underlineColored' "=" 'ColorRed' $ 'text' "Error Section"
-- 'underlineColored' "~" 'ColorGreen' $ 'text' "Success"
-- 'underlineColored' "─" 'ColorBrightCyan' $ 'text' "Info"
-- @
underlineColored :: Element a => String -> Color -> a -> L
underlineColored char color element = L (Underlined (render element) char (Just color))

ul :: [L] -> L
ul = UL

ol :: [L] -> L
ol = OL

inlineBar :: String -> Double -> L
inlineBar label progress = L (InlineBar label progress)

statusCard :: String -> String -> L
statusCard label content = LStatusCard label content BorderNormal

layout :: [L] -> L
layout elements = L (Layout elements)

row :: [L] -> L
row elements = L (Row elements False)

-- | Create horizontal row with no spacing between elements (for gradients, etc.)
tightRow :: [L] -> L
tightRow elements = L (Row elements True)

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

-- | Banner: a titleless bordered box (Double border by default)
--
-- @
-- 'banner' ["System Dashboard"]
-- -- ╔══════════════════╗
-- -- ║ System Dashboard ║
-- -- ╚══════════════════╝
-- @
banner :: [L] -> L
banner content = LBox "" content BorderDouble

-- | Create margin with custom prefix
--
-- @
-- 'margin' "[error]" ['text' "Something went wrong"]
-- 'margin' "[info]" ['text' "FYI: Check the logs"]
-- @
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

-- | Place elements side by side (2 spaces between columns)
--
-- @
-- 'columns' ['layout' ["A", "B"], 'layout' ["C", "D"]]
-- -- A  C
-- -- B  D
-- @
columns :: [L] -> L
columns elements = L (Columns elements 2)

-- | Like 'columns' with a custom gap between columns
columns' :: Int -> [L] -> L
columns' spacing elements = L (Columns elements spacing)

-- | Truncate an element with a trailing @"..."@ when it is too wide
--
-- @
-- 'truncate'' 15 "Very long text that will be cut off"  -- Very long te...
-- @
truncate' :: Element a => Int -> a -> L
truncate' maxWidth element = L (Truncated (render element) maxWidth "...")

-- | Like 'truncate'' with a custom ellipsis
truncate'' :: Element a => Int -> String -> a -> L
truncate'' maxWidth ellipsis element = L (Truncated (render element) maxWidth ellipsis)

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
-- Elements that support borders: 'box', 'statusCard', 'table'.
-- Other elements are returned unchanged.
--
-- @
-- withBorder BorderDouble $ table [\"Name\"] [[text \"Alice\"]]
-- @
withBorder :: Border -> L -> L
withBorder = setBorder

-- | Apply a color to an element
--
-- @
-- withColor ColorBrightYellow $ box \"Warning\" [text "Check logs"]
-- @
withColor :: Color -> L -> L
withColor = Colored

-- | Apply a style to an element
--
-- @
-- withStyle StyleBold $ text "Important!"
-- @
withStyle :: Style -> L -> L
withStyle = Styled

-- | Create tree structure
tree :: String -> [Tree] -> L
tree name children = L (Tree name children)

-- | Create leaf tree node (no children)
leaf :: String -> Tree
leaf name = Tree name []

-- | Create branch tree node with children
branch :: String -> [Tree] -> Tree
branch name children = Tree name children

-- ============================================================================
-- Spinner Animations
-- ============================================================================

-- | Spinner style with animation frames
data SpinnerStyle
  = SpinnerDots
  | SpinnerLine
  | SpinnerClock
  | SpinnerBounce
  | SpinnerEarth
  | SpinnerMoon
  | SpinnerGrow
  | SpinnerArrow
  deriving (Show, Eq)

-- | Get animation frames for a spinner style
spinnerFrames :: SpinnerStyle -> [String]
spinnerFrames SpinnerDots   = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
spinnerFrames SpinnerLine   = ["|", "/", "-", "\\"]
spinnerFrames SpinnerClock  = ["🕐", "🕑", "🕒", "🕓", "🕔", "🕕", "🕖", "🕗", "🕘", "🕙", "🕚", "🕛"]
spinnerFrames SpinnerBounce = ["⠁", "⠂", "⠄", "⠂"]
spinnerFrames SpinnerEarth  = ["🌍", "🌎", "🌏"]
spinnerFrames SpinnerMoon   = ["🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘"]
spinnerFrames SpinnerGrow   = ["▏", "▎", "▍", "▌", "▋", "▊", "▉", "█", "▉", "▊", "▋", "▌", "▍", "▎"]
spinnerFrames SpinnerArrow  = ["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]

-- | Spinner animation element
data Spinner = Spinner String Int SpinnerStyle  -- label, frame, style

instance Element Spinner where
  renderElement (Spinner label frame style) =
    let frames = spinnerFrames style
        spinChar = frames !! (frame `mod` length frames)
    in if null label
       then spinChar
       else spinChar <> " " <> label

-- | Create an animated spinner
--
-- Example usage:
--
-- @
-- 'spinner' \"Loading\" 5 'SpinnerDots'   -- Shows the 5th frame of dots spinner
-- 'spinner' \"Processing\" 0 'SpinnerLine'  -- Shows first frame with label
-- @
--
-- Increment the frame number each render to animate:
--
-- @
-- 'layout' ['spinner' \"Working\" (tickCount \`mod\` 10) 'SpinnerDots']
-- @
spinner :: String -> Int -> SpinnerStyle -> L
spinner label frame style = L (Spinner label frame style)

-- ============================================================================
-- Visualization Primitives
-- ============================================================================

-- | Block characters for sparklines and bar charts (indices 0–8)
blockChars :: String
blockChars = " ▁▂▃▄▅▆▇█"

-- | Braille dot bit flag for position (row 0–3, col 0–1) in a 2×4 braille cell
brailleDot :: Int -> Int -> Int
brailleDot 0 0 = 0x01
brailleDot 1 0 = 0x02
brailleDot 2 0 = 0x04
brailleDot 3 0 = 0x40
brailleDot 0 1 = 0x08
brailleDot 1 1 = 0x10
brailleDot 2 1 = 0x20
brailleDot 3 1 = 0x80
brailleDot _ _ = 0

-- | Default color palette for cycling through series/slices
defaultPalette :: [Color]
defaultPalette = [ ColorBrightCyan, ColorBrightMagenta, ColorBrightYellow
                 , ColorBrightGreen, ColorBrightRed, ColorBrightBlue ]

-- | Use explicit color if not ColorDefault, else cycle palette by index
pickColor :: Int -> Color -> Color
pickColor idx ColorDefault = defaultPalette !! (idx `mod` length defaultPalette)
pickColor _   c            = c

-- | Format number for axis labels: "3" for integers, "3.5" for decimals
formatAxisNum :: Double -> String
formatAxisNum v
  | v == fromInteger (round v) = show (round v :: Integer)
  | otherwise                  = printf "%.1f" v

-- | ANSI 256-color background escape sequence
bgColor256 :: Int -> String
bgColor256 n = "\ESC[48;5;" <> show n <> "m"

-- | ANSI reset sequence
ansiReset :: String
ansiReset = "\ESC[0m"

-- Sparkline --------------------------------------------------

data SparklineData = SparklineData [Double]

instance Element SparklineData where
  renderElement (SparklineData []) = ""
  renderElement (SparklineData vals) =
    let mn  = minimum vals
        mx  = maximum vals
        rng = mx - mn
        idx v | rng == 0  = 4
              | otherwise = max 1 $ min 8 $ floor ((v - mn) / rng * 8)
    in map (\v -> blockChars !! idx v) vals

-- | Create a sparkline from a list of values
plotSparkline :: [Double] -> L
plotSparkline = L . SparklineData

-- Line Plot (Braille) --------------------------------------------------

-- | A data series for line plots: points, label, color
data Series = Series [(Double, Double)] String Color

data PlotData = PlotData [Series] Int Int  -- series, width, height

instance Element PlotData where
  renderElement (PlotData ss w h)
    | null ss || all (\(Series ps _ _) -> null ps) ss = "No data"
    | otherwise =
      let allPts = concatMap (\(Series ps _ _) -> ps) ss
          (xs, ys) = unzip allPts
          xMin = minimum xs; xMax = maximum xs
          yMin = minimum ys; yMax = maximum ys
          xRng = if xMax == xMin then 1.0 else xMax - xMin
          yRng = if yMax == yMin then 1.0 else yMax - yMin
          pxW  = w * 2; pxH = h * 4

          toPixel (x, y) =
            ( clampI 0 (pxW - 1) $ round ((x - xMin) / xRng * fromIntegral (pxW - 1))
            , clampI 0 (pxH - 1) $ round ((yMax - y) / yRng * fromIntegral (pxH - 1))
            )
          clampI lo hi = max lo . min hi

          emptyGrid = replicate h (replicate w (0 :: Int, -1 :: Int))

          plotSeries grd sIdx (Series ps _ _) =
            foldl (\g p ->
              let (px, py) = toPixel p
                  cx = px `div` 2; cy = py `div` 4
                  dx = px `mod` 2; dy = py `mod` 4
                  bit = brailleDot dy dx
              in updGrid cy cx (\(b, si) -> (b .|. bit, if si < 0 then sIdx else si)) g
            ) grd ps

          updGrid r c f g =
            [ if ri == r
              then [ if ci == c then f cell else cell | (ci, cell) <- zip [0..] row' ]
              else row'
            | (ri, row') <- zip [0..] g ]

          grid = foldl (\g (i, s) -> plotSeries g i s) emptyGrid (zip [0..] ss)

          yTicks  = [ yMax - yRng * fromIntegral i / fromIntegral (max 1 (h - 1)) | i <- [0 .. h-1] ]
          yLabels = map formatAxisNum yTicks
          yLabelW = maximum (0 : map length yLabels)

          gridLines = zipWith (\yLbl row' ->
            padLeft yLabelW yLbl <> " │" <>
            concatMap (\(bits, si) ->
              let ch = if bits == 0 then ' ' else chr (0x2800 + bits)
                  c  = if si >= 0 then pickColor si (sColor (ss !! si)) else ColorDefault
              in if c == ColorDefault then [ch] else wrapAnsi c [ch]
            ) row'
            ) yLabels grid

          xAxis   = replicate (yLabelW + 2) ' ' <> replicate w '─'
          xMinL   = formatAxisNum xMin
          xMaxL   = formatAxisNum xMax
          xLabels = replicate (yLabelW + 2) ' ' <> xMinL
                    <> replicate (max 1 (w - length xMinL - length xMaxL)) ' '
                    <> xMaxL

          legend
            | length ss <= 1 = []
            | otherwise      = ["", intercalate "  " $
                zipWith (\i (Series _ nm cl) ->
                  wrapAnsi (pickColor i cl) "●" <> " " <> nm
                ) [0..] ss]

      in intercalate "\n" (gridLines <> [xAxis, xLabels] <> legend)
    where sColor (Series _ _ c) = c

-- | Create a braille line plot
plotLine :: Int -> Int -> [Series] -> L
plotLine w h ss = L (PlotData ss w h)

-- Pie Chart (Braille) --------------------------------------------------

-- | A slice of a pie chart: value, label, color
data Slice = Slice Double String Color

data PieData = PieData [Slice] Int Int

instance Element PieData where
  renderElement (PieData [] _ _) = "No data"
  renderElement (PieData slices w h) =
    let total   = sum [ v | Slice v _ _ <- slices ]
        cumAngs = scanl (+) 0 [ v / total * 2 * pi | Slice v _ _ <- slices ]

        cxF    = fromIntegral w                   :: Double
        cyF    = fromIntegral (h * 4) / 2.0       :: Double
        radius = min cxF (cyF * 0.9)

        findSlice ang = go 0 (tail cumAngs)
          where go i []      = max 0 (i - 1)
                go i (a:as') = if ang < a then i else go (i + 1) as'

        renderCell gcx gcy =
          let subPx = [ (dy, dx, dist, nAng)
                      | dy <- [0..3], dx <- [0..1]
                      , let dpx  = fromIntegral (gcx * 2 + dx) :: Double
                            dpy  = fromIntegral (gcy * 4 + dy) :: Double
                            relX = dpx - cxF
                            relY = (dpy - cyF) * 2.0
                            dist = sqrt (relX * relX + relY * relY)
                            ang  = atan2 relY relX
                            nAng = if ang < 0 then ang + 2 * pi else ang
                      ]
              inside = [ (dy, dx, nAng) | (dy, dx, dist, nAng) <- subPx, dist <= radius ]
              bits   = foldl (\acc (dy, dx, _) -> acc .|. brailleDot dy dx) 0 inside
              domSi  = case inside of
                         []            -> -1
                         ((_, _, a):_) -> findSlice a
              ch     = if bits == 0 then ' ' else chr (0x2800 + bits)
              color  = if domSi >= 0 && domSi < length slices
                       then pickColor domSi (slColor (slices !! domSi))
                       else ColorDefault
          in if bits == 0 then " " else wrapAnsi color [ch]

        gridLines = [ concatMap (\gcx -> renderCell gcx gcy) [0..w-1] | gcy <- [0..h-1] ]

        legendLines = zipWith (\i (Slice v nm cl) ->
          let c   = pickColor i cl
              pct = printf "%.0f" (v / total * 100) :: String
          in "  " <> wrapAnsi c "●" <> " " <> nm <> " (" <> pct <> "%)"
          ) [0..] slices

    in intercalate "\n" (gridLines <> [""] <> legendLines)
    where slColor (Slice _ _ c) = c

-- | Create a braille pie chart
plotPie :: Int -> Int -> [Slice] -> L
plotPie w h sl = L (PieData sl w h)

-- Bar Chart (Vertical) --------------------------------------------------------

-- | A bar item: value, label, color
data BarItem = BarItem Double String Color

data BarChartData = BarChartData [BarItem] Int Int

instance Element BarChartData where
  renderElement (BarChartData [] _ _) = "No data"
  renderElement (BarChartData items w h) =
    let maxVal   = maximum [ v | BarItem v _ _ <- items ]
        nBars    = length items
        barW     = max 1 $ (w - nBars + 1) `div` nBars
        totalSub = h * 8
        barHts   = [ round (v / maxVal * fromIntegral totalSub) :: Int | BarItem v _ _ <- items ]

        yTicks  = [ maxVal * fromIntegral (h - 1 - i) / fromIntegral (max 1 (h - 1)) | i <- [0..h-1] ]
        yLabels = map formatAxisNum yTicks
        yLabelW = maximum (0 : map length yLabels)

        gridLines =
          [ let r = h - 1 - rowIdx
                barCells = intercalate " " $
                  map (\(i, (bh, BarItem _ _ cl)) ->
                    let filled = min 8 $ max 0 (bh - r * 8)
                        color' = pickColor i cl
                        barStr = replicate barW (blockChars !! filled)
                    in if filled > 0 then wrapAnsi color' barStr else barStr
                  ) (zip [0..] (zip barHts items))
            in padLeft yLabelW (yLabels !! rowIdx) <> " │" <> barCells
          | rowIdx <- [0..h-1]
          ]

        xAxisW    = nBars * barW + nBars - 1
        xAxis     = replicate (yLabelW + 2) ' ' <> replicate xAxisW '─'
        barLabels = replicate (yLabelW + 2) ' ' <>
                    intercalate " " [ take barW (nm <> replicate barW ' ') | BarItem _ nm _ <- items ]

    in intercalate "\n" (gridLines <> [xAxis, barLabels])

-- | Create a vertical bar chart
plotBar :: Int -> Int -> [BarItem] -> L
plotBar w h items = L (BarChartData items w h)

-- Stacked Bar Chart -------------------------------------------------------

-- | A group of stacked bars: segments and group label
data StackedBarGroup = StackedBarGroup [BarItem] String

data StackedBarChartData = StackedBarChartData [StackedBarGroup] Int Int

instance Element StackedBarChartData where
  renderElement (StackedBarChartData [] _ _) = "No data"
  renderElement (StackedBarChartData groups w h) =
    let maxTotal  = maximum [ sum [ v | BarItem v _ _ <- segs ] | StackedBarGroup segs _ <- groups ]
        nGroups   = length groups
        barW      = max 1 $ (w - nGroups + 1) `div` nGroups
        totalSub  = h * 8

        groupBounds = map (\(StackedBarGroup segs _) ->
          let vals    = [ v | BarItem v _ _ <- segs ]
              subHts  = map (\v -> round (v / maxTotal * fromIntegral totalSub) :: Int) vals
              cumHts  = scanl (+) 0 subHts
              bottoms = init cumHts
              tops    = tail cumHts
          in zip segs (zip bottoms tops)
          ) groups

        allLabels = nub [ nm | StackedBarGroup segs _ <- groups, BarItem _ nm _ <- segs ]
        labelIdx nm = case lookup nm (zip allLabels [0..]) of
          Just i  -> i
          Nothing -> 0

        yTicks  = [ maxTotal * fromIntegral (h - 1 - i) / fromIntegral (max 1 (h - 1)) | i <- [0..h-1] ]
        yLabels = map formatAxisNum yTicks
        yLabelW = maximum (0 : map length yLabels)

        gridLines =
          [ let r = h - 1 - rowIdx
                subBot = r * 8
                subTop = r * 8 + 8
                barCells = intercalate " " $
                  map (\bounds ->
                    let overlapping = [ (bi, bot, top)
                                      | (bi, (bot, top)) <- bounds
                                      , top > subBot, bot < subTop ]
                        topSeg = case overlapping of
                          [] -> Nothing
                          _  -> Just $ foldl1 (\a@(_, _, t1) b@(_, _, t2) ->
                                  if t2 > t1 then b else a) overlapping
                    in case topSeg of
                      Nothing -> replicate barW ' '
                      Just (BarItem _ nm cl, _, top) ->
                        let filled = min 8 $ max 0 (top - subBot)
                            color' = case cl of
                              ColorDefault -> pickColor (labelIdx nm) ColorDefault
                              _            -> cl
                            barStr = replicate barW (blockChars !! filled)
                        in if filled > 0 then wrapAnsi color' barStr else barStr
                  ) groupBounds
            in padLeft yLabelW (yLabels !! rowIdx) <> " │" <> barCells
          | rowIdx <- [0..h-1]
          ]

        xAxisW    = nGroups * barW + nGroups - 1
        xAxis     = replicate (yLabelW + 2) ' ' <> replicate xAxisW '─'
        grpLabels = replicate (yLabelW + 2) ' ' <>
                    intercalate " " [ take barW (nm <> replicate barW ' ')
                                    | StackedBarGroup _ nm <- groups ]

        legendItems = map (\nm ->
          let i = labelIdx nm
              c = defaultPalette !! (i `mod` length defaultPalette)
          in wrapAnsi c "█" <> " " <> nm
          ) allLabels
        legendLine
          | length allLabels <= 1 = []
          | otherwise             = ["", intercalate "  " legendItems]

    in intercalate "\n" (gridLines <> [xAxis, grpLabels] <> legendLine)

-- | Create a stacked vertical bar chart
plotStackedBar :: Int -> Int -> [StackedBarGroup] -> L
plotStackedBar w h groups = L (StackedBarChartData groups w h)

-- Heatmap -----------------------------------------------------------

-- | Heatmap data: grid of values, row labels, column labels
data HeatmapData = HeatmapData [[Double]] [String] [String]

data HeatmapElement = HeatmapElement HeatmapData Int  -- data, cellWidth

instance Element HeatmapElement where
  renderElement (HeatmapElement (HeatmapData [] _ _) _) = "No data"
  renderElement (HeatmapElement (HeatmapData grid rowLbls colLbls) cellW) =
    let allVals = concat grid
        mn  = minimum allVals
        mx  = maximum allVals
        rng = if mx == mn then 1.0 else mx - mn
        normalize v = (v - mn) / rng

        toColor256 :: Double -> Int
        toColor256 t
          | t <= 0.0  = 21
          | t >= 1.0  = 196
          | t < 0.25  = let s = t / 0.25      in round (21.0  + s * 30.0)
          | t < 0.5   = let s = (t-0.25)/0.25 in round (51.0  + s * (-5.0))
          | t < 0.75  = let s = (t-0.5)/0.25  in round (46.0  + s * 180.0)
          | otherwise  = let s = (t-0.75)/0.25 in round (226.0 + s * (-30.0))

        rowLblW = maximum (0 : map length rowLbls)

        header = replicate (rowLblW + 1) ' ' <>
                 intercalate " " (map (\l -> padRight cellW (take cellW l)) colLbls)

        dataRows = zipWith (\lbl rowVals ->
          padRight rowLblW (take rowLblW lbl) <> " " <>
          intercalate " " (map (\v ->
            let n    = normalize v
                c256 = toColor256 n
                vs   = formatAxisNum v
            in bgColor256 c256 <> padRight cellW (take cellW vs) <> ansiReset
          ) rowVals)
          ) rowLbls grid

        legendCs   = map (\i -> toColor256 (fromIntegral i / 10.0)) [0..10 :: Int]
        legendBar  = concatMap (\c -> bgColor256 c <> " " <> ansiReset) legendCs
        legendLine = replicate (rowLblW + 1) ' ' <>
                     formatAxisNum mn <> " " <> legendBar <> " " <> formatAxisNum mx

    in intercalate "\n" ([header] <> dataRows <> ["", legendLine])

-- | Create a heatmap with default cell width (6)
plotHeatmap :: HeatmapData -> L
plotHeatmap dat = L (HeatmapElement dat 6)

-- | Create a heatmap with custom cell width
plotHeatmap' :: Int -> HeatmapData -> L
plotHeatmap' cellW dat = L (HeatmapElement dat cellW)

-- ============================================================================
-- TUI Runtime - Simple event loop for interactive terminal applications
-- ============================================================================

-- | Keyboard input representation
data Key
  = KeyChar Char           -- ^ Regular character keys: 'a', '1', ' ', etc.
  | KeyCtrl Char           -- ^ Ctrl+key: KeyCtrl 'C', KeyCtrl 'Q', etc.
  | KeyEnter               -- ^ Enter/Return key
  | KeyBackspace           -- ^ Backspace key
  | KeyTab                 -- ^ Tab key
  | KeyEscape              -- ^ Escape key
  | KeyDelete              -- ^ Delete key
  | KeyUp                  -- ^ Up arrow
  | KeyDown                -- ^ Down arrow
  | KeyLeft                -- ^ Left arrow
  | KeyRight               -- ^ Right arrow
  | KeyPageUp              -- ^ Page Up
  | KeyPageDown            -- ^ Page Down
  | KeyHome                -- ^ Home
  | KeyEnd                 -- ^ End
  | KeySpecial String      -- ^ Other unrecognized escape sequences
  deriving (Show, Eq)

-- | Commands - side effects the runtime will execute after each update
data Cmd msg
  = CmdNone                     -- ^ No effect
  | CmdRun (IO (Maybe msg))    -- ^ Run IO, optionally produce a message
  | CmdBatch [Cmd msg]         -- ^ Combine multiple commands
  | CmdExit                    -- ^ Gracefully quit the application

-- | Create a command from an IO action (fire and forget)
cmdFire :: IO () -> Cmd msg
cmdFire io = CmdRun (io >> pure Nothing)

-- | Create a command that produces a message after IO completes
cmdTask :: IO msg -> Cmd msg
cmdTask io = CmdRun (Just <$> io)

-- | Create a command that fires a message after a delay
cmdAfterMs :: Int -> msg -> Cmd msg
cmdAfterMs delayMs msg = CmdRun (threadDelay (delayMs * 1000) >> pure (Just msg))

-- | Execute a command and return any resulting message.
-- Returns 'Nothing' for 'CmdExit' — the exit signal is handled by the runtime.
executeCmd :: Cmd msg -> IO (Maybe msg)
executeCmd CmdNone = pure Nothing
executeCmd CmdExit = pure Nothing
executeCmd (CmdRun io) = io
executeCmd (CmdBatch cmds) = do
  results <- mapM executeCmd cmds
  pure $ foldr pickFirst Nothing results
  where
    pickFirst (Just m) _ = Just m
    pickFirst Nothing  r = r

-- | Check whether a command (or any sub-command in a batch) is 'CmdExit'.
cmdIsExit :: Cmd msg -> Bool
cmdIsExit CmdExit = True
cmdIsExit (CmdBatch cmds) = any cmdIsExit cmds
cmdIsExit _ = False

-- | Subscriptions - event sources your app listens to
data Sub msg
  = SubNone                                    -- ^ No subscriptions
  | SubKeyPress (Key -> Maybe msg)             -- ^ Subscribe to keyboard input
  | SubEveryMs Int msg                         -- ^ Subscribe to periodic ticks (interval in ms + message)
  | SubBatch [Sub msg]                         -- ^ Combine multiple subscriptions

-- | Combine multiple subscriptions
subBatch :: [Sub msg] -> Sub msg
subBatch = SubBatch

-- | Subscribe to keyboard events
subKeyPress :: (Key -> Maybe msg) -> Sub msg
subKeyPress = SubKeyPress

-- | Subscribe to periodic ticks with custom interval (milliseconds)
subEveryMs :: Int -> msg -> Sub msg
subEveryMs = SubEveryMs

-- | The core application structure - Elm Architecture style
--
-- Build interactive TUI apps by defining:
--
-- * Initial state and startup commands
-- * How to update state based on messages
-- * What events to subscribe to
-- * How to render state to UI
--
-- Example:
--
-- @
-- data CounterMsg = Inc | Dec
--
-- counterApp :: 'LayoutzApp' Int CounterMsg
-- counterApp = 'LayoutzApp'
--   { 'appInit' = (0, 'CmdNone')
--   , 'appUpdate' = \\msg count -> case msg of
--       'Inc' -> (count + 1, 'CmdNone')
--       'Dec' -> (count - 1, 'CmdNone')
--   , 'appSubscriptions' = \\_ -> 'subKeyPress' $ \\key -> case key of
--       'KeyChar' \'+\' -> Just 'Inc'
--       'KeyChar' \'-\' -> Just 'Dec'
--       _           -> Nothing
--   , 'appView' = \\count -> 'layout' ['text' $ "Count: " <> show count]
--   }
-- @
data LayoutzApp state msg = LayoutzApp
  { appInit          :: (state, Cmd msg)                 -- ^ Initial state and startup commands
  , appUpdate        :: msg -> state -> (state, Cmd msg) -- ^ Update state with message, return new state and commands
  , appSubscriptions :: state -> Sub msg                 -- ^ Declare event subscriptions based on current state
  , appView          :: state -> L                       -- ^ Render state to UI
  }

-- | App-level alignment within the terminal window
data AppAlignment = AppAlignLeft | AppAlignCenter | AppAlignRight
  deriving (Show, Eq)

-- | Options for running a 'LayoutzApp'. Use 'defaultAppOptions' and override
-- the fields you care about:
--
-- @
-- 'runAppWith' 'defaultAppOptions' { 'optAlignment' = 'AppAlignCenter' } myApp
-- @
data AppOptions = AppOptions
  { optAlignment    :: AppAlignment -- ^ Alignment of the app block in the terminal (default: 'AppAlignLeft')
  , optClearOnStart :: Bool         -- ^ Enter alt screen and clear on start (default: 'True')
  , optClearOnExit  :: Bool         -- ^ Exit alt screen on quit (default: 'True')
  } deriving (Show, Eq)

-- | Sensible defaults: left-aligned, full-screen (alt screen on start\/exit).
defaultAppOptions :: AppOptions
defaultAppOptions = AppOptions
  { optAlignment    = AppAlignLeft
  , optClearOnStart = True
  , optClearOnExit  = True
  }

-- | Get terminal width via ANSI cursor position report (zero dependencies).
-- Moves cursor to far bottom-right, queries position, restores cursor.
-- Falls back to 80 columns on timeout or parse failure.
getTerminalWidth :: IO Int
getTerminalWidth = do
  -- Save cursor, move to 999;999, query position, restore cursor
  putStr "\ESC7\ESC[999;999H\ESC[6n\ESC8"
  hFlush stdout
  -- Terminal responds with: ESC [ rows ; cols R
  result <- timeout 100000 readCPR   -- 100ms timeout
  pure $ maybe 80 id result
  where
    readCPR = do
      c <- getChar
      if c == '\ESC' then do
        c2 <- getChar
        if c2 == '[' then parseResponse ""
        else pure 80
      else pure 80
    parseResponse acc = do
      c <- getChar
      if c == 'R' then
        let resp = reverse acc
        in pure $ case break (== ';') resp of
             (_, ';':cols) -> case reads cols of
               [(n, "")] -> n
               _         -> 80
             _ -> 80
      else parseResponse (c : acc)

-- | Get terminal size as @(rows, cols)@ via an ANSI cursor position report.
-- Falls back to @(24, 80)@ on timeout or parse failure. Must be called while
-- raw mode is active and nothing else is reading stdin.
getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  putStr "\ESC7\ESC[999;999H\ESC[6n\ESC8"
  hFlush stdout
  result <- timeout 100000 (readCPR "")
  pure $ maybe (24, 80) id result
  where
    readCPR acc = do
      c <- getChar
      case c of
        '\ESC' -> getChar >> readCPR acc   -- skip ESC and the following '['
        '['    -> readCPR acc
        'R'    -> pure (parseRC (reverse acc))
        _      -> readCPR (c : acc)
    parseRC resp = case break (== ';') resp of
      (rows, ';':cols) -> (readOr 24 rows, readOr 80 cols)
      _                -> (24, 80)
    readOr d s = case reads s of
      [(n, "")] -> n
      _         -> d

-- | Run an interactive TUI application with default options.
--
-- This function:
--
-- * Sets up raw terminal mode (no echo, no buffering)
-- * Clears screen and hides cursor
-- * Enters event loop that:
--
--     * Listens to subscribed events (keyboard, ticks, etc.)
--     * Dispatches messages to update function
--     * Updates state and re-renders
--
-- * Restores terminal on exit (ESC, Ctrl+C, or Ctrl+D)
--
-- Press ESC, Ctrl+C, or Ctrl+D to quit the application.
runApp :: LayoutzApp state msg -> IO ()
runApp app = runAppWithFinal defaultAppOptions app >> pure ()

-- | Like 'runApp', but returns the final application state after exit.
-- Useful for interactive tweaking of state followed by further processing.
runAppFinal :: LayoutzApp state msg -> IO state
runAppFinal = runAppWithFinal defaultAppOptions

-- | Run an app inline — no alt screen, no clearing. The app renders in-place
-- below whatever is already on screen and returns when it issues 'CmdExit'.
-- Useful for embedding animated progress bars or spinners in scripts.
runInline :: LayoutzApp state msg -> IO ()
runInline app = runAppWithFinal defaultAppOptions
  { optClearOnStart = False, optClearOnExit = False } app >> pure ()

-- | Run an interactive TUI application with custom options.
--
-- @
-- 'runAppWith' 'defaultAppOptions' { 'optAlignment' = 'AppAlignCenter' } myApp
-- @
runAppWith :: AppOptions -> LayoutzApp state msg -> IO ()
runAppWith opts app = runAppWithFinal opts app >> pure ()

-- | Internal runtime event. Every source (keypresses, interval ticks, command
-- results) is funnelled into a single channel as one of these, so there is
-- exactly one consumer applying 'appUpdate' with no races on state
data AppEvent msg
  = AppMsg msg
  | AppExit

-- | Like 'runAppWith', but returns the final application state after exit.
-- Useful for interactive tweaking of state followed by further processing.
--
-- @
-- finalState <- 'runAppWithFinal' 'defaultAppOptions' { 'optAlignment' = 'AppAlignCenter' } myApp
-- @
runAppWithFinal :: AppOptions -> LayoutzApp state msg -> IO state
runAppWithFinal opts LayoutzApp{..} = do
  oldBuffering <- hGetBuffering stdin
  oldEcho <- hGetEcho stdin

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  when (optClearOnStart opts) $ do
    enterAltScreen
    clearScreen
  hideCursor

  -- Query terminal width once, after raw mode is set, before threads
  termWidth <- getTerminalWidth

  let (initialState, initialCmd) = appInit

  stateRef  <- newIORef initialState
  eventChan <- newChan  -- Single event stream: keys, ticks, cmd results, exit
  cmdChan   <- newChan  -- Commands awaiting async execution

  let applyMsg msg = do
        cmdToRun <- atomicModifyIORef' stateRef $ \s ->
          let (newState, c) = appUpdate msg s in (newState, c)
        case cmdToRun of
          CmdNone -> pure ()
          CmdExit -> writeChan eventChan AppExit
          _       -> writeChan cmdChan cmdToRun

  cmdThread <- forkIO $ forever $ do
    cmdToRun <- readChan cmdChan
    when (cmdIsExit cmdToRun) $ writeChan eventChan AppExit
    maybeMsg <- executeCmd cmdToRun
    case maybeMsg of
      Just msg -> writeChan eventChan (AppMsg msg)
      Nothing  -> pure ()

  case initialCmd of
    CmdNone -> pure ()
    _       -> writeChan cmdChan initialCmd

  lastLineCount <- newIORef 0
  lastRendered <- newIORef ""
  sentKittyIds <- newIORef IntSet.empty  -- kitty images already transmitted this session

  renderThread <- forkIO $ forever $ do
    state <- readIORef stateRef
    let rendered = render (appView state)
    lastRender <- readIORef lastRendered

    when (rendered /= lastRender) $ do
      prevLineCount <- readIORef lastLineCount
      -- Drop transmit blocks for images already sent, so each distinct kitty
      -- image is uploaded only once across frames.
      priorIds <- readIORef sentKittyIds
      let (deduped, newKittyIds) = dedupeTransmits priorIds rendered
      writeIORef sentKittyIds newKittyIds
      let renderedLines = lines deduped
          currentLineCount = length renderedLines
          maxLineWidth = maximum (0 : map visibleLength renderedLines)
          blockPad = case optAlignment opts of
            AppAlignLeft   -> 0
            AppAlignCenter -> max 0 ((termWidth - maxLineWidth) `div` 2)
            AppAlignRight  -> max 0 (termWidth - maxLineWidth)
          padding = replicate blockPad ' '
          alignedLines = if blockPad > 0
                         then map (padding <>) renderedLines
                         else renderedLines
          moveUp = if optClearOnStart opts
                     then "\ESC[H"
                     else if prevLineCount > 0
                       then "\ESC[" <> show prevLineCount <> "A\r"
                       else ""
          output = moveUp <> concatMap (\l -> l <> "\ESC[K\n") alignedLines
                   <> concat (replicate (max 0 (prevLineCount - currentLineCount)) "\ESC[K\n")
      putStr output
      hFlush stdout
      writeIORef lastRendered rendered
      writeIORef lastLineCount currentLineCount

    threadDelay 33333

  let getKeyHandler sub = case sub of
        SubKeyPress handler -> Just handler
        SubBatch subs -> case [h | SubKeyPress h <- subs] of
          (h:_) -> Just h
          [] -> Nothing
        _ -> Nothing

      getTicks sub = case sub of
        SubEveryMs interval msg -> [(interval, msg)]
        SubBatch subs           -> concatMap getTicks subs
        _                       -> []

  tickThread <- forkIO $
    let tickLoop deadlines = do
          state <- readIORef stateRef
          now   <- getMonotonicTimeNSec
          let ticks     = getTicks (appSubscriptions state)
              nsOf ms   = fromIntegral ms * 1000 * 1000
              intervals = foldr (\i acc -> if i `elem` acc then acc else i : acc)
                                [] (map fst ticks)
              armed     = [ (i, maybe (now + nsOf i) id (lookup i deadlines))
                          | i <- intervals ]
              dueNow    = [ i | (i, d) <- armed, d <= now ]
              fired     = [ (i, msg) | (i, msg) <- ticks, i `elem` dueNow ]
              deadlines' = [ (i, if i `elem` dueNow
                                   then let nd = d + nsOf i
                                        in if nd <= now then now + nsOf i else nd
                                   else d)
                           | (i, d) <- armed ]
          mapM_ (\(_, msg) -> writeChan eventChan (AppMsg msg)) fired
          threadDelay 5000  -- 5ms poll: bounds tick jitter without busy-waiting
          tickLoop deadlines'
    in tickLoop []

  -- Input thread: translate keypresses into events. Exit keys become AppExit;
  -- a closed stdin (EOF) is treated as a quit request, like Ctrl-D.
  inputThread <- forkIO $
    let readLoop = do
          key <- readKey
          case key of
            KeyEscape   -> writeChan eventChan AppExit
            KeyCtrl 'C' -> writeChan eventChan AppExit
            KeyCtrl 'D' -> writeChan eventChan AppExit
            _ -> do
              state <- readIORef stateRef
              case getKeyHandler (appSubscriptions state) of
                Just handler -> maybe (pure ()) (writeChan eventChan . AppMsg) (handler key)
                Nothing      -> pure ()
              readLoop
    in readLoop `catch` \e ->
         let _ = (e :: IOException) in writeChan eventChan AppExit

  let killThreads = killThread renderThread >> killThread cmdThread
                 >> killThread tickThread   >> killThread inputThread

      doCleanup = do
        killThreads
        showCursor
        when (optClearOnExit opts) exitAltScreen
        hFlush stdout
        hSetBuffering stdin oldBuffering
        hSetEcho stdin oldEcho

      dispatchLoop = do
        ev <- readChan eventChan
        case ev of
          AppExit    -> pure ()             -- stop looping; `finally` restores the tty
          AppMsg msg -> applyMsg msg >> dispatchLoop

      -- Ctrl-C quits gracefully; every other exit path (normal quit, an
      -- exception from update/view, or an async kill) still restores the
      -- terminal via `finally`, so the tty is never left in raw mode.
      onAsync :: AsyncException -> IO ()
      onAsync UserInterrupt = pure ()
      onAsync e             = throwIO e

  (dispatchLoop `catch` onAsync) `finally` doCleanup

  readIORef stateRef

-- | Clear the screen and move cursor to top-left
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Hide the terminal cursor
hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

-- | Show the terminal cursor
showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

-- | Enter alternate screen buffer (like vim/less use)
enterAltScreen :: IO ()
enterAltScreen = putStr "\ESC[?1049h"

-- | Exit alternate screen buffer
exitAltScreen :: IO ()
exitAltScreen = putStr "\ESC[?1049l"

-- | Read a single key from stdin and parse it
readKey :: IO Key
readKey = do
  c <- getChar
  case ord c of
    10  -> return KeyEnter         -- LF (Unix)
    13  -> return KeyEnter         -- CR (Windows/Mac)
    27  -> readEscapeSequence      -- ESC - might be arrow key
    9   -> return KeyTab
    127 -> return KeyBackspace     -- DEL (often used as backspace)
    8   -> return KeyBackspace     -- BS
    n | n >= 32 && n <= 126 -> return $ KeyChar (chr n)  -- Printable ASCII
      | n < 32 -> return $ KeyCtrl (chr (n + 64))  -- Ctrl+Key
      | otherwise -> return $ KeyChar (chr n)

-- | Read escape sequence for arrow keys and other special keys
-- Uses a small timeout to distinguish between ESC key and ESC sequences
readEscapeSequence :: IO Key
readEscapeSequence = do
  -- Try to read next character with 50ms timeout
  -- If timeout, it's just ESC key; if character arrives, it's an escape sequence
  maybeChar <- timeout 50000 getChar  -- 50000 microseconds = 50ms
  case maybeChar of
    Nothing -> return KeyEscape  -- Timeout - just ESC key pressed
    Just '[' -> do
      -- It's an escape sequence, read the command character
      c2 <- getChar
      case c2 of
        'A' -> return KeyUp
        'B' -> return KeyDown
        'C' -> return KeyRight
        'D' -> return KeyLeft
        'H' -> return KeyHome
        'F' -> return KeyEnd
        -- Numeric sequences terminated by '~': ESC [ n ~
        n | n >= '1' && n <= '6' -> do
          c3 <- getChar
          if c3 == '~'
            then return $ case n of
                   '1' -> KeyHome
                   '3' -> KeyDelete
                   '4' -> KeyEnd
                   '5' -> KeyPageUp
                   '6' -> KeyPageDown
                   _   -> KeyEscape
            else return KeyEscape
        _ -> return KeyEscape
    Just 'O' -> do
      -- Application cursor mode: ESC O H/F for Home/End
      c2 <- getChar
      case c2 of
        'H' -> return KeyHome
        'F' -> return KeyEnd
        _ -> return KeyEscape
    Just _ -> return KeyEscape  -- Some other character after ESC


-- ============================================================================
-- Shared helpers (small string/ANSI utilities used by kitty, loader and Ask)
-- ============================================================================

-- | Dim a string with the SGR faint style.
dimText :: String -> String
dimText = wrapStyle StyleDim

-- | Colour a string, leaving it untouched for 'ColorDefault'.
paintColor :: Color -> String -> String
paintColor = wrapAnsi

-- | Split on a single character, keeping empty trailing segments (like
-- Scala's @split(sep, -1)@ — unlike 'lines', a trailing separator yields a
-- final empty segment).
splitOnChar :: Char -> String -> [String]
splitOnChar c s = case break (== c) s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitOnChar c rest

-- | Split a string at the first occurrence of a substring pattern, returning
-- @(before, fromPattern)@ where @fromPattern@ begins with the pattern.
splitOnFirst :: String -> String -> Maybe (String, String)
splitOnFirst pat = go ""
  where
    go acc s
      | pat `isPrefixOf` s = Just (reverse acc, s)
      | otherwise = case s of
          []     -> Nothing
          (x:xs) -> go (x:acc) xs

-- | Break a list into chunks of at most @n@ elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = [xs]
  | null xs   = []
  | otherwise = let (a, b) = splitAt n xs in a : chunksOf n b

-- | ANSI background-colour SGR code corresponding to a foreground 'Color'.
bgColorCode :: Color -> String
bgColorCode ColorDefault      = ""
bgColorCode (ColorFull n)     = "48;5;" <> show (clamp n)
bgColorCode (ColorTrue r g b) = "48;2;" <> show (clamp r) <> ";" <> show (clamp g) <> ";" <> show (clamp b)
bgColorCode c = case reads (colorCode c) :: [(Int, String)] of
  [(n, "")] -> show (n + 10)   -- 30-37 -> 40-47, 90-97 -> 100-107
  _         -> ""

-- ============================================================================
-- Kitty graphics protocol
-- ============================================================================

-- | Kitty transmit format: PNG.
formatPng :: Int
formatPng = 100

-- | Kitty transmit format: raw RGB (3 bytes/pixel).
formatRgb :: Int
formatRgb = 24

-- | Kitty transmit format: raw RGBA (4 bytes/pixel).
formatRgba :: Int
formatRgba = 32

-- | An inline image rendered via the kitty graphics protocol using Unicode
-- placeholders, so it composes like any other element: it measures exactly
-- @cols@ wide and @rows@ tall and drops into 'box', 'row', 'table', 'center', …
--
-- Prefer the smart constructors 'kittyImage', 'kittyRGB', 'kittyRGBA' (or the
-- 'kittyImageFile' loader) over building this record directly.
data KittyImage = KittyImage
  { kiPayload    :: [Word8]    -- ^ Raw image bytes (PNG, or raw RGB\/RGBA pixels)
  , kiFormat     :: Int        -- ^ 100 = PNG, 24 = RGB, 32 = RGBA
  , kiCols       :: Int        -- ^ Cell width
  , kiRows       :: Int        -- ^ Cell height
  , kiPxW        :: Int        -- ^ Source pixel width  (raw formats only)
  , kiPxH        :: Int        -- ^ Source pixel height (raw formats only)
  , kiAlt        :: String     -- ^ Alt text (kept for parity; unused by terminals)
  , kiIdOverride :: Maybe Int  -- ^ Explicit image id (otherwise a content hash)
  }

instance Element KittyImage where
  renderElement img = kittyTransmit img <> kittyPlacement img
  width  = kiCols
  height = kiRows

-- | Deterministic 24-bit image id: an explicit override, or an FNV-1a hash of
-- the payload masked to 24 bits (forced non-zero). Stable across redraws so a
-- runtime can transmit each distinct image only once.
kittyImageId :: [Word8] -> Maybe Int -> Int
kittyImageId payload override =
  let raw    = maybe (foldl step 0x811c9dc5 payload) id override
      masked = raw .&. 0xffffff
  in if masked == 0 then 1 else masked
  where
    step h b = (h `xor` fromIntegral b) * 0x01000193 .&. 0xffffffff

-- | Standard base64 (RFC 4648, @+/@ alphabet, @=@ padding). Shipped inline to
-- keep the library dependency-light.
kittyBase64 :: [Word8] -> String
kittyBase64 = go
  where
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    idx i = alphabet !! i
    go [] = ""
    go (b0:b1:b2:rest) =
      let t = (fromIntegral b0 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2
      in idx ((t `shiftR` 18) .&. 0x3f) : idx ((t `shiftR` 12) .&. 0x3f)
         : idx ((t `shiftR` 6) .&. 0x3f) : idx (t .&. 0x3f) : go rest
    go [b0, b1] =
      let t = (fromIntegral b0 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 8) :: Int
      in idx ((t `shiftR` 18) .&. 0x3f) : idx ((t `shiftR` 12) .&. 0x3f)
         : idx ((t `shiftR` 6) .&. 0x3f) : "="
    go [b0] =
      let t = fromIntegral b0 `shiftL` 16 :: Int
      in idx ((t `shiftR` 18) .&. 0x3f) : idx ((t `shiftR` 12) .&. 0x3f) : "=="

kittyApcG :: String
kittyApcG = "\ESC_G"

kittyST :: String
kittyST = "\ESC\\"

-- | Transmit block: base64 the payload, chunk into <=4096-char escapes. Every
-- chunk carries @i=\<id\>@ so 'dedupeTransmits' can drop all chunks of an
-- already-sent image.
kittyTransmit :: KittyImage -> String
kittyTransmit img =
  let b64      = kittyBase64 (kiPayload img)
      chunks   = if null b64 then [""] else chunksOf 4096 b64
      theId    = kittyImageId (kiPayload img) (kiIdOverride img)
      dims     = if kiFormat img == formatPng then ""
                 else ",s=" <> show (kiPxW img) <> ",v=" <> show (kiPxH img)
      firstKeys = "a=T,U=1,i=" <> show theId <> ",f=" <> show (kiFormat img)
                  <> ",c=" <> show (kiCols img) <> ",r=" <> show (kiRows img) <> dims <> ",q=2"
      contKeys  = "i=" <> show theId <> ",q=2"
      lastIdx   = length chunks - 1
      block i chunk =
        let m    = if i == lastIdx then "0" else "1"
            keys = (if i == 0 then firstKeys else contKeys) <> ",m=" <> m
        in kittyApcG <> keys <> ";" <> chunk <> kittyST
  in concat (zipWith block [0 ..] chunks)

-- | Placement grid: one line per image row of @U+10EEEE@ placeholders; the
-- image id rides in the foreground colour and the (row, column) in two
-- combining diacritics on the first cell of each row.
kittyPlacement :: KittyImage -> String
kittyPlacement img =
  let theId = kittyImageId (kiPayload img) (kiIdOverride img)
      fg = "\ESC[38;2;" <> show ((theId `shiftR` 16) .&. 0xff) <> ";"
           <> show ((theId `shiftR` 8) .&. 0xff) <> ";" <> show (theId .&. 0xff) <> "m"
      reset       = "\ESC[0m"
      placeholder = [kittyPlaceholderChar]
      col0        = [chr (head rowColumnDiacritics)]
      rowLine r =
        let firstCell = placeholder <> [chr (rowColumnDiacritics !! r)] <> col0
        in fg <> firstCell <> concat (replicate (kiCols img - 1) placeholder) <> reset
  in intercalate "\n" (map rowLine [0 .. kiRows img - 1])

-- | Strip transmit blocks whose image id is already in the seen-set, adding
-- new ids as they are encountered. Returns the rewritten string and the
-- updated set. Placement grids are always left intact.
dedupeTransmits :: IntSet.IntSet -> String -> (String, IntSet.IntSet)
dedupeTransmits ids0 input = go ids0 input
  where
    go ids s = case splitOnFirst kittyApcG s of
      Nothing -> (s, ids)
      Just (before, rest) ->
        let (block, after) = takeBlock rest
            bid            = parseId block
            (keep, ids')
              | bid /= 0 && IntSet.member bid ids = ("", ids)
              | bid /= 0                          = (block, IntSet.insert bid ids)
              | otherwise                         = (block, ids)
            (restOut, idsF) = go ids' after
        in (before <> keep <> restOut, idsF)

    -- @s@ starts with the APC introducer; return (whole block incl. ST, rest).
    takeBlock s = case splitOnFirst kittyST (drop (length kittyApcG) s) of
      Nothing          -> (s, "")   -- unterminated: keep verbatim
      Just (mid, after) -> (kittyApcG <> mid <> kittyST, drop (length kittyST) after)

    parseId block =
      let body   = drop (length kittyApcG) block
          keys   = takeWhile (/= ';') body
          fields = splitOnChar ',' keys
      in case [ drop 2 f | f <- fields, "i=" `isPrefixOf` f ] of
           (v:_) -> let ds = takeWhile isDigit v in if null ds then 0 else read ds
           []    -> 0

-- | Validate dimensions/format and build a 'KittyImage'.
mkKitty :: [Word8] -> Int -> Int -> Int -> Int -> Int -> String -> KittyImage
mkKitty payload fmt cols rows pxW pxH alt
  | cols < 1 || rows < 1 = error "KittyImage cols/rows must be >= 1"
  | cols > maxCells || rows > maxCells =
      error ("KittyImage cols/rows must be <= " <> show maxCells <> " (the diacritics table size)")
  | not validFormat =
      error "raw RGB/RGBA images (format 24/32) require pxW > 0 and pxH > 0"
  | otherwise = KittyImage payload fmt cols rows pxW pxH alt Nothing
  where
    maxCells    = length rowColumnDiacritics
    validFormat = fmt == formatPng
                  || ((fmt == formatRgb || fmt == formatRgba) && pxW > 0 && pxH > 0)

-- | Inline PNG image sized to a @cols@x@rows@ cell footprint. The terminal
-- decodes the PNG and fits it into that box; layout math uses @cols@\/@rows@.
kittyImage :: [Word8] -> Int -> Int -> L
kittyImage bytes cols rows = L (mkKitty bytes formatPng cols rows 0 0 "")

-- | Inline raw RGB image (@format 24@): @pxW@x@pxH@ source pixels shown in a
-- @cols@x@rows@ cell footprint.
kittyRGB :: [Word8] -> Int -> Int -> Int -> Int -> L
kittyRGB pixels pxW pxH cols rows = L (mkKitty pixels formatRgb cols rows pxW pxH "")

-- | Inline raw RGBA image (@format 32@): like 'kittyRGB' with an alpha channel.
kittyRGBA :: [Word8] -> Int -> Int -> Int -> Int -> L
kittyRGBA pixels pxW pxH cols rows = L (mkKitty pixels formatRgba cols rows pxW pxH "")

-- | Load an image file and size it to a @cols@x@rows@ cell footprint. PNG bytes
-- are sent to the terminal as-is.
kittyImageFile :: FilePath -> Int -> Int -> IO L
kittyImageFile path cols rows = do
  bytes <- readFileBytes path
  pure (kittyImage bytes cols rows)

-- | Read a file strictly as a list of bytes (binary mode, no dependencies).
readFileBytes :: FilePath -> IO [Word8]
readFileBytes path = withBinaryFile path ReadMode $ \h -> do
  contents <- hGetContents h
  let n = length contents
  n `seq` pure (map (fromIntegral . ord) contents)

-- ============================================================================
-- Progress loaders
-- ============================================================================

-- | Visual preset for a 'loader'. Controls the bar glyphs and colour for
-- bounded loaders, and the spinner frames + colour for streaming loaders.
data LoaderStyle = LoaderStyle
  { lsFill    :: Char
  , lsEmpty   :: Char
  , lsOpen    :: String
  , lsClose   :: String
  , lsHead    :: String
  , lsSmooth  :: Bool
  , lsSpinner :: SpinnerStyle
  , lsColor   :: Color
  }

-- | Smooth gradient blocks @████▊░░░@ (default).
styleBlocks :: LoaderStyle
styleBlocks = LoaderStyle '█' '░' "" "" "" True SpinnerGrow ColorCyan

-- | Classic bracketed bar @[██████░░░░]@.
styleBar :: LoaderStyle
styleBar = LoaderStyle '█' '░' "[" "]" "" False SpinnerDots ColorGreen

-- | Retro ASCII with an arrow head @[====>    ]@.
styleAscii :: LoaderStyle
styleAscii = LoaderStyle '=' ' ' "[" "]" ">" False SpinnerLine ColorYellow

-- | Dotted @●●●●∙∙∙∙@.
styleDots :: LoaderStyle
styleDots = LoaderStyle '●' '∙' "" "" "" False SpinnerBounce ColorMagenta

-- | Heavy\/light rule @━━━━────@.
styleLine :: LoaderStyle
styleLine = LoaderStyle '━' '─' "" "" "" False SpinnerLine ColorBlue

-- | Segmented pipes @▰▰▰▱▱▱@.
stylePipes :: LoaderStyle
stylePipes = LoaderStyle '▰' '▱' "" "" "" False SpinnerArrow ColorBrightMagenta

-- | All built-in styles, in display order.
loaderStyles :: [LoaderStyle]
loaderStyles = [styleBlocks, styleBar, styleAscii, styleDots, styleLine, stylePipes]

-- | Fractional block glyphs for the smooth-fill boundary cell (1\/8 .. 8\/8).
blockEighths :: [String]
blockEighths = ["", "▏", "▎", "▍", "▌", "▋", "▊", "▉"]

-- | Render a progress bar of the given style filled to @progress@ (0..1) over
-- @w@ cells.
renderProgressBar :: LoaderStyle -> Double -> Int -> String
renderProgressBar style progress w =
  let p      = max 0.0 (min 1.0 progress)
      exact  = p * fromIntegral w
      filled = floor exact :: Int
  in if lsSmooth style
       then let eighths    = floor ((exact - fromIntegral filled) * 8) :: Int
                hasPartial = filled < w && eighths > 0
                partial    = if hasPartial then blockEighths !! eighths else ""
                used       = filled + (if hasPartial then 1 else 0)
                body       = paintColor (lsColor style) (replicate filled (lsFill style) <> partial)
            in lsOpen style <> body <> dimText (replicate (w - used) (lsEmpty style)) <> lsClose style
       else let showHead   = not (null (lsHead style)) && filled > 0 && filled < w
                hd         = if showHead then lsHead style else ""
                emptyCount = max 0 (w - filled - length hd)
                body       = paintColor (lsColor style) (replicate filled (lsFill style) <> hd)
            in lsOpen style <> body <> dimText (replicate emptyCount (lsEmpty style)) <> lsClose style

-- | Run an inline progress line: a background ticker redraws @renderLine@ every
-- 80ms while the main thread maps @action@ over @items@, bumping @counter@ per
-- item. On completion the ticker stops and @finalLine@ is drawn.
runLoader :: (Int -> IO String) -> IO String -> IORef Int -> [a] -> (a -> IO b) -> IO [b]
runLoader renderLine finalLine counter items action = do
  hideCursor
  hFlush stdout
  stopRef <- newIORef False
  ticker <- forkIO $
    let tick frame = do
          stop <- readIORef stopRef
          unless stop $ do
            line <- renderLine frame
            putStr ("\r" <> line <> "\ESC[K")
            hFlush stdout
            threadDelay 80000
            tick (frame + 1)
    in tick 0
  results <- mapM (\x -> do r <- action x; modifyIORef' counter (+ 1); pure r) items
  writeIORef stopRef True
  killThread ticker
  final <- finalLine
  putStr ("\r" <> final <> "\ESC[K\n")
  showCursor
  hFlush stdout
  pure results

boundedRender :: LoaderStyle -> String -> Int -> IORef Int -> Int -> IO String
boundedRender style label total counter _frame = do
  done <- readIORef counter
  let progress = if total > 0 then min 1.0 (fromIntegral done / fromIntegral total) else 1.0
      bar      = renderProgressBar style progress 20
      pct      = floor (progress * 100) :: Int
      prefix   = if null label then "" else label <> " "
  pure (prefix <> bar <> " " <> show done <> "/" <> show total <> " (" <> show pct <> "%)")

streamingRender :: LoaderStyle -> String -> IORef Int -> Int -> IO String
streamingRender style label counter frame = do
  done <- readIORef counter
  let frames = spinnerFrames (lsSpinner style)
      spin   = paintColor (lsColor style) (frames !! (frame `mod` length frames))
      prefix = if null label then "" else label <> " "
  pure (spin <> " " <> prefix <> "(" <> show done <> " processed)")

streamingFinal :: String -> IORef Int -> IO String
streamingFinal label counter = do
  done <- readIORef counter
  let prefix = if null label then "" else label <> " "
  pure (prefix <> show done <> " processed")

-- | Iterate a sized collection while streaming an inline progress bar (blocks
-- style). Returns the results of @action@ over each item.
--
-- @
-- _ <- 'loader' "Processing" [1..100] (\\_ -> threadDelay 20000)
-- @
loader :: String -> [a] -> (a -> IO b) -> IO [b]
loader = loaderStyled styleBlocks

-- | Like 'loader' with an explicit 'LoaderStyle' (e.g. 'styleAscii').
loaderStyled :: LoaderStyle -> String -> [a] -> (a -> IO b) -> IO [b]
loaderStyled style label items action = do
  let total = length items
  counter <- newIORef 0
  let rl = boundedRender style label total counter
  runLoader rl (rl 0) counter items action

-- | Iterate a collection of unknown \"size\", streaming a spinner + running
-- count (blocks style spinner).
loaderStream :: String -> [a] -> (a -> IO b) -> IO [b]
loaderStream = loaderStreamStyled styleBlocks

-- | Like 'loaderStream' with an explicit 'LoaderStyle'.
loaderStreamStyled :: LoaderStyle -> String -> [a] -> (a -> IO b) -> IO [b]
loaderStreamStyled style label items action = do
  counter <- newIORef 0
  runLoader (streamingRender style label counter) (streamingFinal label counter) counter items action

-- ============================================================================
-- Interactive prompts (Ask)
-- ============================================================================
--
-- One-shot prompts. Each takes over the tty briefly, returns a value, and
-- leaves a single committed line behind. 'Nothing' means the user cancelled
-- (Esc / Ctrl-C / Ctrl-D).

askCursorUp :: Int -> String
askCursorUp n = "\ESC[" <> show n <> "A\r"

askClearEol :: String
askClearEol = "\ESC[K"

askClearBelow :: String
askClearBelow = "\ESC[J"

-- | Put the tty into raw mode, hide the cursor, run the body, and restore the
-- terminal afterwards (even on exception).
withAskTty :: IO a -> IO a
withAskTty body = do
  oldBuffering <- hGetBuffering stdin
  oldEcho      <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  hideCursor
  body `finally` do
    showCursor
    hSetEcho stdin oldEcho
    hSetBuffering stdin oldBuffering
    hFlush stdout

-- | Redraw a multi-line frame in place, returning its line count.
askRepaint :: String -> Int -> IO Int
askRepaint frame prevLines = do
  let ls   = splitOnChar '\n' frame
      up   = if prevLines > 0 then askCursorUp prevLines else ""
      body = concatMap (\l -> l <> askClearEol <> "\n") ls
      belw = if prevLines > length ls then askClearBelow else ""
  putStr (up <> body <> belw)
  hFlush stdout
  pure (length ls)

-- | Replace the live frame with a single committed summary line.
askCommit :: String -> Int -> IO ()
askCommit summary prevLines = do
  let up   = if prevLines > 0 then askCursorUp prevLines else ""
      belw = if prevLines > 1 then askClearBelow else ""
  putStr (up <> summary <> askClearEol <> "\n" <> belw)
  hFlush stdout

-- | Wipe the live frame entirely (used on cancel).
askErase :: Int -> IO ()
askErase prevLines = when (prevLines > 0) $ do
  putStr (askCursorUp prevLines <> askClearBelow)
  hFlush stdout

-- | Render an active confirm button (white on a coloured background, bold).
askActiveBtn :: Color -> String -> String
askActiveBtn bg t = "\ESC[1;37;" <> bgColorCode bg <> "m" <> t <> "\ESC[0m"

renderInputFrame :: String -> String -> String -> String
renderInputFrame prompt value placeholder =
  let body = if null value && not (null placeholder) then dimText placeholder else value
  in prompt <> body <> paintColor ColorCyan "▌"

renderConfirmFrame :: String -> Bool -> String -> String -> String
renderConfirmFrame question yes affirmative negative =
  question <> "\n  " <> btn affirmative yes ColorGreen <> "  " <> btn negative (not yes) ColorRed
  where
    btn t active bg = let padded = "  " <> t <> "  "
                      in if active then askActiveBtn bg padded else padded

renderChooseFrame :: String -> [a] -> Int -> (a -> String) -> String
renderChooseFrame prompt items idx renderItem =
  let rows = [ if i == idx then paintColor ColorCyan ("› " <> renderItem item) else "  " <> renderItem item
             | (item, i) <- zip items [0 ..] ]
  in (if not (null prompt) then prompt <> "\n" else "") <> intercalate "\n" rows

renderChooseManyFrame :: String -> [a] -> Int -> [Int] -> Int -> (a -> String) -> String
renderChooseManyFrame prompt items idx selected limit renderItem =
  let rows = [ let mark = if i `elem` selected then paintColor ColorGreen "[x]" else dimText "[ ]"
                   line = mark <> " " <> renderItem item
               in if i == idx then paintColor ColorCyan "› " <> line else "  " <> line
             | (item, i) <- zip items [0 ..] ]
      header
        | null prompt = ""
        | otherwise =
            let tag | limit > 0            = " (" <> show (length selected) <> "/" <> show limit <> ")"
                    | not (null selected)  = " (" <> show (length selected) <> ")"
                    | otherwise            = ""
            in prompt <> dimText tag <> "\n"
  in header <> intercalate "\n" rows

renderWriteFrame :: String -> String -> String -> String -> String
renderWriteFrame prompt value placeholder hint =
  let header = if not (null prompt) then prompt <> "\n" else ""
      body   = if null value && not (null placeholder)
                 then dimText placeholder <> paintColor ColorCyan "▌"
                 else value <> paintColor ColorCyan "▌"
      footer = if not (null hint) then "\n" <> dimText hint else ""
  in header <> body <> footer

renderFilterFrame :: String -> String -> [a] -> Int -> Int -> (a -> String) -> String
renderFilterFrame prompt query matches idx viewHeight renderItem =
  let total   = length matches
      h       = max 0 (min viewHeight total)
      top     = if total <= h then 0 else max 0 (min (idx - h `div` 2) (total - h))
      visible = take h (drop top matches)
      rows    = [ let absIdx = top + i; label = renderItem item
                  in if absIdx == idx then paintColor ColorCyan ("› " <> label) else "  " <> label
                | (item, i) <- zip visible [0 ..] ]
      header  = prompt <> query <> paintColor ColorCyan "▌"
      list    = if null rows then dimText "  (no matches)" else intercalate "\n" rows
  in header <> "\n" <> list

-- | Fuzzy-match score: 'Nothing' if @query@ isn't a subsequence of @target@,
-- otherwise @Just gap@ where a smaller gap sum means a tighter match.
fuzzyScore :: String -> String -> Maybe Int
fuzzyScore query target
  | null query = Just 0
  | otherwise  = loop (map toLower query) (map toLower target) (-1) 0 0
  where
    loop [] _ _ gap _ = Just gap
    loop _ [] _ _ _   = Nothing
    loop (qc:qs) (sc:ss) lastMatch gap ti
      | qc == sc  = let gap' = if lastMatch >= 0 then gap + (ti - lastMatch - 1) else gap
                    in loop qs ss ti gap' (ti + 1)
      | otherwise = loop (qc:qs) ss lastMatch gap (ti + 1)

-- | Filter items to those matching @query@, best (tightest) matches first.
fuzzyMatches :: String -> [a] -> (a -> String) -> [a]
fuzzyMatches query items renderItem
  | null query = items
  | otherwise  = map fst $ sortOn snd
      [ (item, score) | item <- items, Just score <- [fuzzyScore query (renderItem item)] ]

-- | A directory entry for the file picker.
data FileEntry = FileEntry { feName :: String, feIsDir :: Bool }

feDisplay :: FileEntry -> String
feDisplay e = if feIsDir e then feName e <> "/" else feName e

renderFileFrame :: String -> [FileEntry] -> Int -> Int -> String
renderFileFrame path entries idx viewHeight =
  let total   = length entries
      h       = max 0 (min viewHeight total)
      top     = if total <= h then 0 else max 0 (min (idx - h `div` 2) (total - h))
      visible = take h (drop top entries)
      rows    = [ let absIdx = top + i
                      name   = if feIsDir e then paintColor ColorBlue (feDisplay e) else feDisplay e
                  in if absIdx == idx then paintColor ColorCyan "› " <> name else "  " <> name
                | (e, i) <- zip visible [0 ..] ]
  in dimText path <> "\n" <> intercalate "\n" rows

renderPagerFrame :: [String] -> Int -> Int -> Int -> Bool -> String
renderPagerFrame allLines top h viewWidth lineNumbers =
  let total    = length allLines
      visible  = take h (drop top allLines)
      numWidth = length (show total)
      gutter   = if lineNumbers then numWidth + 3 else 0
      maxLineW = max 20 (viewWidth - gutter)
      body     = intercalate "\n"
        [ let truncated = if length line > maxLineW then take (maxLineW - 1) line <> "…" else line
          in if lineNumbers
               then dimText (padNum numWidth (top + i + 1) <> " │ ") <> truncated
               else truncated
        | (line, i) <- zip visible [0 ..] ]
      viewEnd = min (top + h) total
      percent = if total == 0 then 100 else (viewEnd * 100) `div` total
      status  = dimText ("  L" <> show (top + 1) <> "–" <> show viewEnd <> " of " <> show total
                         <> "  " <> show percent <> "%   ↑↓ PgUp/PgDn Home/End  q to quit")
  in body <> "\n" <> status
  where
    padNum w n = let s = show n in replicate (max 0 (w - length s)) ' ' <> s

-- | Prompt for a single line of text. Returns 'Nothing' if cancelled.
askInput :: String       -- ^ prompt (e.g. @"› "@)
         -> String       -- ^ placeholder shown when empty
         -> String       -- ^ initial value
         -> IO (Maybe String)
askInput prompt placeholder initial = withAskTty $ do
  initLines <- askRepaint (renderInputFrame prompt initial placeholder) 0
  let loop value prevLines = do
        key <- readKey
        case key of
          KeyEnter    -> askCommit (prompt <> value) prevLines >> pure (Just value)
          KeyEscape   -> askErase prevLines >> pure Nothing
          KeyCtrl 'C' -> askErase prevLines >> pure Nothing
          KeyCtrl 'D' -> askErase prevLines >> pure Nothing
          KeyBackspace | not (null value) -> do
            let nv = init value
            n <- askRepaint (renderInputFrame prompt nv placeholder) prevLines
            loop nv n
          KeyChar c -> do
            let nv = value <> [c]
            n <- askRepaint (renderInputFrame prompt nv placeholder) prevLines
            loop nv n
          _ -> loop value prevLines
  loop initial initLines

-- | Yes\/No confirmation. On cancel returns the opposite of @def@ (matching the
-- Scala @Ask.confirm@).
askConfirm :: String   -- ^ question
           -> Bool     -- ^ default selection
           -> String   -- ^ affirmative label (e.g. @"Yes"@)
           -> String   -- ^ negative label    (e.g. @"No"@)
           -> IO Bool
askConfirm question def affirmative negative = do
  r <- withAskTty $ do
    initLines <- askRepaint (renderConfirmFrame question def affirmative negative) 0
    let picked yes prevLines = do
          let label = if yes then affirmative else negative
          askCommit (question <> " " <> paintColor ColorCyan label) prevLines
          pure (Just yes)
        loop yes prevLines = do
          let flipSel = do
                let ny = not yes
                n <- askRepaint (renderConfirmFrame question ny affirmative negative) prevLines
                loop ny n
          key <- readKey
          case key of
            KeyEnter -> picked yes prevLines
            KeyChar c | c `elem` ("yY" :: String) -> picked True prevLines
                      | c `elem` ("nN" :: String) -> picked False prevLines
            KeyLeft     -> flipSel
            KeyRight    -> flipSel
            KeyTab      -> flipSel
            KeyEscape   -> askErase prevLines >> pure (Just (not def))
            KeyCtrl 'C' -> askErase prevLines >> pure (Just (not def))
            KeyCtrl 'D' -> askErase prevLines >> pure (Just (not def))
            _           -> loop yes prevLines
    loop def initLines
  pure (maybe def id r)

-- | Single-choice menu. Returns 'Nothing' if cancelled or @items@ is empty.
askChoose :: String -> [a] -> (a -> String) -> IO (Maybe a)
askChoose prompt items renderItem
  | null items = pure Nothing
  | otherwise  = withAskTty $ do
      initLines <- askRepaint (renderChooseFrame prompt items 0 renderItem) 0
      let n = length items
          loop idx prevLines = do
            let move ni = do
                  pl <- askRepaint (renderChooseFrame prompt items ni renderItem) prevLines
                  loop ni pl
            key <- readKey
            case key of
              KeyEnter -> do
                let pick = items !! idx
                askCommit (prompt <> " " <> paintColor ColorCyan (renderItem pick)) prevLines
                pure (Just pick)
              KeyUp       -> move ((idx - 1 + n) `mod` n)
              KeyDown     -> move ((idx + 1) `mod` n)
              KeyTab      -> move ((idx + 1) `mod` n)
              KeyEscape   -> askErase prevLines >> pure Nothing
              KeyCtrl 'C' -> askErase prevLines >> pure Nothing
              KeyCtrl 'D' -> askErase prevLines >> pure Nothing
              _           -> loop idx prevLines
      loop 0 initLines

-- | Multi-choice menu. @limit@ of 0 means unlimited. Space\/Tab toggles the
-- current row; Enter commits. Returns 'Nothing' if cancelled or empty.
askChooseMany :: String -> [a] -> Int -> (a -> String) -> IO (Maybe [a])
askChooseMany prompt items limit renderItem
  | null items = pure Nothing
  | otherwise  = withAskTty $ do
      let n = length items
          toggle i sel
            | i `elem` sel                     = filter (/= i) sel
            | limit > 0 && length sel >= limit = sel
            | otherwise                        = sel <> [i]
          loop idx selected prevLines = do
            pl <- askRepaint (renderChooseManyFrame prompt items idx selected limit renderItem) prevLines
            key <- readKey
            case key of
              KeyEnter -> do
                let picks   = [ item | (item, i) <- zip items [0 ..], i `elem` selected ]
                    summary = if null picks
                                then prompt <> " " <> dimText "(nothing)"
                                else prompt <> " " <> paintColor ColorCyan (intercalate ", " (map renderItem picks))
                askCommit summary pl
                pure (Just picks)
              KeyUp       -> loop ((idx - 1 + n) `mod` n) selected pl
              KeyDown     -> loop ((idx + 1) `mod` n) selected pl
              KeyChar ' ' -> loop idx (toggle idx selected) pl
              KeyTab      -> loop idx (toggle idx selected) pl
              KeyEscape   -> askErase pl >> pure Nothing
              KeyCtrl 'C' -> askErase pl >> pure Nothing
              KeyCtrl 'D' -> askErase pl >> pure Nothing
              _           -> loop idx selected pl
      loop 0 [] 0

-- | Run @task@ while animating a spinner labelled @label@; print a ✓\/✗ line
-- when it finishes. Re-throws if the task threw.
askSpin :: String -> SpinnerStyle -> IO a -> IO a
askSpin label style task = do
  hideCursor
  hFlush stdout
  stopRef <- newIORef False
  ticker <- forkIO $
    let tick i = do
          stop <- readIORef stopRef
          unless stop $ do
            let frames = spinnerFrames style
                frame  = paintColor ColorCyan (frames !! (i `mod` length frames)) <> " " <> label
            putStr ("\r" <> frame <> askClearEol)
            hFlush stdout
            threadDelay 80000
            tick (i + 1)
    in tick 0
  outcome <- (Right <$> task) `catch` \e -> pure (Left (e :: SomeException))
  writeIORef stopRef True
  killThread ticker
  let (glyph, col) = case outcome of
        Right _ -> ("✓", ColorGreen)
        Left _  -> ("✗", ColorRed)
  putStr ("\r" <> paintColor col (glyph <> " " <> label) <> askClearEol <> "\n")
  showCursor
  hFlush stdout
  case outcome of
    Right v -> pure v
    Left e  -> throwIO e

-- | Multi-line text editor. Enter inserts a newline; Ctrl-D submits; Esc
-- cancels.
askWrite :: String   -- ^ prompt
         -> String   -- ^ placeholder
         -> String   -- ^ initial value
         -> String   -- ^ hint line
         -> IO (Maybe String)
askWrite prompt placeholder initial hint = withAskTty $ do
  let loop value prevLines = do
        pl <- askRepaint (renderWriteFrame prompt value placeholder hint) prevLines
        key <- readKey
        case key of
          KeyCtrl 'D' -> do
            let firstLine = case splitOnChar '\n' value of (x:_) -> x; [] -> ""
                summary   = if '\n' `elem` value then firstLine <> " …" else firstLine
            askCommit ((if not (null prompt) then prompt <> " " else "") <> paintColor ColorCyan summary) pl
            pure (Just value)
          KeyEscape                        -> askErase pl >> pure Nothing
          KeyCtrl 'C'                      -> askErase pl >> pure Nothing
          KeyEnter                         -> loop (value <> "\n") pl
          KeyChar c                        -> loop (value <> [c]) pl
          KeyBackspace | not (null value)  -> loop (init value) pl
          _                                -> loop value pl
  loop initial 0

-- | Fuzzy incremental filter over @items@. Type to narrow, arrows to move,
-- Enter to pick. Returns 'Nothing' if cancelled or empty.
askFilter :: String -> [a] -> Int -> (a -> String) -> IO (Maybe a)
askFilter prompt items viewHeight renderItem
  | null items = pure Nothing
  | otherwise  = withAskTty $ do
      let loop query idx prevLines = do
            let matches = fuzzyMatches query items renderItem
                safeIdx = if null matches then 0 else max 0 (min idx (length matches - 1))
                m       = length matches
            pl <- askRepaint (renderFilterFrame prompt query matches safeIdx viewHeight renderItem) prevLines
            key <- readKey
            case key of
              KeyEnter | not (null matches) -> do
                let pick = matches !! safeIdx
                askCommit (prompt <> query <> "  " <> paintColor ColorCyan (renderItem pick)) pl
                pure (Just pick)
              KeyUp   | not (null matches) -> loop query ((safeIdx - 1 + m) `mod` m) pl
              KeyDown | not (null matches) -> loop query ((safeIdx + 1) `mod` m) pl
              KeyTab  | not (null matches) -> loop query ((safeIdx + 1) `mod` m) pl
              KeyBackspace | not (null query) -> loop (init query) 0 pl
              KeyChar c   -> loop (query <> [c]) 0 pl
              KeyEscape   -> askErase pl >> pure Nothing
              KeyCtrl 'C' -> askErase pl >> pure Nothing
              KeyCtrl 'D' -> askErase pl >> pure Nothing
              _           -> loop query safeIdx pl
      loop "" 0 0

-- | Directory browser. Enter descends into a directory or picks a file; Right
-- descends; Left\/Backspace goes to the parent. Returns the chosen file path.
askFile :: String -> Int -> IO (Maybe String)
askFile start viewHeight = withAskTty (loop start 0 0)
  where
    loop dir idx prevLines = do
      canonical <- canonicalizePath dir
      res <- listEntries canonical
      case res of
        Left err -> askCommit (dimText err) prevLines >> pure Nothing
        Right entries -> do
          let count   = length entries
              safeIdx = max 0 (min idx (count - 1))
              target  = entries !! safeIdx
          pl <- askRepaint (renderFileFrame canonical entries safeIdx viewHeight) prevLines
          key <- readKey
          case key of
            KeyEnter -> do
              full <- canonicalizePath (canonical <> "/" <> feName target)
              if feIsDir target
                then loop full 0 pl
                else askCommit (paintColor ColorCyan full) pl >> pure (Just full)
            KeyRight ->
              if feIsDir target
                then do full <- canonicalizePath (canonical <> "/" <> feName target); loop full 0 pl
                else loop canonical safeIdx pl
            KeyUp        -> loop canonical ((safeIdx - 1 + count) `mod` count) pl
            KeyDown      -> loop canonical ((safeIdx + 1) `mod` count) pl
            KeyTab       -> loop canonical ((safeIdx + 1) `mod` count) pl
            KeyLeft      -> goParent canonical safeIdx pl
            KeyBackspace -> goParent canonical safeIdx pl
            KeyEscape    -> askErase pl >> pure Nothing
            KeyCtrl 'C'  -> askErase pl >> pure Nothing
            KeyCtrl 'D'  -> askErase pl >> pure Nothing
            _            -> loop canonical safeIdx pl
    goParent canonical safeIdx pl = do
      parent <- canonicalizePath (canonical <> "/..")
      if parent /= canonical then loop parent 0 pl else loop canonical safeIdx pl

-- | List a directory as sorted 'FileEntry's (dirs first), with @".."@ prepended.
listEntries :: String -> IO (Either String [FileEntry])
listEntries path = do
  exists <- doesPathExist path
  isDir  <- doesDirectoryExist path
  if not exists then pure (Left ("not found: " <> path))
  else if not isDir then pure (Left ("not a directory: " <> path))
  else do
    names <- listDirectory path `catch` \e -> let _ = (e :: IOException) in pure []
    entries <- forM names $ \nm -> do
      d <- doesDirectoryExist (path <> "/" <> nm)
      pure (FileEntry nm d)
    let sorted = sortOn (\e -> (not (feIsDir e), map toLower (feName e))) entries
    pure (Right (FileEntry ".." True : sorted))

-- | Scrollable pager. @viewHeight@ of 0 auto-sizes to the terminal.
-- Arrows\/PgUp\/PgDn\/Home\/End navigate; q or Esc quits.
askPager :: String -> Int -> Bool -> IO ()
askPager content viewHeight lineNumbers = do
  let ls = if null content then [""] else splitOnChar '\n' content
  _ <- withAskTty $ do
    (termH, termW) <- getTerminalSize
    let h      = if viewHeight > 0 then viewHeight else max 5 (termH - 3)
        total  = length ls
        maxTop = max 0 (total - h)
        loop top prevLines = do
          let safeTop = max 0 (min top maxTop)
          pl <- askRepaint (renderPagerFrame ls safeTop h termW lineNumbers) prevLines
          key <- readKey
          case key of
            KeyChar 'q' -> askErase pl
            KeyEscape   -> askErase pl
            KeyCtrl 'C' -> askErase pl
            KeyUp       -> loop (safeTop - 1) pl
            KeyDown     -> loop (safeTop + 1) pl
            KeyEnter    -> loop (safeTop + 1) pl
            KeyPageUp   -> loop (safeTop - h) pl
            KeyPageDown -> loop (safeTop + h) pl
            KeyChar ' ' -> loop (safeTop + h) pl
            KeyHome     -> loop 0 pl
            KeyEnd      -> loop maxTop pl
            _           -> loop safeTop pl
    loop 0 0
  pure ()

