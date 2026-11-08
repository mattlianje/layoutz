{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Layoutz
Description : Friendly, expressive print-layout DSL for Haskell
Copyright   : (c) 2026 Matthieu Court
License     : Apache-2.0

Compositional terminal layouts, charts, prompts and a small Elm-style TUI runtime.
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
  , layout, vstack
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

import Data.List (transpose, nub, sortOn)
import Data.Text (type Text, pattern (:<), pattern Empty)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.IntSet as IntSet
import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
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

--------------------------------------------------------------------------------
-- Text measurement

replicateChar :: Int -> Char -> Text
replicateChar n = T.replicate n . T.singleton

-- | Drop SGR sequences and kitty APC blocks (@ESC _ ... ESC \\@) so neither
-- counts towards visible width.
stripAnsi :: Text -> Text
stripAnsi t = case T.break (== '\ESC') t of
  (before, rest) -> case rest of
    '\ESC' :< '[' :< more -> before <> stripAnsi (dropAfterM more)
    '\ESC' :< '_' :< more -> before <> stripAnsi (dropAfterST more)
    '\ESC' :< more        -> before <> T.cons '\ESC' (stripAnsi more)
    _                     -> before
  where
    dropAfterM ('m' :< xs) = xs
    dropAfterM (_ :< xs) = dropAfterM xs
    dropAfterM Empty = ""
    dropAfterST ('\ESC' :< '\\' :< xs) = xs
    dropAfterST (_ :< xs) = dropAfterST xs
    dropAfterST Empty = ""

-- | Terminal cell width: 0 for combining marks, 2 for wide CJK / emoji.
charWidth :: Char -> Int
charWidth c
  | c < '\x0300' = 1
  | c == kittyPlaceholderChar = 1
  | isRowColumnDiacritic c = 0
  | c >= '\x0300' && c < '\x0370' = 0    -- combining diacriticals
  | c >= '\x1100' && c < '\x1200' = 2    -- Hangul Jamo
  | c >= '\x2E80' && c < '\x9FFF' = 2    -- CJK
  | c >= '\xAC00' && c < '\xD7A4' = 2    -- Hangul syllables
  | c >= '\xF900' && c < '\xFB00' = 2    -- CJK compatibility ideographs
  | c >= '\xFE10' && c < '\xFE20' = 2    -- vertical forms
  | c >= '\xFE30' && c < '\xFE70' = 2    -- CJK compatibility forms
  | c >= '\xFF00' && c < '\xFF61' = 2    -- fullwidth forms
  | c >= '\xFFE0' && c < '\xFFE7' = 2    -- fullwidth symbols
  | c >= '\x1F000' = 2                   -- emoji, supplementary planes
  | c >= '\x20000' && c < '\x2FFFF' = 2
  | c >= '\x30000' && c < '\x3FFFF' = 2
  | otherwise = 1

visibleLength :: Text -> Int
visibleLength = T.foldl' (\acc c -> acc + charWidth c) 0 . stripAnsi

-- | Kitty reserves one cell per placeholder glyph.
kittyPlaceholderChar :: Char
kittyPlaceholderChar = '\x10EEEE'

-- | Zero-width diacritics kitty uses to encode a placeholder's (row, column).
-- Its length caps the image size in cells.
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

rowColumnDiacriticsSet :: IntSet.IntSet
rowColumnDiacriticsSet = IntSet.fromList rowColumnDiacritics

isRowColumnDiacritic :: Char -> Bool
isRowColumnDiacritic c = IntSet.member (ord c) rowColumnDiacriticsSet

-- | Map over lines, keeping a trailing newline if there was one.
mapLines :: (Text -> Text) -> Text -> Text
mapLines f str
  | T.null str          = str
  | T.last str == '\n'  = T.unlines (map f (T.lines str))
  | otherwise           = T.intercalate "\n" (map f (T.lines str))

padRight :: Int -> Text -> Text
padRight w str = str <> T.replicate (max 0 (w - visibleLength str)) " "

padLeft :: Int -> Text -> Text
padLeft w str = T.replicate (max 0 (w - visibleLength str)) " " <> str

centerString :: Int -> Text -> Text
centerString w str
  | len >= w  = str
  | otherwise = leftPad <> str <> rightPad
  where
    len      = visibleLength str
    total    = w - len
    leftPad  = T.replicate (total `div` 2) " "
    rightPad = T.replicate (total - T.length leftPad) " "

-- | Spread words across @w@ columns, leftmost gaps taking the remainder.
justifyString :: Int -> Text -> Text
justifyString w str
  | T.length str >= w = str
  | length ws <= 1    = str
  | otherwise         = T.concat (zipWith (<>) ws spaces)
  where
    ws          = T.words str
    totalSpaces = w - sum (map T.length ws)
    gaps        = length ws - 1
    (base, extra) = totalSpaces `divMod` gaps
    spaces = replicate extra (T.replicate (base + 1) " ")
          <> replicate (gaps - extra) (T.replicate base " ")
          <> [""]

--------------------------------------------------------------------------------
-- Core

class Element a where
  renderElement :: a -> Text

  -- | Widest visible line.
  width :: a -> Int
  width element = case T.lines (renderElement element) of
    [] -> 0
    ls -> maximum (0 : map visibleLength ls)

  -- | Line count (an empty render still takes one line).
  height :: a -> Int
  height element =
    let rendered = renderElement element
    in if T.null rendered then 1 else length (T.lines rendered)

render :: Element a => a -> String
render = T.unpack . renderElement

-- | The universal layout node. Anything that is an 'Element' can be wrapped
-- with 'L', so boxes, tables, text and charts compose freely:
--
-- @
-- 'layout' ['text' "title", 'box' "content" [...], 'center' ('text' "footer")]
-- @
data L = forall a. Element a => L a
       | UL [L]
       | OL [L]
       | AutoCenter L
       | Colored Color L
       | Styled Style L
       | LBox Text [L] Border
       | LStatusCard Text Text Border
       | LTable [Text] [[L]] Border

instance Element L where
  renderElement (L x) = renderElement x
  renderElement (UL items) = renderElement (UnorderedList items)
  renderElement (OL items) = renderElement (OrderedList items)
  renderElement (AutoCenter element) = renderElement element  -- Layout does the centering
  renderElement (Colored color element) = mapLines (wrapAnsi color) (renderElement element)
  renderElement (Styled style element) = mapLines (wrapStyle style) (renderElement element)
  renderElement (LBox title elements border) = renderElement (Box title elements border)
  renderElement (LStatusCard label content border) = renderElement (StatusCard label content border)
  renderElement (LTable headers rows border) = renderElement (Table headers rows border)

  width (L x) = width x
  width (UL items) = width (UnorderedList items)
  width (OL items) = width (OrderedList items)
  width (AutoCenter element) = width element
  width (Colored _ element) = width element
  width (Styled _ element) = width element
  width (LBox title elements border) = width (Box title elements border)
  width (LStatusCard label content border) = width (StatusCard label content border)
  width (LTable headers rows border) = width (Table headers rows border)

  height (L x) = height x
  height (UL items) = height (UnorderedList items)
  height (OL items) = height (OrderedList items)
  height (AutoCenter element) = height element
  height (Colored _ element) = height element
  height (Styled _ element) = height element
  height (LBox title elements border) = height (Box title elements border)
  height (LStatusCard label content border) = height (StatusCard label content border)
  height (LTable headers rows border) = height (Table headers rows border)

instance Show L where
  show = render

-- | Lets you write @layout [\"Hello\", \"World\"]@ under @OverloadedStrings@.
instance IsString L where
  fromString = text

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
  | BorderCustom Text Text Text  -- ^ corner, horizontal, vertical
  | BorderNone
  deriving (Show, Eq)

class HasBorder a where
  setBorder :: Border -> a -> a

instance HasBorder L where
  setBorder border (LBox title elements _) = LBox title elements border
  setBorder border (LStatusCard label content _) = LStatusCard label content border
  setBorder border (LTable headers rows _) = LTable headers rows border
  setBorder border (Colored color element) = Colored color (setBorder border element)
  setBorder border (Styled style element) = Styled style (setBorder border element)
  setBorder _ other = other

data Color = ColorDefault | ColorBlack | ColorRed | ColorGreen | ColorYellow
           | ColorBlue | ColorMagenta | ColorCyan | ColorWhite
           | ColorBrightBlack | ColorBrightRed | ColorBrightGreen | ColorBrightYellow
           | ColorBrightBlue | ColorBrightMagenta | ColorBrightCyan | ColorBrightWhite
           | ColorFull Int           -- ^ 256-colour palette index
           | ColorTrue Int Int Int   -- ^ 24-bit RGB
  deriving (Show, Eq)

colorCode :: Color -> Text
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
colorCode (ColorFull n)      = "38;5;" <> T.show (clamp n)
colorCode (ColorTrue r g b)  = "38;2;" <> T.show (clamp r) <> ";" <> T.show (clamp g) <> ";" <> T.show (clamp b)

clamp :: Int -> Int
clamp = max 0 . min 255

-- | No-op for 'ColorDefault'.
wrapAnsi :: Color -> Text -> Text
wrapAnsi color str
  | T.null (colorCode color) = str
  | otherwise = "\ESC[" <> colorCode color <> "m" <> str <> "\ESC[0m"

data Style = StyleDefault | StyleBold | StyleDim | StyleItalic | StyleUnderline
           | StyleBlink | StyleReverse | StyleHidden | StyleStrikethrough
           | StyleCombined [Style]
  deriving (Show, Eq)

-- | Combine styles with @<>@, e.g. @StyleBold <> StyleItalic@.
instance Semigroup Style where
  StyleDefault <> other = other
  other <> StyleDefault = other
  StyleCombined a <> StyleCombined b = StyleCombined (a <> b)
  StyleCombined a <> s = StyleCombined (a <> [s])
  s <> StyleCombined b = StyleCombined (s : b)
  a <> b = StyleCombined [a, b]

instance Monoid Style where
  mempty = StyleDefault

styleCode :: Style -> Text
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
  T.intercalate ";" (filter (not . T.null) (map styleCode styles))

wrapStyle :: Style -> Text -> Text
wrapStyle style str
  | T.null (styleCode style) = str
  | otherwise = "\ESC[" <> styleCode style <> "m" <> str <> "\ESC[0m"

-- | Top/bottom and left/right glyphs are separate so half-block borders work.
data BorderChars = BorderChars
  { bcTL, bcTR, bcBL, bcBR         :: Text
  , bcHTop, bcHBottom              :: Text
  , bcVLeft, bcVRight              :: Text
  , bcLeftTee, bcRightTee, bcCross :: Text
  , bcTopTee, bcBottomTee          :: Text
  }

mkSymmetric :: Text -> Text -> Text -> Text -> Text -> Text
            -> Text -> Text -> Text -> Text -> Text -> BorderChars
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

--------------------------------------------------------------------------------
-- Elements

instance Element Text where renderElement = id

data LineBreak = LineBreak
instance Element LineBreak where renderElement _ = ""

newtype Layout = Layout [L]
instance Element Layout where
  renderElement (Layout elements) = T.intercalate "\n" (map renderIn elements)
    where
      -- AutoCenter children centre against the widest sibling
      sized    = [e | e <- elements, not (isAutoCenter e)]
      ctxWidth = if null sized then 80 else maximum (0 : map width sized)

      isAutoCenter (AutoCenter _) = True
      isAutoCenter _              = False

      renderIn (AutoCenter e) = renderElement (Centered (renderElement e) ctxWidth)
      renderIn e              = renderElement e

data Centered = Centered Text Int
instance Element Centered where
  renderElement (Centered content w) =
    T.intercalate "\n" $ map (centerString w) (T.lines content)

data Underlined = Underlined Text Text (Maybe Color)
instance Element Underlined where
  renderElement (Underlined content glyph maybeColor) =
    let contentLines = T.lines content
        w = if null contentLines then 0 else maximum (map visibleLength contentLines)
        (reps, rest) = w `divMod` T.length glyph
        rule = T.replicate reps glyph <> T.take rest glyph
    in content <> "\n" <> maybe rule (`wrapAnsi` rule) maybeColor

data Row = Row [L] Bool  -- tight?
instance Element Row where
  renderElement (Row elements tight)
    | null elements = ""
    | otherwise = T.intercalate "\n" $ map (T.intercalate sep) (transpose padded)
    where
      sep       = if tight then "" else " "
      cellLines = map (T.lines . renderElement) elements
      maxHeight = maximum (map length cellLines)
      widths    = map (maximum . map visibleLength) cellLines
      padded    = zipWith padCell widths cellLines
      padCell w ls = map (padRight w) (ls <> replicate (maxHeight - length ls) "")

data Alignment = AlignLeft | AlignRight | AlignCenter | Justify
  deriving (Show, Eq)

data AlignedText = AlignedText Text Int Alignment
instance Element AlignedText where
  renderElement (AlignedText content w alignment) =
    T.intercalate "\n" $ map align (T.lines content)
    where
      align = case alignment of
        AlignLeft   -> padRight w
        AlignRight  -> padLeft w
        AlignCenter -> centerString w
        Justify     -> justifyString w

data Box = Box Text [L] Border

instance HasBorder Box where
  setBorder border (Box title elements _) = Box title elements border

instance Element Box where
  renderElement (Box title elements border) =
    let content      = T.intercalate "\n" (map renderElement elements)
        contentLines = if T.null content then [""] else T.lines content
        contentWidth = maximum (0 : map visibleLength contentLines)
        titleWidth   = if T.null title then 0 else visibleLength title + 2
        innerWidth   = max contentWidth titleWidth
        totalWidth   = innerWidth + 4
        BorderChars{..} = borderChars border
        hTop    = T.take 1 bcHTop
        hBottom = T.take 1 bcHBottom

        topBorder
          | T.null title = bcTL <> T.replicate (totalWidth - 2) hTop <> bcTR
          | otherwise =
              let slack = totalWidth - visibleLength title - 2
                  l     = slack `div` 2
              in bcTL <> T.replicate l hTop <> title <> T.replicate (slack - l) hTop <> bcTR

        bottomBorder = bcBL <> T.replicate (totalWidth - 2) hBottom <> bcBR
        body = map (\line -> bcVLeft <> " " <> padRight innerWidth line <> " " <> bcVRight) contentLines

    in T.intercalate "\n" (topBorder : body <> [bottomBorder])

data StatusCard = StatusCard Text Text Border

instance HasBorder StatusCard where
  setBorder border (StatusCard label content _) = StatusCard label content border

instance Element StatusCard where
  renderElement (StatusCard label content border) =
    let allLines = T.lines label <> T.lines content
        inner    = maximum (0 : map visibleLength allLines) + 2
        BorderChars{..} = borderChars border
        hTop    = T.take 1 bcHTop
        hBottom = T.take 1 bcHBottom
        topBorder    = bcTL <> T.replicate (inner + 2) hTop <> bcTR
        bottomBorder = bcBL <> T.replicate (inner + 2) hBottom <> bcBR
        cardLine l   = bcVLeft <> " " <> padRight inner l <> " " <> bcVRight
    in T.intercalate "\n" $ [topBorder] <> map cardLine allLines <> [bottomBorder]

data Margin = Margin Text [L]
instance Element Margin where
  renderElement (Margin prefix elements) =
    let content = case elements of
          [single] -> renderElement single
          _        -> renderElement (Layout elements)
    in T.intercalate "\n" $ map ((prefix <> " ") <>) (T.lines content)

data HorizontalRule = HorizontalRule Text Int
instance Element HorizontalRule where
  renderElement (HorizontalRule glyph n) = T.replicate n glyph

data VerticalRule = VerticalRule Text Int
instance Element VerticalRule where
  renderElement (VerticalRule glyph n) = T.intercalate "\n" (replicate n glyph)

data Padded = Padded Text Int
instance Element Padded where
  renderElement (Padded content p) =
    let contentLines = T.lines content
        w      = maximum (0 : map T.length contentLines)
        side   = T.replicate p " "
        blank  = replicate p (T.replicate (w + p * 2) " ")
        body   = map (\line -> side <> padRight w line <> side) contentLines
    in T.intercalate "\n" (blank <> body <> blank)

data Columns = Columns [L] Int
instance Element Columns where
  renderElement (Columns elements spacing)
    | null elements = ""
    | otherwise = T.intercalate "\n" $ map (T.intercalate sep) (transpose padded)
    where
      sep       = T.replicate spacing " "
      cellLines = map (T.lines . renderElement) elements
      maxHeight = maximum (map length cellLines)
      widths    = map (maximum . (0 :) . map visibleLength) cellLines
      padded    = zipWith padCol widths cellLines
      padCol w ls = map (padRight w) (ls <> replicate (maxHeight - length ls) "")

data Truncated = Truncated Text Int Text  -- content, max width, ellipsis
instance Element Truncated where
  renderElement (Truncated content maxWidth ellipsis) =
    T.intercalate "\n" $ map cut (T.lines content)
    where
      cut line
        | visibleLength line <= maxWidth = line
        | keep <= 0                      = T.take maxWidth ellipsis
        | otherwise                      = T.take keep (stripAnsi line) <> ellipsis
        where keep = maxWidth - T.length ellipsis

newtype Chart = Chart [(Text, Double)]
instance Element Chart where
  renderElement (Chart points)
    | null points = "No data"
    | otherwise   = T.intercalate "\n" $ map bar points
    where
      maxValue   = maximum (0 : map snd points)
      labelWidth = min 15 (maximum (0 : map (T.length . fst) points))
      barWidth   = 40

      bar (label, value) =
        let label'
              | T.length label > labelWidth = T.take (labelWidth - 3) label <> "..."
              | otherwise                   = label
            filled = floor (value / maxValue * fromIntegral barWidth)
            body   = T.replicate filled "█" <> T.replicate (barWidth - filled) "─"
        in padRight labelWidth label' <> " │" <> body <> "│ " <> formatAxisNum value

data Table = Table [Text] [[L]] Border

instance HasBorder Table where
  setBorder border (Table headers rows _) = Table headers rows border

instance Element Table where
  renderElement (Table headers rows border) =
    let rows'  = map (fitRow (length headers)) rows
        widths = columnWidths rows'
        BorderChars{..} = borderChars border
        hTop    = T.take 1 bcHTop
        hBottom = T.take 1 bcHBottom

        rule l mid r h = l <> h <> T.intercalate (h <> mid <> h) [T.replicate w h | w <- widths] <> h <> r
        topBorder    = rule bcTL bcTopTee bcTR hTop
        separator    = rule bcLeftTee bcCross bcRightTee hTop
        bottomBorder = rule bcBL bcBottomTee bcBR hBottom

        line cells = bcVLeft <> " " <> T.intercalate (" " <> bcVLeft <> " ") cells <> " " <> bcVRight
        headerRow  = line (zipWith padRight widths headers)
        dataRows   = concatMap (map line . rowLines widths) rows'

    in T.intercalate "\n" ([topBorder, headerRow, separator] <> dataRows <> [bottomBorder])
    where
      fitRow n cells
        | length cells >= n = take n cells
        | otherwise         = cells <> replicate (n - length cells) (text "")

      columnWidths rs =
        let headerWidths = map visibleLength headers
            cellWidths   = map (map (maximum . (0:) . map visibleLength . T.lines . renderElement)) rs
        in map (maximum . (0:)) (transpose (headerWidths : cellWidths))

      -- A row may be several lines tall when a cell renders multi-line.
      rowLines widths cells =
        let cellLines = map (T.lines . renderElement) cells
            h         = maximum (1 : map length cellLines)
            padCell w ls = map (padRight w) (ls <> replicate (h - length ls) "")
        in transpose (zipWith padCell widths cellLines)

data Section = Section Text [L] Text Int  -- title, content, glyph, flank width
instance Element Section where
  renderElement (Section title content glyph n) =
    let flank = T.replicate n (T.take 1 glyph)
    in flank <> " " <> title <> " " <> flank <> "\n" <> renderElement (Layout content)

newtype KeyValue = KeyValue [(Text, Text)]
instance Element KeyValue where
  renderElement (KeyValue []) = ""
  renderElement (KeyValue pairs) = T.intercalate "\n" $ map pair pairs
    where
      col = maximum (map (visibleLength . fst) pairs) + 2
      pair (k, v) =
        let k' = k <> ":"
        in k' <> T.replicate (max 1 (col - visibleLength k')) " " <> v

data Tree = Tree Text [Tree]

instance Element Tree where
  renderElement = go True "" True
    where
      go isRoot prefix isLast (Tree name children) =
        let nodeLine
              | isRoot    = name
              | otherwise = prefix <> (if isLast then "└── " else "├── ") <> name
            childPrefix
              | isRoot    = ""
              | otherwise = prefix <> (if isLast then "    " else "│   ")
            n = length children
            childLines = zipWith (\i c -> go False childPrefix (i == n - 1) c) [0 :: Int ..] children
        in T.intercalate "\n" (nodeLine : childLines)

-- | Prefix the first line with @marker@ and hang the rest under it.
hangIndent :: Text -> Text -> Text -> Text
hangIndent indent marker content = case T.lines content of
  []     -> indent <> marker
  (l:ls) -> T.intercalate "\n" $ (indent <> marker <> l) : map (hang <>) ls
  where hang = indent <> T.replicate (T.length marker) " "

newtype UnorderedList = UnorderedList [L]
instance Element UnorderedList where
  renderElement (UnorderedList items) = go 0 items
    where
      bullets = ["•", "◦", "▪"]
      go :: Int -> [L] -> Text
      go level = T.intercalate "\n" . map item
        where
          bullet = bullets !! (level `mod` length bullets)
          indent = T.replicate (level * 2) " "
          item (UL nested) = go (level + 1) nested
          item e           = hangIndent indent (bullet <> " ") (renderElement e)

newtype OrderedList = OrderedList [L]
instance Element OrderedList where
  renderElement (OrderedList items) = go 0 items
    where
      go :: Int -> [L] -> Text
      go level = T.intercalate "\n" . zipWith item [1 ..]
        where
          indent = T.replicate (level * 2) " "
          item _ (OL nested) = go (level + 1) nested
          item n e           = hangIndent indent (marker n <> ". ") (renderElement e)

          marker n = case level `mod` 3 of
            0 -> T.show n
            1 -> T.singleton (toEnum (96 + n))
            _ -> roman n

      roman :: Int -> Text
      roman = \case
        1  -> "i";   2  -> "ii";  3  -> "iii"; 4  -> "iv"; 5  -> "v"
        6  -> "vi";  7  -> "vii"; 8  -> "viii"; 9  -> "ix"; 10 -> "x"
        n  -> T.show n

data InlineBar = InlineBar Text Double
instance Element InlineBar where
  renderElement (InlineBar label progress) =
    let p      = max 0.0 (min 1.0 progress)
        filled = floor (p * 20)
        bar    = T.replicate filled "█" <> T.replicate (20 - filled) "─"
    in label <> " [" <> bar <> "] " <> T.show (floor (p * 100) :: Int) <> "%"

--------------------------------------------------------------------------------
-- Constructors

text :: String -> L
text = L . T.pack

br :: L
br = L LineBreak

-- | Centre against the widest sibling in the enclosing 'layout'.
center :: Element a => a -> L
center element = AutoCenter (L element)

-- | Centre within a fixed width.
center' :: Element a => Int -> a -> L
center' w element = L (Centered (renderElement element) w)

underline :: Element a => a -> L
underline element = L (Underlined (renderElement element) "─" Nothing)

underline' :: Element a => String -> a -> L
underline' glyph element = L (Underlined (renderElement element) (T.pack glyph) Nothing)

-- |
-- @
-- 'underlineColored' "=" 'ColorRed' $ 'text' "Error Section"
-- @
underlineColored :: Element a => String -> Color -> a -> L
underlineColored glyph color element = L (Underlined (renderElement element) (T.pack glyph) (Just color))

ul :: [L] -> L
ul = UL

ol :: [L] -> L
ol = OL

inlineBar :: String -> Double -> L
inlineBar label progress = L (InlineBar (T.pack label) progress)

statusCard :: String -> String -> L
statusCard label content = LStatusCard (T.pack label) (T.pack content) BorderNormal

layout :: [L] -> L
layout = L . Layout

-- | Alias for 'layout'.
vstack :: [L] -> L
vstack = layout

row :: [L] -> L
row elements = L (Row elements False)

-- | 'row' without the single-space gutter.
tightRow :: [L] -> L
tightRow elements = L (Row elements True)

alignLeft :: Int -> String -> L
alignLeft w s = L (AlignedText (T.pack s) w AlignLeft)

alignRight :: Int -> String -> L
alignRight w s = L (AlignedText (T.pack s) w AlignRight)

alignCenter :: Int -> String -> L
alignCenter w s = L (AlignedText (T.pack s) w AlignCenter)

justify :: Int -> String -> L
justify w s = L (AlignedText (T.pack s) w Justify)

-- | Greedy word wrap. A word longer than @w@ gets a line to itself.
wrap :: Int -> String -> L
wrap w = layout . map L . wrapWords . T.words . T.pack
  where
    wrapWords :: [Text] -> [Text]
    wrapWords [] = []
    wrapWords ws = let (line, rest) = takeLine ws in line : wrapWords rest

    takeLine [] = ("", [])
    takeLine (x:xs)
      | T.length x > w = (x, xs)
      | otherwise      = go (T.length x) [x] xs

    go _ acc [] = (T.unwords (reverse acc), [])
    go len acc (y:ys)
      | len' <= w = go len' (y:acc) ys
      | otherwise = (T.unwords (reverse acc), y:ys)
      where len' = len + 1 + T.length y

box :: String -> [L] -> L
box title elements = LBox (T.pack title) elements BorderNormal

-- | Titleless box with a double border.
--
-- @
-- 'banner' ["System Dashboard"]
-- -- ╔══════════════════╗
-- -- ║ System Dashboard ║
-- -- ╚══════════════════╝
-- @
banner :: [L] -> L
banner content = LBox "" content BorderDouble

-- | Prefix every line, e.g. @'margin' "[error]" ['text' "boom"]@.
margin :: String -> [L] -> L
margin prefix elements = L (Margin (T.pack prefix) elements)

hr :: L
hr = L (HorizontalRule "─" 50)

hr' :: String -> L
hr' glyph = L (HorizontalRule (T.pack glyph) 50)

hr'' :: String -> Int -> L
hr'' glyph n = L (HorizontalRule (T.pack glyph) n)

vr :: L
vr = L (VerticalRule "│" 10)

vr' :: String -> L
vr' glyph = L (VerticalRule (T.pack glyph) 10)

vr'' :: String -> Int -> L
vr'' glyph n = L (VerticalRule (T.pack glyph) n)

pad :: Element a => Int -> a -> L
pad p element = L (Padded (renderElement element) p)

-- | Side by side with a two-space gap.
--
-- @
-- 'columns' ['layout' ["A", "B"], 'layout' ["C", "D"]]
-- -- A  C
-- -- B  D
-- @
columns :: [L] -> L
columns elements = L (Columns elements 2)

columns' :: Int -> [L] -> L
columns' spacing elements = L (Columns elements spacing)

-- |
-- @
-- 'truncate'' 15 "Very long text that will be cut off"  -- Very long te...
-- @
truncate' :: Element a => Int -> a -> L
truncate' maxWidth element = L (Truncated (renderElement element) maxWidth "...")

truncate'' :: Element a => Int -> String -> a -> L
truncate'' maxWidth ellipsis element = L (Truncated (renderElement element) maxWidth (T.pack ellipsis))

-- | Horizontal bar chart.
chart :: [(String, Double)] -> L
chart points = L (Chart [ (T.pack label, v) | (label, v) <- points ])

table :: [String] -> [[L]] -> L
table headers rows = LTable (map T.pack headers) rows BorderNormal

section :: String -> [L] -> L
section title content = L (Section (T.pack title) content "=" 3)

section' :: String -> String -> [L] -> L
section' glyph title content = L (Section (T.pack title) content (T.pack glyph) 3)

section'' :: String -> String -> Int -> [L] -> L
section'' glyph title n content = L (Section (T.pack title) content (T.pack glyph) n)

kv :: [(String, String)] -> L
kv pairs = L (KeyValue [ (T.pack k, T.pack v) | (k, v) <- pairs ])

-- | Only 'box', 'statusCard' and 'table' have borders; anything else passes
-- through untouched.
--
-- @
-- withBorder BorderDouble $ table [\"Name\"] [[text \"Alice\"]]
-- @
withBorder :: Border -> L -> L
withBorder = setBorder

withColor :: Color -> L -> L
withColor = Colored

withStyle :: Style -> L -> L
withStyle = Styled

tree :: String -> [Tree] -> L
tree name children = L (Tree (T.pack name) children)

leaf :: String -> Tree
leaf name = Tree (T.pack name) []

branch :: String -> [Tree] -> Tree
branch name = Tree (T.pack name)

--------------------------------------------------------------------------------
-- Spinners

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

spinnerFrames :: SpinnerStyle -> [Text]
spinnerFrames SpinnerDots   = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
spinnerFrames SpinnerLine   = ["|", "/", "-", "\\"]
spinnerFrames SpinnerClock  = ["🕐", "🕑", "🕒", "🕓", "🕔", "🕕", "🕖", "🕗", "🕘", "🕙", "🕚", "🕛"]
spinnerFrames SpinnerBounce = ["⠁", "⠂", "⠄", "⠂"]
spinnerFrames SpinnerEarth  = ["🌍", "🌎", "🌏"]
spinnerFrames SpinnerMoon   = ["🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘"]
spinnerFrames SpinnerGrow   = ["▏", "▎", "▍", "▌", "▋", "▊", "▉", "█", "▉", "▊", "▋", "▌", "▍", "▎"]
spinnerFrames SpinnerArrow  = ["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]

spinnerFrame :: SpinnerStyle -> Int -> Text
spinnerFrame style i = let fs = spinnerFrames style in fs !! (i `mod` length fs)

data Spinner = Spinner Text Int SpinnerStyle

instance Element Spinner where
  renderElement (Spinner label frame style)
    | T.null label = glyph
    | otherwise    = glyph <> " " <> label
    where glyph = spinnerFrame style frame

-- | One frame of a spinner; bump the frame each tick to animate.
--
-- @
-- 'layout' ['spinner' \"Working\" tick 'SpinnerDots']
-- @
spinner :: String -> Int -> SpinnerStyle -> L
spinner label frame style = L (Spinner (T.pack label) frame style)

--------------------------------------------------------------------------------
-- Plots

-- | Eighth-block ramp, index 0 (blank) to 8 (full).
blockChars :: Text
blockChars = " ▁▂▃▄▅▆▇█"

-- | Bit for dot (row, col) of a 2x4 braille cell.
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

defaultPalette :: [Color]
defaultPalette = [ ColorBrightCyan, ColorBrightMagenta, ColorBrightYellow
                 , ColorBrightGreen, ColorBrightRed, ColorBrightBlue ]

-- | Explicit colour wins; 'ColorDefault' cycles the palette by index.
pickColor :: Int -> Color -> Color
pickColor idx ColorDefault = defaultPalette !! (idx `mod` length defaultPalette)
pickColor _   c            = c

-- | @3@ for whole numbers, @3.5@ otherwise.
formatAxisNum :: Double -> Text
formatAxisNum v
  | v == fromInteger (round v) = T.show (round v :: Integer)
  | otherwise                  = T.pack (printf "%.1f" v)

bgColor256 :: Int -> Text
bgColor256 n = "\ESC[48;5;" <> T.show n <> "m"

ansiReset :: Text
ansiReset = "\ESC[0m"

newtype SparklineData = SparklineData [Double]

instance Element SparklineData where
  renderElement (SparklineData []) = ""
  renderElement (SparklineData vals) = T.pack (map (T.index blockChars . level) vals)
    where
      mn  = minimum vals
      rng = maximum vals - mn
      level v
        | rng == 0  = 4
        | otherwise = max 1 $ min 8 $ floor ((v - mn) / rng * 8)

plotSparkline :: [Double] -> L
plotSparkline = L . SparklineData

-- | Points, label, colour.
data Series = Series [(Double, Double)] String Color

data PlotData = PlotData [Series] Int Int

instance Element PlotData where
  renderElement (PlotData ss w h)
    | all (\(Series ps _ _) -> null ps) ss = "No data"
    | otherwise =
      let (xs, ys) = unzip (concatMap (\(Series ps _ _) -> ps) ss)
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

          -- Each cell holds its braille bits and the first series to touch it.
          emptyGrid = replicate h (replicate w (0 :: Int, -1 :: Int))

          plotSeries grd sIdx (Series ps _ _) = foldl plot grd ps
            where
              plot g p =
                let (px, py) = toPixel p
                    bit = brailleDot (py `mod` 4) (px `mod` 2)
                in updGrid (py `div` 4) (px `div` 2)
                     (\(b, si) -> (b .|. bit, if si < 0 then sIdx else si)) g

          updGrid r c f g =
            [ if ri == r then [ if ci == c then f x else x | (ci, x) <- zip [0..] row' ] else row'
            | (ri, row') <- zip [0 :: Int ..] g ]

          grid = foldl (\g (i, s) -> plotSeries g i s) emptyGrid (zip [0..] ss)

          yTicks  = [ yMax - yRng * fromIntegral i / fromIntegral (max 1 (h - 1)) | i <- [0 .. h-1] ]
          yLabels = map formatAxisNum yTicks
          yLabelW = maximum (0 : map T.length yLabels)

          cell (bits, si)
            | bits == 0 = " "
            | si < 0    = glyph
            | otherwise = wrapAnsi (pickColor si (sColor (ss !! si))) glyph
            where glyph = T.singleton (chr (0x2800 + bits))

          gridLines = zipWith (\lbl r -> padLeft yLabelW lbl <> " │" <> foldMap cell r) yLabels grid

          gutter  = T.replicate (yLabelW + 2) " "
          xMinL   = formatAxisNum xMin
          xMaxL   = formatAxisNum xMax
          xAxis   = gutter <> T.replicate w "─"
          xLabels = gutter <> xMinL
                    <> T.replicate (max 1 (w - T.length xMinL - T.length xMaxL)) " "
                    <> xMaxL

          legend
            | length ss <= 1 = []
            | otherwise      = ["", T.intercalate "  "
                [ wrapAnsi (pickColor i cl) "●" <> " " <> T.pack nm | (i, Series _ nm cl) <- zip [0..] ss ]]

      in T.intercalate "\n" (gridLines <> [xAxis, xLabels] <> legend)
    where sColor (Series _ _ c) = c

-- | Braille line plot, @w@x@h@ cells.
plotLine :: Int -> Int -> [Series] -> L
plotLine w h ss = L (PlotData ss w h)

-- | Value, label, colour.
data Slice = Slice Double String Color

data PieData = PieData [Slice] Int Int

instance Element PieData where
  renderElement (PieData [] _ _) = "No data"
  renderElement (PieData slices w h) =
    let total   = sum [ v | Slice v _ _ <- slices ]
        cumAngs = scanl (+) 0 [ v / total * 2 * pi | Slice v _ _ <- slices ]

        cxF    = fromIntegral w             :: Double
        cyF    = fromIntegral (h * 4) / 2.0 :: Double
        radius = min cxF (cyF * 0.9)

        sliceAt ang = go 0 (drop 1 cumAngs)
          where go i []     = max 0 (i - 1)
                go i (a:as) = if ang < a then i else go (i + 1) as

        -- Cells are twice as tall as wide, so y distances are doubled.
        renderCell gcx gcy =
          let inside = [ (dy, dx, if ang < 0 then ang + 2 * pi else ang)
                       | dy <- [0..3], dx <- [0..1]
                       , let relX = fromIntegral (gcx * 2 + dx) - cxF
                             relY = (fromIntegral (gcy * 4 + dy) - cyF) * 2.0
                             ang  = atan2 relY relX
                       , sqrt (relX * relX + relY * relY) <= radius ]
              bits  = foldl (\acc (dy, dx, _) -> acc .|. brailleDot dy dx) 0 inside
              si    = case inside of
                        []            -> -1
                        ((_, _, a):_) -> sliceAt a
              color = if si >= 0 && si < length slices
                        then pickColor si (slColor (slices !! si))
                        else ColorDefault
          in if bits == 0 then " " else wrapAnsi color (T.singleton (chr (0x2800 + bits)))

        gridLines = [ foldMap (`renderCell` gcy) [0..w-1] | gcy <- [0..h-1] ]

        legendLines =
          [ "  " <> wrapAnsi (pickColor i cl) "●" <> " " <> T.pack nm
              <> " (" <> T.pack (printf "%.0f" (v / total * 100)) <> "%)"
          | (i, Slice v nm cl) <- zip [0..] slices ]

    in T.intercalate "\n" (gridLines <> [""] <> legendLines)
    where slColor (Slice _ _ c) = c

-- | Braille pie chart, @w@x@h@ cells.
plotPie :: Int -> Int -> [Slice] -> L
plotPie w h sl = L (PieData sl w h)

-- | Value, label, colour.
data BarItem = BarItem Double String Color

data BarChartData = BarChartData [BarItem] Int Int

-- | Y-axis labels top to bottom, plus their width.
yAxisLabels :: Double -> Int -> ([Text], Int)
yAxisLabels top h =
  let ls = [ formatAxisNum (top * fromIntegral (h - 1 - i) / fromIntegral (max 1 (h - 1))) | i <- [0..h-1] ]
  in (ls, maximum (0 : map T.length ls))

-- | One bar's slice of a row, @filled@ eighths high.
barCell :: Int -> Color -> Int -> Text
barCell barW color filled
  | filled > 0 = wrapAnsi color glyphs
  | otherwise  = glyphs
  where glyphs = replicateChar barW (T.index blockChars filled)

instance Element BarChartData where
  renderElement (BarChartData [] _ _) = "No data"
  renderElement (BarChartData items w h) =
    let maxVal = maximum [ v | BarItem v _ _ <- items ]
        nBars  = length items
        barW   = max 1 $ (w - nBars + 1) `div` nBars
        barHts = [ round (v / maxVal * fromIntegral (h * 8)) :: Int | BarItem v _ _ <- items ]
        (yLabels, yLabelW) = yAxisLabels maxVal h

        gridLines =
          [ padLeft yLabelW yLab <> " │" <> T.intercalate " "
              [ barCell barW (pickColor i cl) (min 8 $ max 0 (bh - r * 8))
              | (i, bh, BarItem _ _ cl) <- zip3 [0..] barHts items ]
          | (r, yLab) <- zip [h - 1, h - 2 ..] yLabels ]

        gutter    = T.replicate (yLabelW + 2) " "
        xAxis     = gutter <> T.replicate (nBars * barW + nBars - 1) "─"
        barLabels = gutter <> T.intercalate " "
                      [ T.take barW (T.pack nm <> T.replicate barW " ") | BarItem _ nm _ <- items ]

    in T.intercalate "\n" (gridLines <> [xAxis, barLabels])

-- | Vertical bar chart, @w@x@h@ cells.
plotBar :: Int -> Int -> [BarItem] -> L
plotBar w h items = L (BarChartData items w h)

-- | Segments bottom to top, and the group label.
data StackedBarGroup = StackedBarGroup [BarItem] String

data StackedBarChartData = StackedBarChartData [StackedBarGroup] Int Int

instance Element StackedBarChartData where
  renderElement (StackedBarChartData [] _ _) = "No data"
  renderElement (StackedBarChartData groups w h) =
    let maxTotal = maximum [ sum [ v | BarItem v _ _ <- segs ] | StackedBarGroup segs _ <- groups ]
        nGroups  = length groups
        barW     = max 1 $ (w - nGroups + 1) `div` nGroups
        (yLabels, yLabelW) = yAxisLabels maxTotal h

        -- (segment, (bottom, top)) in eighths
        groupBounds =
          [ let cum = scanl (+) 0 [ round (v / maxTotal * fromIntegral (h * 8)) :: Int | BarItem v _ _ <- segs ]
            in zip segs (zip cum (drop 1 cum))
          | StackedBarGroup segs _ <- groups ]

        allLabels = nub [ nm | StackedBarGroup segs _ <- groups, BarItem _ nm _ <- segs ]

        -- A label keeps one colour across bars and legend: its first explicit
        -- colour, else the palette slot for its index.
        labelColors = [ (nm, pickColor i (firstExplicit nm)) | (i, nm) <- zip [0..] allLabels ]
        firstExplicit nm = case [ cl | StackedBarGroup segs _ <- groups
                                     , BarItem _ nm' cl <- segs
                                     , nm' == nm, cl /= ColorDefault ] of
          (cl:_) -> cl
          []     -> ColorDefault
        labelColor nm = fromMaybe ColorDefault (lookup nm labelColors)

        -- Only the highest segment reaching into a row is drawn in it.
        cellAt r bounds =
          case [ (seg, top) | (seg, (bot, top)) <- bounds, top > r * 8, bot < r * 8 + 8 ] of
            [] -> T.replicate barW " "
            overlapping ->
              let (BarItem _ nm _, top) = foldl1 (\a b -> if snd b > snd a then b else a) overlapping
              in barCell barW (labelColor nm) (min 8 $ max 0 (top - r * 8))

        gridLines =
          [ padLeft yLabelW yLab <> " │" <> T.intercalate " " (map (cellAt r) groupBounds)
          | (r, yLab) <- zip [h - 1, h - 2 ..] yLabels ]

        gutter    = T.replicate (yLabelW + 2) " "
        xAxis     = gutter <> T.replicate (nGroups * barW + nGroups - 1) "─"
        grpLabels = gutter <> T.intercalate " "
                      [ T.take barW (T.pack nm <> T.replicate barW " ") | StackedBarGroup _ nm <- groups ]

        legend
          | length allLabels <= 1 = []
          | otherwise = ["", T.intercalate "  "
              [ wrapAnsi (labelColor nm) "█" <> " " <> T.pack nm | nm <- allLabels ]]

    in T.intercalate "\n" (gridLines <> [xAxis, grpLabels] <> legend)

plotStackedBar :: Int -> Int -> [StackedBarGroup] -> L
plotStackedBar w h groups = L (StackedBarChartData groups w h)

-- | Grid of values, row labels, column labels.
data HeatmapData = HeatmapData [[Double]] [String] [String]

data HeatmapElement = HeatmapElement HeatmapData Int

instance Element HeatmapElement where
  renderElement (HeatmapElement (HeatmapData [] _ _) _) = "No data"
  renderElement (HeatmapElement (HeatmapData grid rowLbls colLbls) cellW) =
    let allVals = concat grid
        mn  = minimum allVals
        mx  = maximum allVals
        rng = if mx == mn then 1.0 else mx - mn

        -- blue -> cyan -> green -> yellow -> red through the 256-colour cube
        toColor256 :: Double -> Int
        toColor256 t
          | t <= 0.0  = 21
          | t >= 1.0  = 196
          | t < 0.25  = let s = t / 0.25      in round (21.0  + s * 30.0)
          | t < 0.5   = let s = (t-0.25)/0.25 in round (51.0  + s * (-5.0))
          | t < 0.75  = let s = (t-0.5)/0.25  in round (46.0  + s * 180.0)
          | otherwise = let s = (t-0.75)/0.25 in round (226.0 + s * (-30.0))

        rowLblW = maximum (0 : map length rowLbls)
        gutter  = T.replicate (rowLblW + 1) " "
        fit n   = padRight n . T.take n

        header = gutter <> T.intercalate " " (map (fit cellW . T.pack) colLbls)

        dataRows = zipWith (\lbl vals ->
          fit rowLblW (T.pack lbl) <> " " <> T.intercalate " "
            [ bgColor256 (toColor256 ((v - mn) / rng)) <> fit cellW (formatAxisNum v) <> ansiReset | v <- vals ]
          ) rowLbls grid

        legendBar  = foldMap (\i -> bgColor256 (toColor256 (fromIntegral i / 10.0)) <> " " <> ansiReset) [0..10 :: Int]
        legendLine = gutter <> formatAxisNum mn <> " " <> legendBar <> " " <> formatAxisNum mx

    in T.intercalate "\n" ([header] <> dataRows <> ["", legendLine])

-- | Heatmap with 6-column cells.
plotHeatmap :: HeatmapData -> L
plotHeatmap = plotHeatmap' 6

plotHeatmap' :: Int -> HeatmapData -> L
plotHeatmap' cellW dat = L (HeatmapElement dat cellW)

--------------------------------------------------------------------------------
-- TUI runtime

data Key
  = KeyChar Char
  | KeyCtrl Char           -- ^ e.g. @KeyCtrl 'C'@
  | KeyEnter
  | KeyBackspace
  | KeyTab
  | KeyEscape
  | KeyDelete
  | KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd
  | KeySpecial String      -- ^ unrecognised escape sequence
  deriving (Show, Eq)

-- | Side effects returned from 'appUpdate'.
data Cmd msg
  = CmdNone
  | CmdRun (IO (Maybe msg))
  | CmdBatch [Cmd msg]
  | CmdExit

-- | Fire and forget.
cmdFire :: IO () -> Cmd msg
cmdFire io = CmdRun (io >> pure Nothing)

-- | Run IO and feed the result back as a message.
cmdTask :: IO msg -> Cmd msg
cmdTask io = CmdRun (Just <$> io)

cmdAfterMs :: Int -> msg -> Cmd msg
cmdAfterMs ms msg = CmdRun (threadDelay (ms * 1000) >> pure (Just msg))

-- | Run a command, returning the first message it produced. 'CmdExit' is
-- left for the runtime to notice via 'cmdIsExit'.
executeCmd :: Cmd msg -> IO (Maybe msg)
executeCmd CmdNone         = pure Nothing
executeCmd CmdExit         = pure Nothing
executeCmd (CmdRun io)     = io
executeCmd (CmdBatch cmds) = asum <$> mapM executeCmd cmds

cmdIsExit :: Cmd msg -> Bool
cmdIsExit CmdExit         = True
cmdIsExit (CmdBatch cmds) = any cmdIsExit cmds
cmdIsExit _               = False

data Sub msg
  = SubNone
  | SubKeyPress (Key -> Maybe msg)
  | SubEveryMs Int msg               -- ^ interval in ms
  | SubBatch [Sub msg]

subBatch :: [Sub msg] -> Sub msg
subBatch = SubBatch

subKeyPress :: (Key -> Maybe msg) -> Sub msg
subKeyPress = SubKeyPress

subEveryMs :: Int -> msg -> Sub msg
subEveryMs = SubEveryMs

-- | An Elm-style app.
--
-- @
-- data Msg = Inc | Dec
--
-- counter :: 'LayoutzApp' Int Msg
-- counter = 'LayoutzApp'
--   { 'appInit'          = (0, 'CmdNone')
--   , 'appUpdate'        = \\msg n -> case msg of
--       Inc -> (n + 1, 'CmdNone')
--       Dec -> (n - 1, 'CmdNone')
--   , 'appSubscriptions' = \\_ -> 'subKeyPress' $ \\case
--       'KeyChar' \'+\' -> Just Inc
--       'KeyChar' \'-\' -> Just Dec
--       _             -> Nothing
--   , 'appView'          = \\n -> 'layout' ['text' $ "Count: " <> show n]
--   }
-- @
data LayoutzApp state msg = LayoutzApp
  { appInit          :: (state, Cmd msg)
  , appUpdate        :: msg -> state -> (state, Cmd msg)
  , appSubscriptions :: state -> Sub msg
  , appView          :: state -> L
  }

data AppAlignment = AppAlignLeft | AppAlignCenter | AppAlignRight
  deriving (Show, Eq)

-- |
-- @
-- 'runAppWith' 'defaultAppOptions' { 'optAlignment' = 'AppAlignCenter' } myApp
-- @
data AppOptions = AppOptions
  { optAlignment    :: AppAlignment
  , optClearOnStart :: Bool  -- ^ enter the alt screen and clear
  , optClearOnExit  :: Bool  -- ^ leave the alt screen
  } deriving (Show, Eq)

-- | Left-aligned, full screen.
defaultAppOptions :: AppOptions
defaultAppOptions = AppOptions
  { optAlignment    = AppAlignLeft
  , optClearOnStart = True
  , optClearOnExit  = True
  }

-- | Save cursor, jump to the far corner, ask where we ended up, restore.
cursorProbe :: Text
cursorProbe = "\ESC7\ESC[999;999H\ESC[6n\ESC8"

-- | Terminal width from a cursor position report; 80 if the terminal
-- doesn't answer within 100ms.
getTerminalWidth :: IO Int
getTerminalWidth = do
  putText cursorProbe
  hFlush stdout
  fromMaybe 80 <$> timeout 100000 readCPR
  where
    -- ESC [ rows ; cols R
    readCPR = do
      c <- getChar
      if c /= '\ESC' then pure 80 else do
        c2 <- getChar
        if c2 /= '[' then pure 80 else parseResponse ""
    parseResponse acc = do
      c <- getChar
      if c /= 'R' then parseResponse (c : acc) else
        pure $ case break (== ';') (reverse acc) of
          (_, ';':cols) | [(n, "")] <- reads cols -> n
          _ -> 80

-- | @(rows, cols)@, falling back to @(24, 80)@. Needs raw mode and nobody
-- else reading stdin.
getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  putText cursorProbe
  hFlush stdout
  fromMaybe (24, 80) <$> timeout 100000 (readCPR "")
  where
    readCPR acc = do
      c <- getChar
      case c of
        '\ESC' -> getChar >> readCPR acc
        '['    -> readCPR acc
        'R'    -> pure (parseRC (reverse acc))
        _      -> readCPR (c : acc)
    parseRC resp = case break (== ';') resp of
      (rows, ';':cols) -> (readOr 24 rows, readOr 80 cols)
      _                -> (24, 80)
    readOr d s = case reads s of
      [(n, "")] -> n
      _         -> d

-- | Run full screen. Esc, Ctrl-C or Ctrl-D quits.
runApp :: LayoutzApp state msg -> IO ()
runApp app = () <$ runAppWithFinal defaultAppOptions app

-- | Like 'runApp', returning the final state.
runAppFinal :: LayoutzApp state msg -> IO state
runAppFinal = runAppWithFinal defaultAppOptions

-- | Render in place below the current output (no alt screen) until the app
-- issues 'CmdExit'. Handy for progress bars in scripts.
runInline :: LayoutzApp state msg -> IO ()
runInline app = () <$ runAppWithFinal defaultAppOptions
  { optClearOnStart = False, optClearOnExit = False } app

runAppWith :: AppOptions -> LayoutzApp state msg -> IO ()
runAppWith opts app = () <$ runAppWithFinal opts app

-- | Every source (keys, ticks, command results) feeds one channel, so a single
-- thread applies 'appUpdate'.
data AppEvent msg
  = AppMsg msg
  | AppExit

isQuitKey :: Key -> Bool
isQuitKey k = k == KeyEscape || k == KeyCtrl 'C' || k == KeyCtrl 'D'

-- | Like 'runAppWith', returning the final state.
runAppWithFinal :: AppOptions -> LayoutzApp state msg -> IO state
runAppWithFinal opts LayoutzApp{..} = do
  oldBuffering <- hGetBuffering stdin
  oldEcho <- hGetEcho stdin

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  when (optClearOnStart opts) $ enterAltScreen >> clearScreen
  hideCursor

  -- before any thread starts reading stdin
  termWidth <- getTerminalWidth

  let (initialState, initialCmd) = appInit

  stateRef  <- newIORef initialState
  eventChan <- newChan
  cmdChan   <- newChan

  let applyMsg msg = do
        cmd <- atomicModifyIORef' stateRef (appUpdate msg)
        case cmd of
          CmdNone -> pure ()
          CmdExit -> writeChan eventChan AppExit
          _       -> writeChan cmdChan cmd

  cmdThread <- forkIO $ forever $ do
    cmd <- readChan cmdChan
    when (cmdIsExit cmd) $ writeChan eventChan AppExit
    executeCmd cmd >>= mapM_ (writeChan eventChan . AppMsg)

  case initialCmd of
    CmdNone -> pure ()
    _       -> writeChan cmdChan initialCmd

  lastLineCount <- newIORef 0
  lastRendered  <- newIORef ""
  sentKittyIds  <- newIORef IntSet.empty

  renderThread <- forkIO $ forever $ do
    rendered <- renderElement . appView <$> readIORef stateRef
    prev <- readIORef lastRendered

    when (rendered /= prev) $ do
      prevLineCount <- readIORef lastLineCount
      -- upload each kitty image once, not every frame
      sent <- readIORef sentKittyIds
      let (deduped, sent') = dedupeTransmits sent rendered
      writeIORef sentKittyIds sent'
      let ls        = T.lines deduped
          lineCount = length ls
          blockW    = maximum (0 : map visibleLength ls)
          indent = case optAlignment opts of
            AppAlignLeft   -> 0
            AppAlignCenter -> max 0 ((termWidth - blockW) `div` 2)
            AppAlignRight  -> max 0 (termWidth - blockW)
          aligned
            | indent > 0 = map (T.replicate indent " " <>) ls
            | otherwise  = ls
          home
            | optClearOnStart opts = "\ESC[H"
            | prevLineCount > 0    = "\ESC[" <> T.show prevLineCount <> "A\r"
            | otherwise            = ""
      putText $ home <> foldMap (<> "\ESC[K\n") aligned
                <> T.replicate (max 0 (prevLineCount - lineCount)) "\ESC[K\n"
      hFlush stdout
      writeIORef lastRendered rendered
      writeIORef lastLineCount lineCount

    threadDelay 33333

  let keyHandler = \case
        SubKeyPress f -> Just f
        SubBatch subs -> case [f | SubKeyPress f <- subs] of
          (f:_) -> Just f
          []    -> Nothing
        _ -> Nothing

      tickSubs = \case
        SubEveryMs interval msg -> [(interval, msg)]
        SubBatch subs           -> concatMap tickSubs subs
        _                       -> []

  -- Deadlines are tracked per interval so re-subscribing each frame doesn't
  -- reset the clock. Polls every 5ms.
  tickThread <- forkIO $
    let loop deadlines = do
          ticks <- tickSubs . appSubscriptions <$> readIORef stateRef
          now   <- getMonotonicTimeNSec
          let ns ms     = fromIntegral ms * 1000 * 1000
              armed     = [ (i, fromMaybe (now + ns i) (lookup i deadlines)) | i <- nub (map fst ticks) ]
              due       = [ i | (i, d) <- armed, d <= now ]
              deadlines' =
                [ (i, if i `elem` due then let nd = d + ns i in if nd <= now then now + ns i else nd else d)
                | (i, d) <- armed ]
          mapM_ (writeChan eventChan . AppMsg) [ msg | (i, msg) <- ticks, i `elem` due ]
          threadDelay 5000
          loop deadlines'
    in loop []

  -- EOF on stdin quits, same as Ctrl-D.
  inputThread <- forkIO $
    let loop = do
          key <- readKey
          if isQuitKey key then writeChan eventChan AppExit else do
            sub <- appSubscriptions <$> readIORef stateRef
            mapM_ (writeChan eventChan . AppMsg) (keyHandler sub >>= ($ key))
            loop
    in loop `catch` \(_ :: IOException) -> writeChan eventChan AppExit

  let cleanup = do
        mapM_ killThread [renderThread, cmdThread, tickThread, inputThread]
        showCursor
        when (optClearOnExit opts) exitAltScreen
        hFlush stdout
        hSetBuffering stdin oldBuffering
        hSetEcho stdin oldEcho

      dispatch = readChan eventChan >>= \case
        AppExit    -> pure ()
        AppMsg msg -> applyMsg msg >> dispatch

      -- Ctrl-C is a normal quit; everything else rethrows, and `finally`
      -- puts the tty back either way.
      onAsync UserInterrupt = pure ()
      onAsync e             = throwIO e

  (dispatch `catch` onAsync) `finally` cleanup

  readIORef stateRef

-- | One write(2) per call. Data.Text.IO on an unbuffered handle issues a
-- syscall per character, so big frames reach the terminal in pieces and tear.
putText :: Text -> IO ()
putText = Utf8.putStr

clearScreen :: IO ()
clearScreen = putText "\ESC[2J\ESC[H"

hideCursor :: IO ()
hideCursor = putText "\ESC[?25l"

showCursor :: IO ()
showCursor = putText "\ESC[?25h"

enterAltScreen :: IO ()
enterAltScreen = putText "\ESC[?1049h"

exitAltScreen :: IO ()
exitAltScreen = putText "\ESC[?1049l"

readKey :: IO Key
readKey = getChar >>= \c -> case ord c of
    10  -> pure KeyEnter
    13  -> pure KeyEnter
    27  -> readEscapeSequence
    9   -> pure KeyTab
    127 -> pure KeyBackspace
    8   -> pure KeyBackspace
    n | n >= 32 && n <= 126 -> pure $ KeyChar c
      | n < 32              -> pure $ KeyCtrl (chr (n + 64))
      | otherwise           -> pure $ KeyChar c

-- | A lone ESC is told apart from an escape sequence by waiting 50ms for
-- the next byte.
readEscapeSequence :: IO Key
readEscapeSequence = timeout 50000 getChar >>= \case
  Nothing  -> pure KeyEscape
  Just '[' -> getChar >>= \case
    'A' -> pure KeyUp
    'B' -> pure KeyDown
    'C' -> pure KeyRight
    'D' -> pure KeyLeft
    'H' -> pure KeyHome
    'F' -> pure KeyEnd
    n | n >= '1' && n <= '6' -> do   -- ESC [ n ~
      c3 <- getChar
      pure $ if c3 /= '~' then KeyEscape else case n of
        '1' -> KeyHome
        '3' -> KeyDelete
        '4' -> KeyEnd
        '5' -> KeyPageUp
        '6' -> KeyPageDown
        _   -> KeyEscape
    _ -> pure KeyEscape
  Just 'O' -> getChar >>= \case      -- application cursor mode
    'H' -> pure KeyHome
    'F' -> pure KeyEnd
    _   -> pure KeyEscape
  Just _ -> pure KeyEscape

--------------------------------------------------------------------------------
-- Small ANSI / text helpers

dimText :: Text -> Text
dimText = wrapStyle StyleDim

-- | Unlike 'T.lines', a trailing separator yields a final empty segment.
splitOnChar :: Char -> Text -> [Text]
splitOnChar c = T.splitOn (T.singleton c)

-- | @(before, rest)@ where @rest@ starts with @pat@.
splitOnFirst :: Text -> Text -> Maybe (Text, Text)
splitOnFirst pat s
  | T.null pat = Just ("", s)
  | otherwise  = case T.breakOn pat s of
      (before, rest) | T.null rest -> Nothing
                     | otherwise   -> Just (before, rest)

-- | Background SGR for a foreground 'Color'.
bgColorCode :: Color -> Text
bgColorCode ColorDefault      = ""
bgColorCode (ColorFull n)     = "48;5;" <> T.show (clamp n)
bgColorCode (ColorTrue r g b) = "48;2;" <> T.show (clamp r) <> ";" <> T.show (clamp g) <> ";" <> T.show (clamp b)
bgColorCode c = case reads (T.unpack (colorCode c)) :: [(Int, String)] of
  [(n, "")] -> T.show (n + 10)   -- 3x -> 4x, 9x -> 10x
  _         -> ""

--------------------------------------------------------------------------------
-- Kitty graphics

formatPng, formatRgb, formatRgba :: Int
formatPng  = 100
formatRgb  = 24
formatRgba = 32

-- | An inline image drawn with kitty's Unicode placeholders. It measures
-- exactly @cols@ x @rows@, so it lays out like any other element inside
-- 'box', 'row', 'table' and friends.
--
-- Build one with 'kittyImage', 'kittyRGB', 'kittyRGBA' or 'kittyImageFile'.
data KittyImage = KittyImage
  { kiPayload    :: [Word8]    -- ^ PNG bytes, or raw RGB\/RGBA pixels
  , kiFormat     :: Int        -- ^ 100 = PNG, 24 = RGB, 32 = RGBA
  , kiCols       :: Int
  , kiRows       :: Int
  , kiPxW        :: Int        -- ^ source pixel width (raw formats only)
  , kiPxH        :: Int        -- ^ source pixel height (raw formats only)
  , kiAlt        :: Text
  , kiIdOverride :: Maybe Int  -- ^ defaults to a hash of the payload
  }

instance Element KittyImage where
  renderElement img = kittyTransmit img <> kittyPlacement img
  width  = kiCols
  height = kiRows

-- | 24-bit FNV-1a of the payload (never 0). Stable across frames, which is
-- what lets the runtime skip re-uploading.
kittyImageId :: [Word8] -> Maybe Int -> Int
kittyImageId payload override =
  let masked = fromMaybe (foldl step 0x811c9dc5 payload) override .&. 0xffffff
  in if masked == 0 then 1 else masked
  where
    step h b = (h `xor` fromIntegral b) * 0x01000193 .&. 0xffffffff

kittyBase64 :: [Word8] -> Text
kittyBase64 = T.pack . go
  where
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" :: String
    sextet t s = alphabet !! ((t `shiftR` s) .&. 0x3f)
    go [] = ""
    go (b0:b1:b2:rest) =
      let t = (fromIntegral b0 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2
      in sextet t 18 : sextet t 12 : sextet t 6 : sextet t 0 : go rest
    go [b0, b1] =
      let t = (fromIntegral b0 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 8) :: Int
      in sextet t 18 : sextet t 12 : sextet t 6 : "="
    go [b0] =
      let t = fromIntegral b0 `shiftL` 16 :: Int
      in sextet t 18 : sextet t 12 : "=="

kittyApcG :: Text
kittyApcG = "\ESC_G"

kittyST :: Text
kittyST = "\ESC\\"

-- | Base64 payload split into 4096-byte APC chunks. Every chunk carries
-- @i=<id>@ so 'dedupeTransmits' can drop all of them at once.
kittyTransmit :: KittyImage -> Text
kittyTransmit img = T.concat (zipWith block [0 ..] chunks)
  where
    b64    = kittyBase64 (kiPayload img)
    chunks = if T.null b64 then [""] else T.chunksOf 4096 b64
    theId  = kittyImageId (kiPayload img) (kiIdOverride img)
    dims
      | kiFormat img == formatPng = ""
      | otherwise = ",s=" <> T.show (kiPxW img) <> ",v=" <> T.show (kiPxH img)
    firstKeys = "a=T,U=1,i=" <> T.show theId <> ",f=" <> T.show (kiFormat img)
                <> ",c=" <> T.show (kiCols img) <> ",r=" <> T.show (kiRows img) <> dims <> ",q=2"
    contKeys  = "i=" <> T.show theId <> ",q=2"
    lastIdx   = length chunks - 1
    block :: Int -> Text -> Text
    block i chunk =
      let more = if i == lastIdx then "0" else "1"
          keys = (if i == 0 then firstKeys else contKeys) <> ",m=" <> more
      in kittyApcG <> keys <> ";" <> chunk <> kittyST

-- | One line of placeholders per image row. The id travels in the foreground
-- colour; the first cell of each row carries (row, col 0) as diacritics.
kittyPlacement :: KittyImage -> Text
kittyPlacement img = T.intercalate "\n" (map rowLine [0 .. kiRows img - 1])
  where
    theId = kittyImageId (kiPayload img) (kiIdOverride img)
    fg = "\ESC[38;2;" <> T.show ((theId `shiftR` 16) .&. 0xff) <> ";"
         <> T.show ((theId `shiftR` 8) .&. 0xff) <> ";" <> T.show (theId .&. 0xff) <> "m"
    placeholder = T.singleton kittyPlaceholderChar
    diacritic i = T.singleton (chr (rowColumnDiacritics !! i))
    rowLine r = fg <> placeholder <> diacritic r <> diacritic 0
                <> T.replicate (kiCols img - 1) placeholder <> ansiReset

-- | Drop transmit blocks for ids already in the set, adding new ones.
-- Placements are left alone.
dedupeTransmits :: IntSet.IntSet -> Text -> (Text, IntSet.IntSet)
dedupeTransmits = go
  where
    go ids s = case splitOnFirst kittyApcG s of
      Nothing -> (s, ids)
      Just (before, rest) ->
        let (block, after) = takeBlock rest
            bid = parseId block
            (keep, ids')
              | bid /= 0 && IntSet.member bid ids = ("", ids)
              | bid /= 0                          = (block, IntSet.insert bid ids)
              | otherwise                         = (block, ids)
            (restOut, idsF) = go ids' after
        in (before <> keep <> restOut, idsF)

    takeBlock s = case splitOnFirst kittyST (T.drop (T.length kittyApcG) s) of
      Nothing           -> (s, "")   -- unterminated, keep as is
      Just (mid, after) -> (kittyApcG <> mid <> kittyST, T.drop (T.length kittyST) after)

    parseId block =
      let keys = T.takeWhile (/= ';') (T.drop (T.length kittyApcG) block)
      in case [ T.drop 2 f | f <- splitOnChar ',' keys, "i=" `T.isPrefixOf` f ] of
           (v:_) | ds <- T.takeWhile isDigit v, not (T.null ds) -> read (T.unpack ds)
           _ -> 0

mkKitty :: [Word8] -> Int -> Int -> Int -> Int -> Int -> Text -> KittyImage
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

-- | PNG bytes shown in a @cols@ x @rows@ cell box. The terminal does the
-- decoding and scaling.
kittyImage :: [Word8] -> Int -> Int -> L
kittyImage bytes cols rows = L (mkKitty bytes formatPng cols rows 0 0 "")

-- | Raw RGB pixels, @pxW@ x @pxH@, shown in a @cols@ x @rows@ cell box.
kittyRGB :: [Word8] -> Int -> Int -> Int -> Int -> L
kittyRGB pixels pxW pxH cols rows = L (mkKitty pixels formatRgb cols rows pxW pxH "")

-- | 'kittyRGB' with an alpha channel.
kittyRGBA :: [Word8] -> Int -> Int -> Int -> Int -> L
kittyRGBA pixels pxW pxH cols rows = L (mkKitty pixels formatRgba cols rows pxW pxH "")

-- | Load a PNG from disk.
kittyImageFile :: FilePath -> Int -> Int -> IO L
kittyImageFile path cols rows = (\bytes -> kittyImage bytes cols rows) <$> readFileBytes path

readFileBytes :: FilePath -> IO [Word8]
readFileBytes path = withBinaryFile path ReadMode $ \h -> do
  contents <- hGetContents h
  length contents `seq` pure (map (fromIntegral . ord) contents)

--------------------------------------------------------------------------------
-- Progress loaders

-- | Bar glyphs and colour for bounded loaders; spinner and colour for
-- streaming ones.
data LoaderStyle = LoaderStyle
  { lsFill    :: Char
  , lsEmpty   :: Char
  , lsOpen    :: Text
  , lsClose   :: Text
  , lsHead    :: Text
  , lsSmooth  :: Bool
  , lsSpinner :: SpinnerStyle
  , lsColor   :: Color
  }

-- | @████▊░░░@, the default.
styleBlocks :: LoaderStyle
styleBlocks = LoaderStyle '█' '░' "" "" "" True SpinnerGrow ColorCyan

-- | @[██████░░░░]@
styleBar :: LoaderStyle
styleBar = LoaderStyle '█' '░' "[" "]" "" False SpinnerDots ColorGreen

-- | @[====>    ]@
styleAscii :: LoaderStyle
styleAscii = LoaderStyle '=' ' ' "[" "]" ">" False SpinnerLine ColorYellow

-- | @●●●●∙∙∙∙@
styleDots :: LoaderStyle
styleDots = LoaderStyle '●' '∙' "" "" "" False SpinnerBounce ColorMagenta

-- | @━━━━────@
styleLine :: LoaderStyle
styleLine = LoaderStyle '━' '─' "" "" "" False SpinnerLine ColorBlue

-- | @▰▰▰▱▱▱@
stylePipes :: LoaderStyle
stylePipes = LoaderStyle '▰' '▱' "" "" "" False SpinnerArrow ColorBrightMagenta

loaderStyles :: [LoaderStyle]
loaderStyles = [styleBlocks, styleBar, styleAscii, styleDots, styleLine, stylePipes]

-- | Partial cells for smooth bars, in eighths.
blockEighths :: [Text]
blockEighths = ["", "▏", "▎", "▍", "▌", "▋", "▊", "▉"]

renderProgressBar :: LoaderStyle -> Double -> Int -> Text
renderProgressBar LoaderStyle{..} progress w =
  lsOpen <> wrapAnsi lsColor (replicateChar filled lsFill <> tip) <> dimText (replicateChar rest lsEmpty) <> lsClose
  where
    exact  = max 0.0 (min 1.0 progress) * fromIntegral w
    filled = floor exact :: Int
    eighths = floor ((exact - fromIntegral filled) * 8) :: Int
    (tip, rest)
      | lsSmooth = if filled < w && eighths > 0
                     then (blockEighths !! eighths, w - filled - 1)
                     else ("", w - filled)
      | not (T.null lsHead) && filled > 0 && filled < w = (lsHead, max 0 (w - filled - T.length lsHead))
      | otherwise = ("", max 0 (w - filled))

-- | Redraw @frameAt i@ on the current line every 80ms while @action@ runs.
withTicker :: (Int -> IO Text) -> IO a -> IO a
withTicker frameAt action = do
  stopRef <- newIORef False
  let tick i = do
        stop <- readIORef stopRef
        unless stop $ do
          line <- frameAt i
          putText ("\r" <> line <> "\ESC[K")
          hFlush stdout
          threadDelay 80000
          tick (i + 1)
  ticker <- forkIO (tick 0)
  r <- action
  writeIORef stopRef True
  killThread ticker
  pure r

runLoader :: (Int -> IO Text) -> IO Text -> IORef Int -> [a] -> (a -> IO b) -> IO [b]
runLoader frameAt finalLine counter items action = do
  hideCursor
  hFlush stdout
  results <- withTicker frameAt $
    mapM (\x -> action x <* modifyIORef' counter (+ 1)) items
  final <- finalLine
  putText ("\r" <> final <> "\ESC[K\n")
  showCursor
  hFlush stdout
  pure results

labelPrefix :: Text -> Text
labelPrefix label = if T.null label then "" else label <> " "

boundedFrame :: LoaderStyle -> Text -> Int -> IORef Int -> IO Text
boundedFrame style label total counter = do
  done <- readIORef counter
  let progress = if total > 0 then min 1.0 (fromIntegral done / fromIntegral total) else 1.0
  pure $ labelPrefix label <> renderProgressBar style progress 20
      <> " " <> T.show done <> "/" <> T.show total
      <> " (" <> T.show (floor (progress * 100) :: Int) <> "%)"

streamingFrame :: LoaderStyle -> Text -> IORef Int -> Int -> IO Text
streamingFrame style label counter i = do
  done <- readIORef counter
  pure $ wrapAnsi (lsColor style) (spinnerFrame (lsSpinner style) i)
      <> " " <> labelPrefix label <> "(" <> T.show done <> " processed)"

streamingFinal :: Text -> IORef Int -> IO Text
streamingFinal label counter = do
  done <- readIORef counter
  pure (labelPrefix label <> T.show done <> " processed")

-- | Map an action over a list while drawing an inline progress bar.
--
-- @
-- _ <- 'loader' "Processing" [1..100] (\\_ -> threadDelay 20000)
-- @
loader :: String -> [a] -> (a -> IO b) -> IO [b]
loader = loaderStyled styleBlocks

loaderStyled :: LoaderStyle -> String -> [a] -> (a -> IO b) -> IO [b]
loaderStyled style labelS items action = do
  counter <- newIORef 0
  let frame = boundedFrame style (T.pack labelS) (length items) counter
  runLoader (const frame) frame counter items action

-- | Like 'loader' but for when the length isn't worth forcing: a spinner and
-- a running count.
loaderStream :: String -> [a] -> (a -> IO b) -> IO [b]
loaderStream = loaderStreamStyled styleBlocks

loaderStreamStyled :: LoaderStyle -> String -> [a] -> (a -> IO b) -> IO [b]
loaderStreamStyled style labelS items action = do
  let label = T.pack labelS
  counter <- newIORef 0
  runLoader (streamingFrame style label counter) (streamingFinal label counter) counter items action

--------------------------------------------------------------------------------
-- Ask: one-shot prompts
--
-- Each grabs the tty, returns a value and leaves one summary line behind.
-- 'Nothing' means cancelled (Esc / Ctrl-C / Ctrl-D).

askCursorUp :: Int -> Text
askCursorUp n = "\ESC[" <> T.show n <> "A\r"

askClearEol :: Text
askClearEol = "\ESC[K"

askClearBelow :: Text
askClearBelow = "\ESC[J"

-- | Raw mode and a hidden cursor for the duration of @body@.
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

-- | Redraw a frame over the previous one; returns its height.
askRepaint :: Text -> Int -> IO Int
askRepaint frame prevLines = do
  let ls = splitOnChar '\n' frame
  putText $ (if prevLines > 0 then askCursorUp prevLines else "")
            <> foldMap (<> askClearEol <> "\n") ls
            <> (if prevLines > length ls then askClearBelow else "")
  hFlush stdout
  pure (length ls)

-- | Replace the live frame with a single summary line.
askCommit :: Text -> Int -> IO ()
askCommit summary prevLines = do
  putText $ (if prevLines > 0 then askCursorUp prevLines else "")
            <> summary <> askClearEol <> "\n"
            <> (if prevLines > 1 then askClearBelow else "")
  hFlush stdout

askErase :: Int -> IO ()
askErase prevLines = when (prevLines > 0) $ do
  putText (askCursorUp prevLines <> askClearBelow)
  hFlush stdout

askActiveBtn :: Color -> Text -> Text
askActiveBtn bg t = "\ESC[1;37;" <> bgColorCode bg <> "m" <> t <> "\ESC[0m"

caret :: Text
caret = wrapAnsi ColorCyan "▌"

-- | Show @window@ rows of @xs@, scrolled to keep @idx@ roughly centred.
-- Returns the offset of the first visible row.
viewport :: Int -> Int -> [a] -> (Int, [a])
viewport window idx xs = (top, take h (drop top xs))
  where
    total = length xs
    h     = max 0 (min window total)
    top   = if total <= h then 0 else max 0 (min (idx - h `div` 2) (total - h))

renderInputFrame :: Text -> Text -> Text -> Text
renderInputFrame prompt value placeholder =
  prompt <> (if T.null value && not (T.null placeholder) then dimText placeholder else value) <> caret

renderConfirmFrame :: Text -> Bool -> Text -> Text -> Text
renderConfirmFrame question yes affirmative negative =
  question <> "\n  " <> btn affirmative yes ColorGreen <> "  " <> btn negative (not yes) ColorRed
  where
    btn t active bg = let padded = "  " <> t <> "  "
                      in if active then askActiveBtn bg padded else padded

renderChooseFrame :: Text -> [a] -> Int -> (a -> Text) -> Text
renderChooseFrame prompt items idx renderItem =
  (if T.null prompt then "" else prompt <> "\n") <> T.intercalate "\n"
    [ if i == idx then wrapAnsi ColorCyan ("› " <> renderItem item) else "  " <> renderItem item
    | (item, i) <- zip items [0 ..] ]

renderChooseManyFrame :: Text -> [a] -> Int -> [Int] -> Int -> (a -> Text) -> Text
renderChooseManyFrame prompt items idx selected limit renderItem =
  header <> T.intercalate "\n" rows
  where
    rows = [ let mark = if i `elem` selected then wrapAnsi ColorGreen "[x]" else dimText "[ ]"
                 line = mark <> " " <> renderItem item
             in if i == idx then wrapAnsi ColorCyan "› " <> line else "  " <> line
           | (item, i) <- zip items [0 ..] ]
    n = T.show (length selected)
    tag | limit > 0          = " (" <> n <> "/" <> T.show limit <> ")"
        | not (null selected) = " (" <> n <> ")"
        | otherwise          = ""
    header
      | T.null prompt = ""
      | otherwise     = prompt <> dimText tag <> "\n"

renderWriteFrame :: Text -> Text -> Text -> Text -> Text
renderWriteFrame prompt value placeholder hint =
  (if T.null prompt then "" else prompt <> "\n")
    <> (if T.null value && not (T.null placeholder) then dimText placeholder else value) <> caret
    <> (if T.null hint then "" else "\n" <> dimText hint)

renderFilterFrame :: Text -> Text -> [a] -> Int -> Int -> (a -> Text) -> Text
renderFilterFrame prompt query matches idx viewHeight renderItem =
  prompt <> query <> caret <> "\n" <> body
  where
    (top, visible) = viewport viewHeight idx matches
    rows = [ if top + i == idx then wrapAnsi ColorCyan ("› " <> label) else "  " <> label
           | (item, i) <- zip visible [0 ..], let label = renderItem item ]
    body = if null rows then dimText "  (no matches)" else T.intercalate "\n" rows

-- | 'Nothing' unless @query@ is a case-insensitive subsequence of @target@;
-- otherwise the total gap between matched characters (lower is tighter).
fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query target
  | T.null query = Just 0
  | otherwise    = go (T.toLower query) (T.toLower target) (-1) 0 0
  where
    go Empty _ _ gap _ = Just gap
    go _ Empty _ _ _   = Nothing
    go q@(qc :< qs) (sc :< ss) lastHit gap i
      | qc == sc  = go qs ss i (if lastHit >= 0 then gap + (i - lastHit - 1) else gap) (i + 1)
      | otherwise = go q ss lastHit gap (i + 1)

fuzzyMatches :: Text -> [a] -> (a -> Text) -> [a]
fuzzyMatches query items renderItem
  | T.null query = items
  | otherwise    = map fst $ sortOn snd
      [ (item, score) | item <- items, Just score <- [fuzzyScore query (renderItem item)] ]

data FileEntry = FileEntry { feName :: FilePath, feIsDir :: Bool }

feDisplay :: FileEntry -> Text
feDisplay e = T.pack (if feIsDir e then feName e <> "/" else feName e)

renderFileFrame :: FilePath -> [FileEntry] -> Int -> Int -> Text
renderFileFrame path entries idx viewHeight =
  dimText (T.pack path) <> "\n" <> T.intercalate "\n" rows
  where
    (top, visible) = viewport viewHeight idx entries
    rows = [ let name = if feIsDir e then wrapAnsi ColorBlue (feDisplay e) else feDisplay e
             in if top + i == idx then wrapAnsi ColorCyan "› " <> name else "  " <> name
           | (e, i) <- zip visible [0 ..] ]

renderPagerFrame :: [Text] -> Int -> Int -> Int -> Bool -> Text
renderPagerFrame allLines top h viewWidth lineNumbers = body <> "\n" <> status
  where
    total    = length allLines
    numWidth = length (show total)
    maxLineW = max 20 (viewWidth - if lineNumbers then numWidth + 3 else 0)
    body = T.intercalate "\n"
      [ let line' = if T.length line > maxLineW then T.take (maxLineW - 1) line <> "…" else line
        in if lineNumbers then dimText (padLeft numWidth (T.show (top + i + 1)) <> " │ ") <> line' else line'
      | (line, i) <- zip (take h (drop top allLines)) [0 ..] ]
    viewEnd = min (top + h) total
    percent = if total == 0 then 100 else (viewEnd * 100) `div` total
    status  = dimText ("  L" <> T.show (top + 1) <> "–" <> T.show viewEnd <> " of " <> T.show total
                       <> "  " <> T.show percent <> "%   ↑↓ PgUp/PgDn Home/End  q to quit")

-- | Single-line text input.
askInput :: String       -- ^ prompt, e.g. @"› "@
         -> String       -- ^ placeholder
         -> String       -- ^ initial value
         -> IO (Maybe String)
askInput promptS placeholderS initialS = withAskTty $ do
  let prompt = T.pack promptS; placeholder = T.pack placeholderS
      frame v = renderInputFrame prompt v placeholder
      edit v pl = askRepaint (frame v) pl >>= loop v
      loop value pl = readKey >>= \case
        KeyEnter -> askCommit (prompt <> value) pl >> pure (Just (T.unpack value))
        k | isQuitKey k -> askErase pl >> pure Nothing
        KeyBackspace | not (T.null value) -> edit (T.init value) pl
        KeyChar c -> edit (T.snoc value c) pl
        _ -> loop value pl
  edit (T.pack initialS) 0

-- | Yes\/no. Cancelling returns the opposite of the default.
askConfirm :: String   -- ^ question
           -> Bool     -- ^ default
           -> String   -- ^ affirmative label, e.g. @"Yes"@
           -> String   -- ^ negative label, e.g. @"No"@
           -> IO Bool
askConfirm questionS def affirmativeS negativeS = withAskTty $ do
  let question = T.pack questionS; affirmative = T.pack affirmativeS; negative = T.pack negativeS
      draw yes pl = askRepaint (renderConfirmFrame question yes affirmative negative) pl >>= loop yes
      pick yes pl = do
        askCommit (question <> " " <> wrapAnsi ColorCyan (if yes then affirmative else negative)) pl
        pure yes
      loop yes pl = readKey >>= \case
        KeyEnter -> pick yes pl
        KeyChar c | c `elem` ("yY" :: String) -> pick True pl
                  | c `elem` ("nN" :: String) -> pick False pl
        KeyLeft  -> draw (not yes) pl
        KeyRight -> draw (not yes) pl
        KeyTab   -> draw (not yes) pl
        k | isQuitKey k -> askErase pl >> pure (not def)
        _ -> loop yes pl
  draw def 0

-- | Pick one. 'Nothing' if cancelled or @items@ is empty.
askChoose :: String -> [a] -> (a -> String) -> IO (Maybe a)
askChoose _ [] _ = pure Nothing
askChoose promptS items renderItemS = withAskTty $ do
  let prompt = T.pack promptS; renderItem = T.pack . renderItemS
      n = length items
      draw i pl = askRepaint (renderChooseFrame prompt items i renderItem) pl >>= loop i
      loop i pl = readKey >>= \case
        KeyEnter -> do
          let picked = items !! i
          askCommit (prompt <> " " <> wrapAnsi ColorCyan (renderItem picked)) pl
          pure (Just picked)
        KeyUp   -> draw ((i - 1 + n) `mod` n) pl
        KeyDown -> draw ((i + 1) `mod` n) pl
        KeyTab  -> draw ((i + 1) `mod` n) pl
        k | isQuitKey k -> askErase pl >> pure Nothing
        _ -> loop i pl
  draw 0 0

-- | Pick several. Space or Tab toggles, Enter commits; @limit@ 0 means no
-- limit. 'Nothing' if cancelled or @items@ is empty.
askChooseMany :: String -> [a] -> Int -> (a -> String) -> IO (Maybe [a])
askChooseMany _ [] _ _ = pure Nothing
askChooseMany promptS items limit renderItemS = withAskTty $ do
  let prompt = T.pack promptS; renderItem = T.pack . renderItemS
      n = length items
      toggle i sel
        | i `elem` sel                     = filter (/= i) sel
        | limit > 0 && length sel >= limit = sel
        | otherwise                        = sel <> [i]
      loop i sel prev = do
        pl <- askRepaint (renderChooseManyFrame prompt items i sel limit renderItem) prev
        readKey >>= \case
          KeyEnter -> do
            let picks = [ item | (item, j) <- zip items [0 ..], j `elem` sel ]
            askCommit (prompt <> " " <> if null picks
                         then dimText "(nothing)"
                         else wrapAnsi ColorCyan (T.intercalate ", " (map renderItem picks))) pl
            pure (Just picks)
          KeyUp       -> loop ((i - 1 + n) `mod` n) sel pl
          KeyDown     -> loop ((i + 1) `mod` n) sel pl
          KeyChar ' ' -> loop i (toggle i sel) pl
          KeyTab      -> loop i (toggle i sel) pl
          k | isQuitKey k -> askErase pl >> pure Nothing
          _ -> loop i sel pl
  loop 0 [] 0

-- | Spin while @task@ runs, then print a ✓ or ✗ line. Exceptions from the
-- task are rethrown.
askSpin :: String -> SpinnerStyle -> IO a -> IO a
askSpin labelS style task = do
  let label = T.pack labelS
  hideCursor
  hFlush stdout
  outcome <- withTicker (\i -> pure (wrapAnsi ColorCyan (spinnerFrame style i) <> " " <> label)) $
    (Right <$> task) `catch` \(e :: SomeException) -> pure (Left e)
  let (glyph, col) = either (const ("✗", ColorRed)) (const ("✓", ColorGreen)) outcome
  putText ("\r" <> wrapAnsi col (glyph <> " " <> label) <> askClearEol <> "\n")
  showCursor
  hFlush stdout
  either throwIO pure outcome

-- | Multi-line input. Enter adds a newline, Ctrl-D submits, Esc cancels.
askWrite :: String   -- ^ prompt
         -> String   -- ^ placeholder
         -> String   -- ^ initial value
         -> String   -- ^ hint line
         -> IO (Maybe String)
askWrite promptS placeholderS initialS hintS = withAskTty $ do
  let prompt = T.pack promptS; placeholder = T.pack placeholderS; hint = T.pack hintS
      loop value prev = do
        pl <- askRepaint (renderWriteFrame prompt value placeholder hint) prev
        readKey >>= \case
          KeyCtrl 'D' -> do
            let firstLine = case splitOnChar '\n' value of (x:_) -> x; [] -> ""
                summary   = if T.elem '\n' value then firstLine <> " …" else firstLine
            askCommit ((if T.null prompt then "" else prompt <> " ") <> wrapAnsi ColorCyan summary) pl
            pure (Just (T.unpack value))
          KeyEscape   -> askErase pl >> pure Nothing
          KeyCtrl 'C' -> askErase pl >> pure Nothing
          KeyEnter    -> loop (value <> "\n") pl
          KeyChar c   -> loop (T.snoc value c) pl
          KeyBackspace | not (T.null value) -> loop (T.init value) pl
          _ -> loop value pl
  loop (T.pack initialS) 0

-- | Type to fuzzy-filter, arrows to move, Enter to pick.
askFilter :: String -> [a] -> Int -> (a -> String) -> IO (Maybe a)
askFilter _ [] _ _ = pure Nothing
askFilter promptS items viewHeight renderItemS = withAskTty $ do
  let prompt = T.pack promptS; renderItem = T.pack . renderItemS
      loop query idx prev = do
        let matches = fuzzyMatches query items renderItem
            m = length matches
            i = if null matches then 0 else max 0 (min idx (m - 1))
            hasMatches = not (null matches)
        pl <- askRepaint (renderFilterFrame prompt query matches i viewHeight renderItem) prev
        readKey >>= \case
          KeyEnter | hasMatches -> do
            let picked = matches !! i
            askCommit (prompt <> query <> "  " <> wrapAnsi ColorCyan (renderItem picked)) pl
            pure (Just picked)
          KeyUp   | hasMatches -> loop query ((i - 1 + m) `mod` m) pl
          KeyDown | hasMatches -> loop query ((i + 1) `mod` m) pl
          KeyTab  | hasMatches -> loop query ((i + 1) `mod` m) pl
          KeyBackspace | not (T.null query) -> loop (T.init query) 0 pl
          KeyChar c -> loop (T.snoc query c) 0 pl
          k | isQuitKey k -> askErase pl >> pure Nothing
          _ -> loop query i pl
  loop "" 0 0

-- | Directory browser. Enter or Right descends, Left or Backspace goes up,
-- Enter on a file picks it.
askFile :: String -> Int -> IO (Maybe String)
askFile start viewHeight = withAskTty (loop start 0 0)
  where
    loop dir idx prev = do
      cwd <- canonicalizePath dir
      listEntries cwd >>= \case
        Left err -> askCommit (dimText (T.pack err)) prev >> pure Nothing
        Right entries -> do
          let count  = length entries
              i      = max 0 (min idx (count - 1))
              target = entries !! i
              enter  = canonicalizePath (cwd <> "/" <> feName target)
          pl <- askRepaint (renderFileFrame cwd entries i viewHeight) prev
          readKey >>= \case
            KeyEnter
              | feIsDir target -> enter >>= \p -> loop p 0 pl
              | otherwise -> do
                  full <- enter
                  askCommit (wrapAnsi ColorCyan (T.pack full)) pl
                  pure (Just full)
            KeyRight
              | feIsDir target -> enter >>= \p -> loop p 0 pl
              | otherwise      -> loop cwd i pl
            KeyUp        -> loop cwd ((i - 1 + count) `mod` count) pl
            KeyDown      -> loop cwd ((i + 1) `mod` count) pl
            KeyTab       -> loop cwd ((i + 1) `mod` count) pl
            KeyLeft      -> up cwd i pl
            KeyBackspace -> up cwd i pl
            k | isQuitKey k -> askErase pl >> pure Nothing
            _ -> loop cwd i pl
    up cwd i pl = do
      parent <- canonicalizePath (cwd <> "/..")
      if parent /= cwd then loop parent 0 pl else loop cwd i pl

-- | Directories first, then case-insensitive by name, with @..@ on top.
listEntries :: String -> IO (Either String [FileEntry])
listEntries path = do
  exists <- doesPathExist path
  isDir  <- doesDirectoryExist path
  if not exists then pure (Left ("not found: " <> path))
  else if not isDir then pure (Left ("not a directory: " <> path))
  else do
    names <- listDirectory path `catch` \(_ :: IOException) -> pure []
    entries <- forM names $ \nm -> FileEntry nm <$> doesDirectoryExist (path <> "/" <> nm)
    pure (Right (FileEntry ".." True : sortOn (\e -> (not (feIsDir e), map toLower (feName e))) entries))

-- | Scrollable pager; @viewHeight@ 0 fills the terminal. q or Esc quits.
askPager :: String -> Int -> Bool -> IO ()
askPager contentS viewHeight lineNumbers = withAskTty $ do
  let content = T.pack contentS
      ls = if T.null content then [""] else splitOnChar '\n' content
  (termH, termW) <- getTerminalSize
  let h      = if viewHeight > 0 then viewHeight else max 5 (termH - 3)
      maxTop = max 0 (length ls - h)
      loop top prev = do
        let t = max 0 (min top maxTop)
        pl <- askRepaint (renderPagerFrame ls t h termW lineNumbers) prev
        readKey >>= \case
          KeyChar 'q' -> askErase pl
          KeyEscape   -> askErase pl
          KeyCtrl 'C' -> askErase pl
          KeyUp       -> loop (t - 1) pl
          KeyDown     -> loop (t + 1) pl
          KeyEnter    -> loop (t + 1) pl
          KeyPageUp   -> loop (t - h) pl
          KeyPageDown -> loop (t + h) pl
          KeyChar ' ' -> loop (t + h) pl
          KeyHome     -> loop 0 pl
          KeyEnd      -> loop maxTop pl
          _           -> loop t pl
  loop 0 0
