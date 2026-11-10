{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}

{- |
Module      : Layoutz
Description : Friendly, expressive print-layout DSL for Haskell
Copyright   : (c) 2026 Matthieu Court
License     : Apache-2.0

Compositional terminal layouts, charts, prompts and a small Elm-style TUI runtime.
-}

module Layoutz
  ( -- * Views
    View
  , render
  , renderIn
  , width
  , height
    -- * Content
  , text
  , display
  , br
  , vstack
  , row
  , box
  , banner
  , statusCard
  , ul
  , ol
  , table
  , section
  , kv
  , tree
  , leaf
  , branch
  , Tree(..)
  , hr
  , vr
  , inlineBar
  , chart
    -- * Attributes
    -- | Apply with @&@ or as plain functions: @box "T" xs & border BorderRound@.
  , fg
  , Color(..)
  , styleBold
  , styleDim
  , styleItalic
  , styleUnderline
  , styleBlink
  , styleReverse
  , styleHidden
  , styleStrikethrough
  , border
  , Border(..)
  , gap
  , glyph
  , size
  , accent
  , ellipsis
  , underline
  , center
  , centerIn
  , pad
  , margin
  , clip
  , fill
  , sized
  , fixedWidth
  , fixedHeight
  , alignLeft
  , alignRight
  , alignCenter
  , justify
  , wrap
  , (&)
    -- * Events
  , onClick
  , onScroll
  , onKey
  , button
  , focusable
  , focusStyle
  , clickAt
  , scrollAt
  , pressKey
  , focusOrder
    -- * Widgets
  , Viewport
  , viewport
  , viewportTop
  , viewportBottom
  , SelectList
  , selectList
  , selectListOf
  , selectedItem
    -- * Spinners
  , spinner
  , SpinnerStyle(..)
    -- * Plots
  , plotSparkline
  , Series(..)
  , plotLine
  , Slice(..)
  , plotPie
  , BarItem(..)
  , plotBar
  , StackedBarGroup(..)
  , plotStackedBar
  , HeatmapData(..)
  , plotHeatmap
  , plotHeatmap'
    -- * Apps
  , LayoutzApp(..)
  , sandbox
  , run
  , runApp
  , runInline
  , Options(..)
  , defaultOptions
  , AppAlignment(..)
  , Key(..)
    -- ** Terminal input
  , Input(..)
  , MouseEvent(..)
  , parseInput
  , Cmd
  , cmdNone
  , cmdFire
  , cmdTask
  , cmdAfterMs
  , cmdQuit
  , cmdFocus
  , executeCmd
  , Sub
  , subNone
  , subKeyPress
  , subEveryMs
  , subAnimationFrame
  , subFocus
    -- * Kitty graphics
  , KittyImage(..)
  , kittyImage
  , kittyRGB
  , kittyRGBA
  , kittyImageFile
    -- * Progress loaders
  , LoaderStyle(..)
  , loaderBlocks
  , loaderBar
  , loaderAscii
  , loaderDots
  , loaderLine
  , loaderPipes
  , loaderStyles
  , loader
  , loaderStyled
  , loaderStream
  , loaderStreamStyled
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

import Data.List (transpose, nub, sortOn, intercalate, elemIndex)
import Data.Text (type Text, pattern (:<), pattern Empty)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR, bit, setBit, testBit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, mapMaybe, isJust, isNothing)
import Data.Function ((&))
import Control.Applicative ((<|>))
import Data.Word (Word8, Word64)
import GHC.Arr (Array, listArray, unsafeAt)
import Data.String (IsString(..))
import Data.Char (ord, chr, toLower, isDigit)
import Text.Printf (printf)
import System.IO
#if !defined(mingw32_HOST_OS)
import System.Posix.Signals (installHandler, Handler(..))
import System.Posix.Signals.Exts (sigWINCH)
#endif
import Control.Exception (catch, finally, throwIO, AsyncException(..), IOException, SomeException)
import System.Timeout (timeout)
import Control.Monad (when, unless, forever, forM, forM_, void)
import Control.Concurrent (forkIO, threadDelay, killThread, newChan, writeChan, readChan)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, newEmptyMVar, takeMVar, tryTakeMVar, tryPutMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef')
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
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

padRight :: Int -> Text -> Text
padRight w str = str <> T.replicate (max 0 (w - visibleLength str)) " "

padLeft :: Int -> Text -> Text
padLeft w str = T.replicate (max 0 (w - visibleLength str)) " " <> str

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

-- | Anything that draws itself as text: the leaves of a 'View'.
class Element a where
  renderElement :: a -> Text

render :: View m -> String
render = T.unpack . blockText . drawView natural

-- | Render with @w@ x @h@ cells of room, the way an app's root is laid out:
-- 'fill' takes what's left.
renderIn :: Int -> Int -> View m -> String
renderIn w h = T.unpack . blockText . drawView (Room (Just w) (Just h) False)

-- | Widest visible line.
width :: View m -> Int
width = blockWidth . drawView natural

-- | Line count (an empty view still takes one line).
height :: View m -> Int
height v = case trimmed (bRows (drawView natural v)) of
  [] -> 1
  rs -> length rs

-- | The layout tree. @msg@ is what its handlers send ('onClick', 'onScroll');
-- a view without handlers fits any @msg@, so write helpers as @Model -> View m@.
--
-- Constructors take content; everything else is an attribute you apply with
-- @&@ (or plain function application):
--
-- @
-- 'vstack'
--   [ 'text' "Status" & 'styleBold' & 'underline'
--   , 'box' "Deploy" ['inlineBar' "build" 0.8] & 'border' BorderRound & 'fg' ColorGreen
--   , 'row' ['button' "-" Dec, 'display' n, 'button' "+" Inc]
--   ]
-- @
data View msg
  = forall a. Element a => Leaf a
  | Stack [View msg]
  | Row Int [View msg]                    -- gap
  | Margin Text (View msg)
  | Section Text Text Int [View msg]      -- title, glyph, flank width
  | Rule Axis Text Int                    -- glyph, length
  | Underline Text (Maybe Color) (View msg)
  | Clip Int Text (View msg)              -- max width, ellipsis
  | Pad Int (View msg)
  | Aligned Alignment Int (View msg)
  | Bullets [View msg]
  | Numbered [View msg]
  | AutoCenter (View msg)
  | Colored Color (View msg)
  | Styled Style (View msg)
  | Box Text [View msg] Border
  | StatusCard Text Text Border
  | Table [Text] [[View msg]] Border
  | Mapped (Text -> Text) (View msg)      -- reflow the child's text
  | OnEvent (Handlers msg) (View msg)
  | Fill (View msg)
  | Fixed (Maybe Int) (Maybe Int) (View msg)
  | Focusable Text (View msg)
  | WhenFocused (forall n. View n -> View n) (View msg)
  | Scrolled (Viewport -> msg) Viewport (View msg)
  | Following Int (View msg)               -- keep this row in sight
  | Sized (Int -> Int -> View msg)         -- built from the room it gets

data Axis = Horizontal | Vertical

instance Show (View m) where
  show = render

-- | Lets you write @layout [\"Hello\", \"World\"]@ under @OverloadedStrings@.
instance IsString (View m) where
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

data Alignment = AlignLeft | AlignRight | AlignCenter | Justify
  deriving (Show, Eq)

renderStatusCard :: Text -> Text -> Border -> Text
renderStatusCard label content bdr =
  T.intercalate "\n" $ [topBorder] <> map cardLine allLines <> [bottomBorder]
  where
    allLines = T.lines label <> T.lines content
    inner    = maximum (0 : map visibleLength allLines) + 2
    BorderChars{..} = borderChars bdr
    topBorder    = bcTL <> T.replicate (inner + 2) (T.take 1 bcHTop) <> bcTR
    bottomBorder = bcBL <> T.replicate (inner + 2) (T.take 1 bcHBottom) <> bcBR
    cardLine l   = bcVLeft <> " " <> padRight inner l <> " " <> bcVRight

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

data InlineBar = InlineBar Text Double
instance Element InlineBar where
  renderElement (InlineBar label progress) =
    let p      = max 0.0 (min 1.0 progress)
        filled = floor (p * 20)
        bar    = T.replicate filled "█" <> T.replicate (20 - filled) "─"
    in label <> " [" <> bar <> "] " <> T.show (floor (p * 100) :: Int) <> "%"

--------------------------------------------------------------------------------
-- Drawing
--
-- A view draws into a block: rows of cells, the click regions inside them, and
-- any kitty uploads to send ahead of the grid. Rows are ragged like lines of
-- text, and containers pad exactly where the old text renderer did, so
-- 'render' prints the same thing it always has.

data Attr = Attr { aStyle :: !Int, aFg :: !Text, aBg :: !Text }  -- SGR style bits, colour params

-- Cells and attrs are compared for every cell of every frame (diffing, pen
-- changes, recolouring), and most of them are shared: 'charText' for the
-- text, one 'Attr' per run. Checking for the very same object first skips
-- the byte comparison for nearly all of them.
instance Eq Attr where
  a@(Attr s f b) == a'@(Attr s' f' b') = same a a' || (s == s' && sameText f f' && sameText b b')

sameText :: Text -> Text -> Bool
sameText t t' = same t t' || t == t'
{-# INLINE sameText #-}

-- | Pointer equality, only ever a shortcut: 'False' says nothing.
same :: a -> a -> Bool
same x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE same #-}

plain :: Attr
plain = Attr 0 "" ""

-- | One terminal column. The right half of a wide character is @Cell ""@.
data Cell = Cell !Text !Attr

instance Eq Cell where
  Cell t a == Cell t' a' = sameText t t' && a == a'

type Cells = [Cell]

data Rect = Rect !Int !Int !Int !Int   -- top, left, height, width

data Block m = Block
  { bRows :: [Cells]
  , bHits :: [(Rect, Handlers m)]      -- outermost first
  , bRaw  :: [Text]
  }

-- | Room a view may grow into. Views keep their natural size unless they sit
-- under 'fill' or 'fixedWidth' / 'fixedHeight', which set 'roomStretch'.
data Room = Room { roomW, roomH :: !(Maybe Int), roomStretch :: !Bool }

natural :: Room
natural = Room Nothing Nothing False

blank :: Cell
blank = Cell " " plain

rowsBlock :: [Cells] -> Block m
rowsBlock rs = Block rs [] []

rowsWidth :: [Cells] -> Int
rowsWidth = maximum . (0 :) . map length

blockWidth :: Block m -> Int
blockWidth = rowsWidth . bRows

-- | What 'T.lines' would have seen: a trailing empty row goes.
trimmed :: [Cells] -> [Cells]
trimmed rs
  | not (null rs), null (last rs) = init rs
  | otherwise                     = rs

padTo :: Int -> Cells -> Cells
padTo w r
  | n <= 0    = r   -- already wide enough: share the row instead of copying it
  | otherwise = r <> replicate n blank
  where n = w - length r

moveHits :: Int -> Int -> [(Rect, h)] -> [(Rect, h)]
moveHits dy dx = map (\(Rect t l h w, x) -> (Rect (t + dy) (l + dx) h w, x))

-- | Stack blocks top to bottom.
vcat :: [Block m] -> Block m
vcat bs = Block (concatMap bRows bs) hits (concatMap bRaw bs)
  where
    tops = scanl (+) 0 (map (length . bRows) bs)
    hits = concat (zipWith (\t b -> moveHits t 0 (bHits b)) tops bs)

cellsOf :: Attr -> Text -> Cells
cellsOf a = reverse . pushText a []

-- | Add text to a row kept in reverse: combining marks join the cell before,
-- wide characters take two.
pushText :: Attr -> Cells -> Text -> Cells
pushText a = T.foldl' step
  where
    step acc c = case charWidth c of
      0 | Cell "" r : Cell t a' : rest <- acc -> Cell "" r : Cell (T.snoc t c) a' : rest  -- onto the wide character, not its right half
        | Cell t a' : rest <- acc -> Cell (T.snoc t c) a' : rest
        | otherwise               -> acc
      2 -> Cell "" a : Cell (charText c) a : acc
      _ -> Cell (charText c) a : acc

-- | A one-character 'Text', shared for ASCII, box drawing and block elements so
-- drawing a row doesn't allocate a fresh string per cell.
charText :: Char -> Text
charText c
  | i < 0x80                  = asciiTexts `unsafeAt` i
  | i >= 0x2500 && i < 0x25A0 = boxTexts `unsafeAt` (i - 0x2500)
  | otherwise                 = T.singleton c
  where i = ord c

asciiTexts, boxTexts :: Array Int Text
asciiTexts = listArray (0, 0x7F) [ T.singleton (chr n) | n <- [0 .. 0x7F] ]
boxTexts   = listArray (0, 0x9F) [ T.singleton (chr n) | n <- [0x2500 .. 0x259F] ]

-- | Read text the way a terminal would: SGR moves the pen, kitty uploads are
-- set aside, any other escape is dropped.
textBlock :: Text -> Block m
textBlock = go plain [] [] . T.splitOn "\n"
  where
    go _ rows raw [] = Block (reverse rows) [] (reverse raw)
    go a rows raw (l : ls) =
      let (line, a', raw') = scan a [] raw l
      in go a' (reverse line : rows) raw' ls

    scan a acc raw s =
      let (chunk, rest) = T.break (== '\ESC') s
          acc'          = pushText a acc chunk
      in case rest of
           '\ESC' :< '[' :< more ->
             let (params, after) = T.span (\c -> c < '@' || c > '~') more
             in case after of
                  'm' :< after' -> scan (applySgr a params) acc' raw after'
                  _ :< after'   -> scan a acc' raw after'
                  Empty         -> (acc', a, raw)
           '\ESC' :< '_' :< more ->
             let (body, after) = T.breakOn kittyST more
                 raw' | "G" `T.isPrefixOf` body = ("\ESC_" <> body <> kittyST) : raw
                      | otherwise               = raw
             in scan a acc' raw' (T.drop (T.length kittyST) after)
           '\ESC' :< _ :< more -> scan a acc' raw more
           _                   -> (acc', a, raw)

applySgr :: Attr -> Text -> Attr
applySgr a0 = go a0 . map param . T.splitOn ";"
  where
    param t
      | not (T.null t), T.all isDigit t = T.foldl' (\n c -> n * 10 + ord c - 48) 0 t
      | otherwise                       = 0 :: Int
    rgb k r g b = T.intercalate ";" (map T.show [k, 2, r, g, b])

    go a [] = a
    go _ (0 : ps)                  = go plain ps
    go a (38 : 5 : n : ps)         = go a { aFg = "38;5;" <> T.show n } ps
    go a (38 : 2 : r : g : b : ps) = go a { aFg = rgb 38 r g b } ps
    go a (48 : 5 : n : ps)         = go a { aBg = "48;5;" <> T.show n } ps
    go a (48 : 2 : r : g : b : ps) = go a { aBg = rgb 48 r g b } ps
    go a (n : ps)
      | n >= 1 && n <= 9                         = go a { aStyle = setBit (aStyle a) n } ps
      | n >= 30 && n <= 37 || n >= 90 && n <= 97 = go a { aFg = T.show n } ps
      | n >= 40 && n <= 47 || n >= 100 && n <= 107 = go a { aBg = T.show n } ps
      | n == 39                                  = go a { aFg = "" } ps
      | n == 49                                  = go a { aBg = "" } ps
      | otherwise                                = go a ps

-- | A row as text, switching the pen only where the attributes change.
rowText :: Cells -> Text
rowText = T.concat . go plain
  where
    go a [] = [ "\ESC[0m" | a /= plain ]
    go a (Cell t a' : cs)
      | a' == a   = t : go a cs
      | otherwise = [ "\ESC[0m" | a /= plain ] <> [pen a', t] <> go a' cs

    pen (Attr st f b) = sgr (T.intercalate ";" [ T.show n | n <- [1 .. 9 :: Int], testBit st n ]) <> sgr f <> sgr b
    sgr p = if T.null p then "" else "\ESC[" <> p <> "m"

-- | What turns a screen showing @old@ into one showing @new@ (1-based rows
-- from the top): each run of changed cells behind one cursor move, with short
-- unchanged gaps rewritten rather than jumped over, and leftover cells cleared.
-- Assumes the screen is blank past the end of each old row.
diffRows :: [Cells] -> [Cells] -> Text
diffRows old new = T.concat (zipWith3 diffRow [1 ..] (upTo old) (upTo new))
  where
    upTo rs = take (max (length old) (length new)) (rs <> repeat [])

diffRow :: Int -> Cells -> Cells -> Text
diffRow i old new = foldMap draw (runs (changedAt 0 new old)) <> clearTail
  where
    newLen = length new

    -- columns where @new@ differs from @old@, in one pass over both
    changedAt :: Int -> Cells -> Cells -> [Int]
    changedAt c (x : xs) (o : os)
      | x == o    = c `seq` changedAt (c + 1) xs os
      | otherwise = c : changedAt (c + 1) xs os
    changedAt c xs [] = take (length xs) [c ..]
    changedAt _ [] _  = []

    -- a cursor move costs about as much as rewriting a few cells
    runs []       = []
    runs (c : cs) = go c c cs
      where
        go a b (d : ds)
          | d - b <= 5 = go a d ds
          | otherwise  = (a, b) : go d d ds
        go a b [] = [(a, b)]

    draw (a, b) =
      let a' = startAt a
      in goto (a' + 1) <> rowText (take (b - a' + 1) (drop a' new))

    -- Never start on the right half of a wide character, or on a kitty
    -- placeholder that takes its image position from the cell before it.
    startAt a
      | a > 0, Cell t _ : _ <- drop a new, T.null t || t == T.singleton kittyPlaceholderChar = startAt (a - 1)
      | otherwise = a

    clearTail
      | length old > newLen = goto (newLen + 1) <> "\ESC[K"
      | otherwise           = ""

    goto c = "\ESC[" <> T.show i <> ";" <> T.show c <> "H"

-- | Every row in full, then whatever is left of the screen cleared. For after
-- a resize, when nothing on screen can be trusted. Unlike clearing first, a
-- terminal that paints mid-frame (no synchronized updates) shows the old
-- frame under the new one rather than blank rows.
repaintRows :: Int -> Int -> [Cells] -> Text
repaintRows termW termH rows =
  T.concat [ goto i <> rowText r <> clearRight r | (i, r) <- zip [1 ..] rows ]
    <> (if length rows < termH then goto (length rows + 1) <> "\ESC[J" else "")
  where
    goto i = "\ESC[" <> T.show (i :: Int) <> ";1H"
    -- not on a full row: the cursor sits on the last cell and would erase it
    clearRight r = if length r < termW then "\ESC[K" else ""

blockText :: Block m -> Text
blockText b = T.concat (bRaw b) <> T.intercalate "\n" (map rowText (bRows b))

recolor :: (Attr -> Attr) -> Block m -> Block m
recolor f b = b { bRows = map (go plain (f plain)) (bRows b) }
  where
    -- runs of cells share an Attr: compute it once per run, not per cell
    go _ _ [] = []
    go i o (Cell t a : cs)
      | a == i    = Cell t o : go i o cs
      | otherwise = let o' = f a in Cell t o' : go a o' cs

styleBits :: Style -> Int
styleBits = \case
  StyleDefault       -> 0
  StyleBold          -> bit 1
  StyleDim           -> bit 2
  StyleItalic        -> bit 3
  StyleUnderline     -> bit 4
  StyleBlink         -> bit 5
  StyleReverse       -> bit 7
  StyleHidden        -> bit 8
  StyleStrikethrough -> bit 9
  StyleCombined ss   -> foldr ((.|.) . styleBits) 0 ss

drawView :: Room -> View m -> Block m
drawView room = \case
  Leaf x            -> textBlock (renderElement x)
  Stack xs          -> drawStack room xs
  Row g xs          -> drawRow room g xs
  Margin p v        ->
    let b   = drawView natural v
        pre = cellsOf plain (p <> " ")
    in b { bRows = map (pre <>) (trimmed (bRows b)), bHits = moveHits 0 (length pre) (bHits b) }
  Section t g n xs  ->
    let flank = T.replicate n (T.take 1 g)
    in vcat [rowsBlock [cellsOf plain (flank <> " " <> t <> " " <> flank)], drawStack natural xs]
  Rule h g n        -> drawRule room h g n
  Underline g c v   -> drawUnderline g c (drawView natural v)
  Clip n e v        -> drawClip n e (drawView natural v)
  Pad p v           -> drawPad p (drawView natural v)
  Aligned a w v     -> alignBlock a w (drawView natural v)
  Bullets xs        -> drawBullets xs
  Numbered xs       -> drawNumbered xs
  AutoCenter v      -> drawView room v  -- the enclosing stack does the centring
  Colored c v       -> recolor (\a -> if T.null (aFg a) then a { aFg = colorCode c } else a) (drawView room v)
  Styled st v       -> recolor (\a -> a { aStyle = aStyle a .|. styleBits st }) (drawView room v)
  Box t xs b        -> drawBox room t xs b
  StatusCard l c b  -> textBlock (renderStatusCard l c b)
  Table hs rs b     -> drawTable hs rs b
  Mapped f v        -> textBlock (f (blockText (drawView natural v)))
  OnEvent h v       ->
    let b = drawView room v
    in b { bHits = (Rect 0 0 (length (bRows b)) (blockWidth b), h { hScope = hScope h <|> focusName v }) : bHits b }
  Focusable n v     ->
    let b    = drawView room v
        own  = (Rect 0 0 (length (bRows b)) (blockWidth b), noHandlers { hFocus = Just n, hScope = Just n })
        mine = [ (r, if isNothing (hScope h) then h { hScope = Just n } else h) | (r, h) <- bHits b ]
    in b { bHits = own : mine }
  WhenFocused _ v   -> drawView room v  -- 'resolveFocus' already applied the focused ones
  Scrolled k vp v   -> drawScrolled room k vp v
  Following i v     ->
    let (inner, h, w) = windowed room v
    in window (max 0 (min (length (bRows inner) - h) (i - h `div` 2))) h w inner
  Fill v            -> claim room { roomStretch = True } (drawView room { roomStretch = True } v)
  Fixed mw mh v     ->
    let r = Room (mw <|> roomW room) (mh <|> roomH room) True
    in crop mw mh (claim r (drawView r v))
  Sized k           -> drawView room (k (fromMaybe 80 (roomW room)) (fromMaybe 24 (roomH room)))

-- | Under 'fill', take the whole room even where nothing was drawn.
claim :: Room -> Block m -> Block m
claim (Room mw mh True) b = b { bRows = rows' }
  where
    w     = fromMaybe (blockWidth b) mw
    rows  = maybe id padTo mw `map` bRows b
    rows' = rows <> replicate (maybe 0 (subtract (length rows)) mh) (replicate w blank)
claim _ b = b

crop :: Maybe Int -> Maybe Int -> Block m -> Block m
crop mw mh b = b { bRows = maybe id take mh (map (maybe id take mw) (bRows b)) }

-- | The content of a scrolling view, the rows it has to show it in, and the
-- width its scrollbar goes after.
windowed :: Room -> View m -> (Block m, Int, Int)
windowed room v = (inner, h, max (blockWidth inner) (fromMaybe 0 innerW))
  where
    stretched = roomStretch room
    innerW    = if stretched then subtract 1 <$> roomW room else Nothing
    -- lines wider than the room are cut rather than pushing the parent out
    inner     = crop innerW Nothing (drawView (Room innerW Nothing False) v)
    h | stretched, Just rh <- roomH room = rh
      | otherwise                        = length (bRows inner)

-- | @h@ rows of a block starting at @top@, with a scrollbar after column @w@
-- if it doesn't fit.
window :: Int -> Int -> Int -> Block m -> Block m
window top h w b
  | total <= h = b
  | otherwise  = Block (zipWith (\r bar -> padTo w r <> [bar]) (take h (drop top rows) <> repeat []) bars) hits (bRaw b)
  where
    rows   = bRows b
    total  = length rows
    thumb  = max 1 (h * h `div` total)
    at     = top * (h - thumb) `div` max 1 (total - h)
    bars   = [ Cell (if i >= at && i < at + thumb then "█" else "│") plain | i <- [0 .. h - 1] ]
    hits   = [ (Rect t' l (b' - t') ww, x)
             | (Rect t l hh ww, x) <- bHits b
             , let t' = max 0 (t - top); b' = min h (t + hh - top)
             , b' > t' ]

drawScrolled :: Room -> (Viewport -> msg) -> Viewport -> View msg -> Block msg
drawScrolled room moved (Viewport off) v = b { bHits = own : bHits b }
  where
    (inner, h, w) = windowed room v
    maxTop = max 0 (length (bRows inner) - h)
    top    = max 0 (min maxTop off)
    b      = window top h w inner
    to n   = moved (Viewport (if n >= maxTop then maxBound else max 0 n))
    own    = (Rect 0 0 h (blockWidth b), noHandlers { hScroll = Just (\d -> to (top + 3 * d)), hKey = Just keys })
    keys = \case
      KeyUp       -> Just (to (top - 1))
      KeyDown     -> Just (to (top + 1))
      KeyPageUp   -> Just (to (top - h))
      KeyPageDown -> Just (to (top + h))
      KeyHome     -> Just (to 0)
      KeyEnd      -> Just (to maxTop)
      _           -> Nothing

-- | Children whose size is up to the parent: wrapped in 'fill', perhaps under
-- colour, style or a handler.
isFill :: View m -> Bool
isFill = \case
  Fill _          -> True
  Colored _ v     -> isFill v
  Styled _ v      -> isFill v
  OnEvent _ v     -> isFill v
  Focusable _ v   -> isFill v
  WhenFocused _ v -> isFill v
  _               -> False

-- | Draw children along one axis: the rest first, offered only the room
-- across it, then the 'fill' ones sharing whatever is left along it.
share :: (Block m -> Int) -> Int -> Maybe Int -> (Maybe Int -> Room) -> [View m] -> [Block m]
share extent gaps room roomFor xs = map draw' (zip xs fixed)
  where
    fixed  = [ if isFill x then Nothing else Just (drawView (roomFor Nothing) x) | x <- xs ]
    used   = sum [ extent b | Just b <- fixed ] + gaps
    nFills = length (filter isFill xs)
    each   = (\r -> max 0 (r - used) `div` max 1 nFills) <$> room
    draw' (x, Nothing) = drawView (roomFor each) x
    draw' (_, Just b)  = b

drawStack :: Room -> [View m] -> Block m
drawStack room xs = vcat (zipWith place xs blocks)
  where
    blocks = share (length . bRows) 0 (roomH room) (\h -> Room (roomW room) h False) xs
    placed = [ b | (x, b) <- zip xs blocks, not (isAutoCenter x) ]
    ctx | roomStretch room, Just w <- roomW room = w
        | null placed                            = 80
        | otherwise                              = maximum (map blockWidth placed)

    isAutoCenter (AutoCenter _) = True
    isAutoCenter _              = False

    place (AutoCenter _) b = alignBlock AlignCenter ctx b
    place _ b              = b

drawRow :: Room -> Int -> [View m] -> Block m
drawRow _ _ [] = rowsBlock []
drawRow room g xs = Block rows hits (concatMap bRaw blocks)
  where
    blocks = share blockWidth (g * (length xs - 1)) (roomW room) (\w -> Room w (roomH room) False) xs
    cols   = map (trimmed . bRows) blocks
    widths = map rowsWidth cols
    h      = maximum (map length cols)
    padded = zipWith (\w c -> map (padTo w) (take h (c <> repeat []))) widths cols
    rows   = map (intercalate (replicate g blank)) (transpose padded)
    lefts  = scanl (\x w -> x + w + g) 0 widths
    hits   = concat (zipWith (\x b -> moveHits 0 x (bHits b)) lefts blocks)

-- | Prefix the first row with @first@ and the others with @rest@.
hang :: Text -> Text -> Block m -> Block m
hang first rest b = Block rows (moveHits 0 (length firstCells) (bHits b)) (bRaw b)
  where
    firstCells = cellsOf plain first
    rows = case trimmed (bRows b) of
      []     -> [firstCells]
      r : rs -> (firstCells <> r) : map (cellsOf plain rest <>) rs

drawRule :: Room -> Axis -> Text -> Int -> Block m
drawRule room axis g n = rowsBlock $ case axis of
  Horizontal -> [cellsOf plain (T.replicate (stretched roomW) g)]
  Vertical   -> replicate (stretched roomH) (cellsOf plain g)
  where
    stretched along
      | roomStretch room = fromMaybe n (along room)
      | otherwise        = n

drawUnderline :: Text -> Maybe Color -> Block m -> Block m
drawUnderline g color b = b { bRows = bRows b <> [cellsOf pen rule] }
  where
    (reps, rest) = rowsWidth (trimmed (bRows b)) `divMod` T.length g
    rule = T.replicate reps g <> T.take rest g
    pen  = maybe plain (\c -> plain { aFg = colorCode c }) color

drawClip :: Int -> Text -> Block m -> Block m
drawClip n ell b = Block (map cut (trimmed (bRows b))) hits (bRaw b)
  where
    keep = n - T.length ell
    cut cs
      | length cs <= n = cs
      | keep <= 0       = cellsOf plain (T.take n ell)
      | otherwise       = whole (take keep cs) (drop keep cs) <> cellsOf plain ell
    -- don't leave half a wide character behind
    whole kept (Cell "" _ : _) = init kept <> [blank]
    whole kept _               = kept
    hits = [ (Rect t l h (min w (n - l)), x) | (Rect t l h w, x) <- bHits b, l < n ]

drawPad :: Int -> Block m -> Block m
drawPad p b = Block (border' <> map (\r -> side <> padTo w r <> side) rows <> border') (moveHits p p (bHits b)) (bRaw b)
  where
    rows    = trimmed (bRows b)
    w       = rowsWidth rows
    side    = replicate p blank
    border' = replicate p (replicate (w + 2 * p) blank)

alignBlock :: Alignment -> Int -> Block m -> Block m
alignBlock a w b = Block (zipWith (\l r -> lead l <> align r) lefts rows) hits (bRaw b)
  where
    rows  = trimmed (bRows b)
    lefts = map leftPad rows
    leftPad r
      | length r >= w = 0
      | otherwise = case a of
          AlignRight  -> w - length r
          AlignCenter -> (w - length r) `div` 2
          _           -> 0
    lead l = replicate l blank
    align r = case a of
      AlignLeft   -> padTo w r
      AlignRight  -> r
      AlignCenter -> padTo (w - leftPad r) r
      Justify     -> case bRows (textBlock (justifyString w (rowText r))) of
                       r' : _ -> r'
                       []     -> r
    hits = [ (Rect t (l + dx) h ww, x) | (Rect t l h ww, x) <- bHits b, let dx = fromMaybe 0 (lookup t (zip [0 ..] lefts)) ]

drawBox :: Room -> Text -> [View m] -> Border -> Block m
drawBox room title xs bdr = Block (cellsOf plain top : body <> [cellsOf plain bottom]) hits (concatMap bRaw blocks)
  where
    inner  | roomStretch room = Room (subtract 4 <$> roomW room) (subtract 2 <$> roomH room) False
           | otherwise        = natural
    blocks = share (length . bRows) 0 (roomH inner) (\h -> Room (roomW inner) h False) xs
    rows   = case trimmed (concatMap bRows blocks) of
      [] -> [[]]
      rs -> rs
    content = rows <> replicate (maybe 0 (subtract (length rows)) (roomH inner)) []
    titleW  = if T.null title then 0 else visibleLength title + 2
    innerW  = maximum [rowsWidth rows, titleW, fromMaybe 0 (roomW inner)]
    totalW  = innerW + 4
    BorderChars{..} = borderChars bdr
    hTop    = T.take 1 bcHTop
    hBottom = T.take 1 bcHBottom

    top
      | T.null title = bcTL <> T.replicate (totalW - 2) hTop <> bcTR
      | otherwise =
          let slack = totalW - visibleLength title - 2
              l     = slack `div` 2
          in bcTL <> T.replicate l hTop <> title <> T.replicate (slack - l) hTop <> bcTR
    bottom = bcBL <> T.replicate (totalW - 2) hBottom <> bcBR
    body   = map (\r -> cellsOf plain (bcVLeft <> " ") <> padTo innerW r <> cellsOf plain (" " <> bcVRight)) content

    tops = scanl (+) 0 (map (length . bRows) blocks)
    hits = concat (zipWith (\t b -> moveHits (1 + t) 2 (bHits b)) tops blocks)

drawTable :: [Text] -> [[View m]] -> Border -> Block m
drawTable headers rows bdr =
  Block (map (cellsOf plain) [topBorder, headerRow, separator] <> concat dataRows <> [cellsOf plain bottomBorder])
        hits (concatMap bRaw (concat cells))
  where
    cells  = map (map (drawView natural) . fitRow (length headers)) rows
    cellRows = map (map (trimmed . bRows)) cells
    widths = map (maximum . (0 :)) (transpose (map visibleLength headers : map (map rowsWidth) cellRows))
    BorderChars{..} = borderChars bdr
    hTop    = T.take 1 bcHTop
    hBottom = T.take 1 bcHBottom

    rule l mid r h = l <> h <> T.intercalate (h <> mid <> h) [T.replicate w h | w <- widths] <> h <> r
    topBorder    = rule bcTL bcTopTee bcTR hTop
    separator    = rule bcLeftTee bcCross bcRightTee hTop
    bottomBorder = rule bcBL bcBottomTee bcBR hBottom

    headerRow = bcVLeft <> " " <> T.intercalate (" " <> bcVLeft <> " ") (zipWith padRight widths headers) <> " " <> bcVRight
    line cs   = cellsOf plain (bcVLeft <> " ") <> intercalate (cellsOf plain (" " <> bcVLeft <> " ")) cs
                <> cellsOf plain (" " <> bcVRight)

    fitRow n cs
      | length cs >= n = take n cs
      | otherwise      = cs <> replicate (n - length cs) (text "")

    -- a row is as tall as its tallest cell
    heights  = [ maximum (1 : map length r) | r <- cellRows ]
    dataRows = [ map line (transpose (zipWith (\w c -> map (padTo w) (take h (c <> repeat []))) widths r))
               | (h, r) <- zip heights cellRows ]

    lefts = scanl (\x w -> x + w + 3) 2 widths
    tops  = scanl (+) 3 heights
    hits  = concat [ moveHits t l (bHits b) | (t, rowCells) <- zip tops cells, (l, b) <- zip lefts rowCells ]

drawBullets :: [View m] -> Block m
drawBullets = go 0
  where
    bullets = ["•", "◦", "▪"]
    go level = vcat . map item
      where
        bullet = bullets !! (level `mod` length bullets)
        indent = T.replicate (level * 2) " "
        item (Bullets nested) = go (level + 1) nested
        item x                = hangMarker indent (bullet <> " ") (drawView natural x)

drawNumbered :: [View m] -> Block m
drawNumbered = go (0 :: Int)
  where
    go level = vcat . zipWith item [1 ..]
      where
        indent = T.replicate (level * 2) " "
        item _ (Numbered nested) = go (level + 1) nested
        item n x                 = hangMarker indent (marker n <> ". ") (drawView natural x)

        marker n = case level `mod` 3 of
          0 -> T.show n
          1 -> T.singleton (toEnum (96 + n))
          _ -> roman n

    roman :: Int -> Text
    roman = \case
      1  -> "i";   2  -> "ii";  3  -> "iii"; 4  -> "iv"; 5  -> "v"
      6  -> "vi";  7  -> "vii"; 8  -> "viii"; 9  -> "ix"; 10 -> "x"
      n  -> T.show n

hangMarker :: Text -> Text -> Block m -> Block m
hangMarker indent marker = hang (indent <> marker) (indent <> T.replicate (T.length marker) " ")

--------------------------------------------------------------------------------
-- Content

text :: String -> View m
text = Leaf . T.pack

-- | @text . show@
display :: Show a => a -> View m
display = text . show

br :: View m
br = Leaf LineBreak

-- | Top to bottom.
vstack :: [View m] -> View m
vstack = Stack

-- | Left to right, one space apart; change it with 'gap'.
--
-- @
-- 'row' ['vstack' ["A", "B"], 'vstack' ["C", "D"]] & 'gap' 2
-- -- A  C
-- -- B  D
-- @
row :: [View m] -> View m
row = Row 1

box :: String -> [View m] -> View m
box title elements = Box (T.pack title) elements BorderNormal

-- | Titleless box with a double border.
--
-- @
-- 'banner' ["System Dashboard"]
-- -- ╔══════════════════╗
-- -- ║ System Dashboard ║
-- -- ╚══════════════════╝
-- @
banner :: [View m] -> View m
banner content = Box "" content BorderDouble

statusCard :: String -> String -> View m
statusCard label content = StatusCard (T.pack label) (T.pack content) BorderNormal

ul :: [View m] -> View m
ul = Bullets

ol :: [View m] -> View m
ol = Numbered

table :: [String] -> [[View m]] -> View m
table headers rows = Table (map T.pack headers) rows BorderNormal

-- | @=== title ===@ over its content; see 'glyph' and 'size'.
section :: String -> [View m] -> View m
section title = Section (T.pack title) "=" 3

kv :: [(String, String)] -> View m
kv pairs = Leaf (KeyValue [ (T.pack k, T.pack v) | (k, v) <- pairs ])

tree :: String -> [Tree] -> View m
tree name children = Leaf (Tree (T.pack name) children)

leaf :: String -> Tree
leaf name = Tree (T.pack name) []

branch :: String -> [Tree] -> Tree
branch name = Tree (T.pack name)

-- | 50 wide; see 'glyph' and 'size'.
hr :: View m
hr = Rule Horizontal "─" 50

-- | 10 tall; see 'glyph' and 'size'.
vr :: View m
vr = Rule Vertical "│" 10

inlineBar :: String -> Double -> View m
inlineBar label progress = Leaf (InlineBar (T.pack label) progress)

-- | Horizontal bar chart.
chart :: [(String, Double)] -> View m
chart points = Leaf (Chart [ (T.pack label, v) | (label, v) <- points ])

--------------------------------------------------------------------------------
-- Attributes
--
-- Each is a @View m -> View m@. Ones that tune a setting ('gap', 'glyph',
-- 'size', 'accent', 'ellipsis', 'border') adjust the nearest node that has
-- it, looking through colour, style and handlers, and leave anything else
-- alone.

adjust :: (View m -> Maybe (View m)) -> View m -> View m
adjust f v = fromMaybe inward (f v)
  where
    inward = case v of
      Colored c x  -> Colored c (adjust f x)
      Styled st x  -> Styled st (adjust f x)
      OnEvent h x  -> OnEvent h (adjust f x)
      AutoCenter x -> AutoCenter (adjust f x)
      Fill x       -> Fill (adjust f x)
      Fixed w h x  -> Fixed w h (adjust f x)
      Focusable n x   -> Focusable n (adjust f x)
      WhenFocused t x -> WhenFocused t (adjust f x)
      _            -> v

fg :: Color -> View m -> View m
fg = Colored

styled :: Style -> View m -> View m
styled st (Styled st' v) = Styled (st' <> st) v
styled st v              = Styled st v

styleBold, styleDim, styleItalic, styleUnderline, styleBlink, styleReverse, styleHidden, styleStrikethrough
  :: View m -> View m
styleBold          = styled StyleBold
styleDim           = styled StyleDim
styleItalic        = styled StyleItalic
styleUnderline     = styled StyleUnderline
styleBlink         = styled StyleBlink
styleReverse       = styled StyleReverse
styleHidden        = styled StyleHidden
styleStrikethrough = styled StyleStrikethrough

-- | For 'box', 'banner', 'statusCard' and 'table'.
border :: Border -> View m -> View m
border b = adjust $ \case
  Box t xs _       -> Just (Box t xs b)
  StatusCard l c _ -> Just (StatusCard l c b)
  Table hs rs _    -> Just (Table hs rs b)
  _                 -> Nothing

-- | Space between 'row' items.
gap :: Int -> View m -> View m
gap n = adjust $ \case
  Row _ xs -> Just (Row n xs)
  _           -> Nothing

-- | The character drawn by 'hr', 'vr', 'underline' and 'section'.
glyph :: String -> View m -> View m
glyph g = adjust $ \case
  Rule h _ n       -> Just (Rule h g' n)
  Underline _ c x  -> Just (Underline g' c x)
  Section t _ n xs -> Just (Section t g' n xs)
  _                 -> Nothing
  where g' = T.pack g

-- | Length of an 'hr' or 'vr', or the flanks of a 'section'.
size :: Int -> View m -> View m
size n = adjust $ \case
  Rule h g _       -> Just (Rule h g n)
  Section t g _ xs -> Just (Section t g n xs)
  _                 -> Nothing

-- | Colour of an 'underline' rule, leaving the text alone.
accent :: Color -> View m -> View m
accent c = adjust $ \case
  Underline g _ x -> Just (Underline g (Just c) x)
  _                -> Nothing

-- | What 'clip' appends, @...@ by default.
ellipsis :: String -> View m -> View m
ellipsis e = adjust $ \case
  Clip n _ x -> Just (Clip n (T.pack e) x)
  _           -> Nothing

-- | A rule under the content, as wide as it is.
underline :: View m -> View m
underline = Underline "─" Nothing

-- | Centre against the widest sibling in the enclosing 'vstack'.
center :: View m -> View m
center = AutoCenter

centerIn :: Int -> View m -> View m
centerIn = Aligned AlignCenter

pad :: Int -> View m -> View m
pad = Pad

-- | Take all the room the parent offers: the leftover width of a 'row',
-- the leftover height of a 'vstack', or the terminal for an app's root.
-- Boxes and rules stretch to it; other views keep their size inside it.
--
-- @
-- 'row' ['box' "nav" navItems & 'fixedWidth' 20, 'box' "main" content & 'fill'] & 'fill'
-- @
fill :: View m -> View m
fill = Fill

-- | Build a view from the width and height it is given, for things drawn to
-- a size such as plots. Under 'fill' that is the room the parent offers, so
-- it redraws as the terminal is resized; with no room to go by it gets 80x24.
--
-- @
-- 'box' "traffic" ['sized' (\w h -> 'plotLine' (w - 8) (h - 2) series) & 'fill'] & 'fill'
-- @
sized :: (Int -> Int -> View m) -> View m
sized = Sized

fixedWidth :: Int -> View m -> View m
fixedWidth w = \case
  Fixed _ h v -> Fixed (Just w) h v
  v           -> Fixed (Just w) Nothing v

fixedHeight :: Int -> View m -> View m
fixedHeight h = \case
  Fixed w _ v -> Fixed w (Just h) v
  v           -> Fixed Nothing (Just h) v

-- | Prefix every line, e.g. @'text' "boom" & 'margin' "[error]"@.
margin :: String -> View m -> View m
margin prefix = Margin (T.pack prefix)

-- | Cut lines longer than @n@, ending them with the 'ellipsis'.
--
-- @
-- 'text' "Very long text that will be cut off" & 'clip' 15  -- Very long te...
-- @
clip :: Int -> View m -> View m
clip n = Clip n "..."

alignLeft, alignRight, alignCenter, justify :: Int -> View m -> View m
alignLeft   = aligned AlignLeft
alignRight  = aligned AlignRight
alignCenter = aligned AlignCenter
justify     = aligned Justify

aligned :: Alignment -> Int -> View m -> View m
aligned = Aligned

-- | Greedy word wrap at @w@ columns. A longer word gets a line to itself.
wrap :: Int -> View m -> View m
wrap w = Mapped (T.intercalate "\n" . wrapWords . T.words)
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

--------------------------------------------------------------------------------
-- Events
--
-- Handlers ride along in the tree as 'OnEvent' nodes. Drawing turns each into
-- a rectangle on the block it lands in, so a click is answered by the last
-- (innermost) rectangle under it that handles it.

data Handlers m = Handlers
  { hClick  :: Maybe m
  , hScroll :: Maybe (Int -> m)        -- +1 per notch down, -1 up
  , hKey    :: Maybe (Key -> Maybe m)
  , hFocus  :: Maybe Text              -- this rectangle is a 'focusable'
  , hScope  :: Maybe Text              -- the focusable whose focus 'hKey' needs
  }

noHandlers :: Handlers m
noHandlers = Handlers Nothing Nothing Nothing Nothing Nothing

instance Functor Handlers where
  fmap f h = h { hClick  = f <$> hClick h
               , hScroll = (f .) <$> hScroll h
               , hKey    = (fmap f .) <$> hKey h }

-- | Rebuild a node from its children run through @f@, mapping messages with @onMsg@.
descend :: (a -> b) -> (View a -> View b) -> View a -> View b
descend onMsg f = \case
  Leaf x            -> Leaf x
  Stack xs          -> Stack (map f xs)
  Row g xs          -> Row g (map f xs)
  Margin p v        -> Margin p (f v)
  Section t g n xs  -> Section t g n (map f xs)
  Rule h g n        -> Rule h g n
  Underline g c v   -> Underline g c (f v)
  Clip n e v        -> Clip n e (f v)
  Pad p v           -> Pad p (f v)
  Aligned a w v     -> Aligned a w (f v)
  Bullets xs        -> Bullets (map f xs)
  Numbered xs       -> Numbered (map f xs)
  AutoCenter v      -> AutoCenter (f v)
  Colored c v       -> Colored c (f v)
  Styled st v       -> Styled st (f v)
  Box t xs b        -> Box t (map f xs) b
  StatusCard l c b  -> StatusCard l c b
  Table hs rs b     -> Table hs (map (map f) rs) b
  Mapped g v        -> Mapped g (f v)
  OnEvent h v       -> OnEvent (fmap onMsg h) (f v)
  Fill v            -> Fill (f v)
  Fixed w h v       -> Fixed w h (f v)
  Focusable n v     -> Focusable n (f v)
  WhenFocused t v   -> WhenFocused t (f v)
  Scrolled k vp v   -> Scrolled (onMsg . k) vp (f v)
  Following i v     -> Following i (f v)
  Sized k           -> Sized (\w h -> f (k w h))

-- | Embed a child component: @fmap ChildMsg (childView model)@.
instance Functor View where
  fmap g = descend g (fmap g)

attach :: Handlers m -> View m -> View m
attach h (AutoCenter v) = AutoCenter (attach h v)  -- keep centring visible to 'layout'
attach h v              = OnEvent h v

-- | Send @msg@ when the element is left-clicked. Clicks go to the innermost
-- element that handles them.
onClick :: msg -> View msg -> View msg
onClick msg = attach noHandlers { hClick = Just msg }

-- | Mouse wheel over the element: @+1@ per notch down, @-1@ up.
onScroll :: (Int -> msg) -> View msg -> View msg
onScroll f = attach noHandlers { hScroll = Just f }

-- | Keys for this view. Inside a 'focusable' they only arrive while it has
-- focus; elsewhere they arrive whenever nothing focused took the key first.
onKey :: (Key -> Maybe msg) -> View msg -> View msg
onKey f = attach noHandlers { hKey = Just f }

-- | A view that can hold the keyboard. Tab and Shift-Tab move focus between
-- them in screen order, a click focuses one, and the first is focused to
-- start with.
focusable :: String -> View msg -> View msg
focusable name = Focusable (T.pack name)

-- | How the view looks while it has focus, e.g. @'focusStyle' ('border' BorderDouble)@.
focusStyle :: (forall n. View n -> View n) -> View m -> View m
focusStyle = WhenFocused

-- | Apply each 'focusStyle' whose view has focus.
resolveFocus :: Maybe Text -> View m -> View m
resolveFocus focused = go False
  where
    go on = \case
      Focusable n v   -> Focusable n (go (Just n == focused) v)
      WhenFocused t v ->
        let v' = go on v
        in if on || focusName v == focused && isJust focused then t v' else v'
      v -> descend id (go on) v

-- | The 'focusable' a view is, under colour, style, handlers and sizing.
focusName :: View m -> Maybe Text
focusName = \case
  Focusable n _   -> Just n
  Colored _ v     -> focusName v
  Styled _ v      -> focusName v
  OnEvent _ v     -> focusName v
  WhenFocused _ v -> focusName v
  Fill v          -> focusName v
  Fixed _ _ v     -> focusName v
  AutoCenter v    -> focusName v
  _               -> Nothing

-- | A clickable label in reverse video.
button :: String -> msg -> View msg
button label msg = onClick msg (styleReverse (text (" " <> label <> " ")))

-- | Handlers under a cell, innermost first.
hitsAt :: (Int, Int) -> [(Rect, h)] -> [h]
hitsAt (r, c) hits = reverse [ h | (Rect t l hh w, h) <- hits, r >= t, r < t + hh, c >= l, c < l + w ]

dispatchAt :: (Handlers m -> Maybe m) -> (Int, Int) -> [(Rect, Handlers m)] -> Maybe m
dispatchAt pick pos = listToMaybe . mapMaybe pick . hitsAt pos

-- | Focusables in screen order.
focusNames :: [(Rect, Handlers m)] -> [Text]
focusNames hits = [ n | (_, h) <- hits, Just n <- [hFocus h] ]

-- | Where a key goes: the focused view's handlers, innermost first, then
-- handlers outside any focusable.
keyMsg :: Maybe Text -> [(Rect, Handlers m)] -> Key -> Maybe m
keyMsg focused hits key = (focused >>= try . Just) <|> try Nothing
  where
    try scope = listToMaybe (mapMaybe ($ key) (reverse [ k | (_, h) <- hits, hScope h == scope, Just k <- [hKey h] ]))

-- | What a left click at @(row, col)@ of the rendered view sends. For tests.
clickAt :: (Int, Int) -> View m -> Maybe m
clickAt pos = dispatchAt hClick pos . bHits . drawView natural

-- | What one wheel notch at @(row, col)@ sends (@+1@ down, @-1@ up).
scrollAt :: Int -> (Int, Int) -> View m -> Maybe m
scrollAt d pos = dispatchAt (\h -> ($ d) <$> hScroll h) pos . bHits . drawView natural

-- | What pressing a key sends while the named 'focusable' has focus. For tests.
pressKey :: Maybe String -> Key -> View m -> Maybe m
pressKey focused key v = keyMsg f (bHits (drawView natural (resolveFocus f v))) key
  where f = T.pack <$> focused

-- | The 'focusable' names in Tab order. For tests.
focusOrder :: View m -> [String]
focusOrder = map T.unpack . focusNames . bHits . drawView natural

--------------------------------------------------------------------------------
-- Widgets
--
-- Their state lives in your model. Each message carries the widget's next
-- state, worked out where the real size is known, so 'appUpdate' just stores it.

-- | How far a 'viewport' is scrolled.
newtype Viewport = Viewport Int   -- first row shown; maxBound sticks to the bottom
  deriving (Show, Eq)

viewportTop, viewportBottom :: Viewport
viewportTop    = Viewport 0
viewportBottom = Viewport maxBound

-- | A scrollable area: wheel, arrows, PgUp\/PgDn, Home\/End, with a scrollbar
-- once the content outgrows it. Give it a height with 'fill' or
-- 'fixedHeight'. Scrolled to the end, it stays there as content grows.
--
-- @
-- 'viewport' LogMoved (logView m) (map text (logs m)) & 'fill' & 'focusable' "logs"
-- @
viewport :: (Viewport -> msg) -> Viewport -> [View msg] -> View msg
viewport moved vp = Scrolled moved vp . vstack

-- | A list with a cursor. Keys, clicks and the wheel move it; Enter picks.
data SelectList a = SelectList [a] Int

instance Functor SelectList where
  fmap f (SelectList xs i) = SelectList (map f xs) i

selectListOf :: [a] -> SelectList a
selectListOf xs = SelectList xs 0

selectedItem :: SelectList a -> Maybe a
selectedItem (SelectList xs i) = listToMaybe (drop i xs)

-- | @selectList moved picked draw list@. Keeps the cursor in sight when it
-- has less room than it needs.
selectList :: (SelectList a -> msg) -> (a -> msg) -> (a -> View msg) -> SelectList a -> View msg
selectList moved picked draw sl@(SelectList xs i) =
  Following i (vstack (zipWith item [0 ..] xs))
    & onKey keys
    & onScroll (\d -> moved (at (i + d)))
  where
    at j = SelectList xs (max 0 (min (length xs - 1) j))
    item j x
      | j == i    = row ["›", draw x] & fg ColorCyan & onClick (moved (at j))
      | otherwise = row [" ", draw x] & onClick (moved (at j))
    keys = \case
      KeyUp       -> Just (moved (at (i - 1)))
      KeyDown     -> Just (moved (at (i + 1)))
      KeyPageUp   -> Just (moved (at (i - 10)))
      KeyPageDown -> Just (moved (at (i + 10)))
      KeyHome     -> Just (moved (at 0))
      KeyEnd      -> Just (moved (at (length xs - 1)))
      KeyEnter    -> picked <$> selectedItem sl
      _           -> Nothing

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
    | T.null label = frameGlyph
    | otherwise    = frameGlyph <> " " <> label
    where frameGlyph = spinnerFrame style frame

-- | One frame of a spinner; bump the frame each tick to animate.
--
-- @
-- 'layout' ['spinner' \"Working\" tick 'SpinnerDots']
-- @
spinner :: String -> Int -> SpinnerStyle -> View m
spinner label frame style = Leaf (Spinner (T.pack label) frame style)

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

-- | @3@ for whole numbers, @3.5@ otherwise. Rounds to one decimal first, so a
-- tick a hair below zero reads @0@, not @-0.0@.
formatAxisNum :: Double -> Text
formatAxisNum v
  | tenths `mod` 10 == 0 = T.show (tenths `div` 10)
  | otherwise            = T.pack (printf "%.1f" (fromInteger tenths / 10 :: Double))
  where tenths = round (v * 10) :: Integer

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

plotSparkline :: [Double] -> View m
plotSparkline = Leaf . SparklineData

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

          -- Each cell holds its braille bits and the first series to touch it,
          -- built in one pass so a dense series doesn't cost points × cells.
          dots = IntMap.fromListWith (\(b, si) (b', si') -> (b .|. b', min si si'))
            [ ((py `div` 4) * w + px `div` 2, (brailleDot (py `mod` 4) (px `mod` 2), sIdx))
            | (sIdx, Series ps _ _) <- zip [0 :: Int ..] ss
            , (px, py) <- map toPixel ps ]

          grid = [ [ IntMap.findWithDefault (0, -1) (r * w + c) dots | c <- [0 .. w - 1] ]
                 | r <- [0 .. h - 1] ]

          yTicks  = [ yMax - yRng * fromIntegral i / fromIntegral (max 1 (h - 1)) | i <- [0 .. h-1] ]
          yLabels = map formatAxisNum yTicks
          yLabelW = maximum (0 : map T.length yLabels)

          cell (bits, si)
            | bits == 0 = " "
            | si < 0    = dot
            | otherwise = wrapAnsi (pickColor si (sColor (ss !! si))) dot
            where dot = T.singleton (chr (0x2800 + bits))

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
plotLine :: Int -> Int -> [Series] -> View m
plotLine w h ss = Leaf (PlotData ss w h)

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
plotPie :: Int -> Int -> [Slice] -> View m
plotPie w h sl = Leaf (PieData sl w h)

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
plotBar :: Int -> Int -> [BarItem] -> View m
plotBar w h items = Leaf (BarChartData items w h)

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

plotStackedBar :: Int -> Int -> [StackedBarGroup] -> View m
plotStackedBar w h groups = Leaf (StackedBarChartData groups w h)

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
plotHeatmap :: HeatmapData -> View m
plotHeatmap = plotHeatmap' 6

plotHeatmap' :: Int -> HeatmapData -> View m
plotHeatmap' cellW dat = Leaf (HeatmapElement dat cellW)

--------------------------------------------------------------------------------
-- Terminal
--
-- Everything that touches the tty directly: setup and teardown, escape codes
-- out, keys and mouse reports in. The runtime and the Ask prompts sit on top.

data TermMode = FullScreen | Inline

-- | Unbuffered, no echo, cursor hidden (and the alt screen for 'FullScreen')
-- while @body@ runs. Put back on the way out, exceptions included.
withTerm :: TermMode -> IO a -> IO a
withTerm mode body = do
  oldBuffering <- hGetBuffering stdin
  oldEcho      <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  when full $ enterAltScreen >> clearScreen
  hideCursor
  body `finally` do
    showCursor
    when full exitAltScreen
    hFlush stdout
    hSetBuffering stdin oldBuffering
    hSetEcho stdin oldEcho
  where
    full = case mode of
      FullScreen -> True
      Inline     -> False

-- | Run @action@ on every SIGWINCH while @body@ runs. Nothing on Windows.
onResize :: IO () -> IO a -> IO a
#if !defined(mingw32_HOST_OS)
onResize action body = do
  old <- installHandler sigWINCH (Catch action) Nothing
  body `finally` installHandler sigWINCH old Nothing
#else
onResize _ body = body
#endif

-- | Press, release and wheel, SGR-encoded.
mouseEnable, mouseDisable :: Text
mouseEnable  = "\ESC[?1000h\ESC[?1006h"
mouseDisable = "\ESC[?1006l\ESC[?1000l"

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

-- | Save cursor, jump to the far corner, ask where we ended up, restore.
cursorProbe :: Text
cursorProbe = "\ESC7\ESC[999;999H\ESC[6n\ESC8"

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

-- | SGR mouse report: button code, 0-based (row, col), press or release.
data MouseEvent = MouseEvent { meButton :: Int, mePos :: (Int, Int), mePress :: Bool }
  deriving (Show, Eq)

data Input = InputKey Key | InputMouse MouseEvent | InputSize Int Int  -- rows, cols
  deriving (Show, Eq)

-- | Keys only; mouse reports are dropped.
readKey :: IO Key
readKey = readInput >>= \case
  InputKey k -> pure k
  _          -> readKey

-- | Read a char at a time until 'parseInput' has a whole input, so nothing
-- is ever read past it. A lone ESC is told apart from the start of a sequence
-- by waiting 50ms for the next byte.
readInput :: IO Input
readInput = getChar >>= go . pure
  where
    go buf = case parseInput buf of
      Just (i, _) -> pure i
      Nothing
        | buf == "\ESC" -> timeout 50000 getChar >>= maybe (pure (InputKey KeyEscape)) (more buf)
        | otherwise     -> getChar >>= more buf
    more buf c = go (buf <> [c])

-- | The first input in what the terminal sent, and what's left after it.
-- 'Nothing' if it's empty or stops partway through a sequence. A lone ESC is
-- 'Nothing' too: it's only an Esc keypress once nothing follows it in time,
-- and that's up to the caller.
--
-- Sequences we don't know come back as 'KeySpecial' rather than being
-- mistaken for Esc.
parseInput :: String -> Maybe (Input, String)
parseInput = \case
  []           -> Nothing
  '\ESC' : cs  -> escape cs
  c : cs       -> Just (InputKey (plainKey c), cs)
  where
    plainKey c = case ord c of
      10  -> KeyEnter
      13  -> KeyEnter
      9   -> KeyTab
      127 -> KeyBackspace
      8   -> KeyBackspace
      n | n >= 32 && n <= 126 -> KeyChar c
        | n < 32              -> KeyCtrl (chr (n + 64))
        | otherwise           -> KeyChar c

    escape = \case
      []           -> Nothing
      '[' : cs     -> csi "" cs
      'O' : c : cs -> Just (InputKey (ss3 c), cs)   -- application cursor mode
      "O"          -> Nothing
      c : cs       -> Just (InputKey (KeySpecial [c]), cs)

    ss3 = \case
      'H' -> KeyHome
      'F' -> KeyEnd
      c   -> KeySpecial ['O', c]

    -- parameter and intermediate bytes, then one final byte in @..~
    csi acc = \case
      [] -> Nothing
      c : cs
        | c >= '@' && c <= '~' -> Just (decode (reverse acc) c, cs)
        | otherwise            -> csi (c : acc) cs

    decode ('<' : ps) f | f == 'M' || f == 'm' = sgrMouse ps (f == 'M')
    -- A cursor position report answering 'cursorProbe'. Modified F3 has the
    -- same shape (ESC [ 1 ; mods R), but a probe at row 999 never reports row 1.
    decode ps 'R' | Just [r, c] <- mapM readInt (splitOnChar ';' (T.pack ps)), r > 1 =
      InputSize r c
    decode ps f = InputKey $ case (ps, f) of
      ("", 'A')  -> KeyUp
      ("", 'B')  -> KeyDown
      ("", 'C')  -> KeyRight
      ("", 'D')  -> KeyLeft
      ("", 'H')  -> KeyHome
      ("", 'F')  -> KeyEnd
      ("", 'Z')  -> KeyBackTab
      ("1", '~') -> KeyHome
      ("3", '~') -> KeyDelete
      ("4", '~') -> KeyEnd
      ("5", '~') -> KeyPageUp
      ("6", '~') -> KeyPageDown
      _          -> KeySpecial ('[' : ps <> [f])

    -- ESC [ < button ; col ; row (M | m), coordinates 1-based
    sgrMouse ps press = case mapM readInt (splitOnChar ';' (T.pack ps)) of
      Just [b, x, y] -> InputMouse (MouseEvent b (y - 1, x - 1) press)
      _              -> InputKey (KeySpecial ("[<" <> ps))
    readInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing

--------------------------------------------------------------------------------
-- TUI runtime

data Key
  = KeyChar Char
  | KeyCtrl Char           -- ^ e.g. @KeyCtrl 'C'@
  | KeyEnter
  | KeyBackspace
  | KeyTab
  | KeyBackTab             -- ^ Shift-Tab
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

-- | Effects for the runtime to run after an update. Combine with @<>@; every
-- message they produce is delivered.
data Cmd msg = Cmd [IO (Maybe msg)] (Maybe Text) Bool   -- effects, focus, then quit?

instance Semigroup (Cmd msg) where
  Cmd a f q <> Cmd b g r = Cmd (a <> b) (g <|> f) (q || r)

instance Monoid (Cmd msg) where
  mempty = Cmd [] Nothing False

instance Functor Cmd where
  fmap f (Cmd ios fo q) = Cmd (map (fmap (fmap f)) ios) fo q

cmdNone :: Cmd msg
cmdNone = mempty

-- | Fire and forget.
cmdFire :: IO () -> Cmd msg
cmdFire io = Cmd [Nothing <$ io] Nothing False

-- | Run IO and feed the result back as a message.
cmdTask :: IO msg -> Cmd msg
cmdTask io = Cmd [Just <$> io] Nothing False

cmdAfterMs :: Int -> msg -> Cmd msg
cmdAfterMs ms msg = cmdTask (threadDelay (ms * 1000) >> pure msg)

-- | Stop the app once the other effects in the same command have run.
cmdQuit :: Cmd msg
cmdQuit = Cmd [] Nothing True

-- | Move keyboard focus to the named 'focusable'.
cmdFocus :: String -> Cmd msg
cmdFocus name = Cmd [] (Just (T.pack name)) False

-- | Run a command's effects in order, collecting the messages.
executeCmd :: Cmd msg -> IO [msg]
executeCmd (Cmd ios _ _) = catMaybes <$> sequence ios

-- | What the app listens to. Combine with @<>@. Key handlers see every key a
-- focused view didn't take.
data Sub msg = Sub [Key -> Maybe msg] [(Int, msg)] [String -> msg] [Double -> msg]

instance Semigroup (Sub msg) where
  Sub a b c d <> Sub e f g h = Sub (a <> e) (b <> f) (c <> g) (d <> h)

instance Monoid (Sub msg) where
  mempty = Sub [] [] [] []

instance Functor Sub where
  fmap f (Sub ks ts fs as) = Sub (map (fmap f .) ks) [ (n, f m) | (n, m) <- ts ] (map (f .) fs) (map (f .) as)

subNone :: Sub msg
subNone = mempty

subKeyPress :: (Key -> Maybe msg) -> Sub msg
subKeyPress k = Sub [k] [] [] []

-- | A message every @n@ milliseconds.
subEveryMs :: Int -> msg -> Sub msg
subEveryMs n m = Sub [] [(n, m)] [] []

-- | The name of the 'focusable' that just got focus.
subFocus :: (String -> msg) -> Sub msg
subFocus f = Sub [] [] [f] []

-- | A message every frame while subscribed, carrying the monotonic time in
-- seconds, for animation. The first one comes as soon as the subscription
-- appears rather than an interval later, so motion starts on the frame after
-- the change that asked for it. Drop it once things are still and an idle app
-- stops waking up.
--
-- @
-- subscriptions m = if moving m then 'subAnimationFrame' Frame else 'subNone'
-- @
subAnimationFrame :: (Double -> msg) -> Sub msg
subAnimationFrame f = Sub [] [] [] [f]

-- | An Elm-style app.
--
-- @
-- data Msg = Inc | Dec
--
-- counter :: 'LayoutzApp' Int Msg
-- counter = 'sandbox' 0 step $ \\n ->
--   'row' ['button' "-" Dec, 'display' n, 'button' "+" Inc]
--   where step Inc = (+ 1); step Dec = subtract 1
-- @
data LayoutzApp model msg = LayoutzApp
  { appInit          :: (model, Cmd msg)
  , appUpdate        :: msg -> model -> (model, Cmd msg)
  , appSubscriptions :: model -> Sub msg
  , appView          :: model -> View msg
  }

-- | An app with no effects and no subscriptions.
sandbox :: model -> (msg -> model -> model) -> (model -> View msg) -> LayoutzApp model msg
sandbox m step view = LayoutzApp
  { appInit          = (m, cmdNone)
  , appUpdate        = \msg s -> (step msg s, cmdNone)
  , appSubscriptions = const subNone
  , appView          = view
  }

data AppAlignment = AppAlignLeft | AppAlignCenter | AppAlignRight
  deriving (Show, Eq)

-- |
-- @
-- 'run' 'defaultOptions' { 'optAlignment' = 'AppAlignCenter' } myApp
-- @
data Options = Options
  { optAlignment :: AppAlignment
  , optInline    :: Bool   -- ^ draw in place under the current output, no alt screen
  , optQuitKeys  :: [Key]  -- ^ keys that end the app; Ctrl-C always does
  , optMaxFps    :: Int    -- ^ frame cap; changes in between share one frame
  } deriving (Show, Eq)

-- | Full screen, left-aligned, Esc or Ctrl-D quits.
defaultOptions :: Options
defaultOptions = Options
  { optAlignment = AppAlignLeft
  , optInline    = False
  , optQuitKeys  = [KeyEscape, KeyCtrl 'D']
  , optMaxFps    = 60
  }

-- | Full screen with 'defaultOptions'.
runApp :: LayoutzApp model msg -> IO ()
runApp = void . run defaultOptions

-- | Draw in place under the current output until 'cmdQuit'. Handy for
-- progress bars in scripts.
runInline :: LayoutzApp model msg -> IO ()
runInline = void . run defaultOptions { optInline = True }

-- | Every source (keys, mouse, ticks, command results) feeds one channel, so
-- a single thread applies 'appUpdate'.
data AppEvent msg
  = AppMsg msg
  | AppFrame Double (MVar ())   -- an animation frame's time; acked once applied
  | AppExit

isQuitKey :: Key -> Bool
isQuitKey k = k == KeyEscape || k == KeyCtrl 'C' || k == KeyCtrl 'D'

-- | Run an app, returning its final model.
--
-- Mouse reporting is on exactly while the view has an 'onClick' or
-- 'onScroll' in it, so apps without them keep native text selection.
run :: Options -> LayoutzApp model msg -> IO model
run opts app
  | optInline opts = withTerm Inline (runIn opts app)
  -- on resize, ask for the size again; the input thread reads the answer
  | otherwise      = withTerm FullScreen (onResize (putText cursorProbe >> hFlush stdout) (runIn opts app))

-- | 'run' once the terminal is set up.
runIn :: Options -> LayoutzApp model msg -> IO model
runIn opts LayoutzApp{..} = do
  let fullScreen = not (optInline opts)
      quits k    = k == KeyCtrl 'C' || k `elem` optQuitKeys opts

  -- before any thread starts reading stdin
  termSize <- newIORef =<< getTerminalSize

  let (initialState, initialCmd) = appInit

  stateRef  <- newIORef initialState
  eventChan <- newChan
  cmdChan   <- newChan

  focusRef  <- newIORef Nothing

  -- Full while something the view depends on has changed since the last
  -- frame; the render thread sleeps on it instead of polling.
  dirty <- newMVar ()
  -- Full when the model changed, so the subscriptions may have: wakes the
  -- tick thread when it has nothing to wait for.
  modelChanged <- newEmptyMVar

  let send = writeChan eventChan . AppMsg
      markDirty = void (tryPutMVar dirty ())
      frameNs = 1000000000 `div` fromIntegral (max 1 (optMaxFps opts)) :: Word64

      setFocus new = do
        old <- atomicModifyIORef' focusRef (\o -> (new, o))
        when (new /= old) $ do
          markDirty
          Sub _ _ listeners _ <- appSubscriptions <$> readIORef stateRef
          forM_ new $ \n -> mapM_ (send . ($ T.unpack n)) listeners

      schedule cmd@(Cmd ios focusOn q) = do
        mapM_ (setFocus . Just) focusOn
        if null ios then when q (writeChan eventChan AppExit) else writeChan cmdChan cmd

      -- Only this thread writes the model. An update that hands back the
      -- model it was given (@(m, cmd)@) changed nothing on screen, so it
      -- doesn't cost a frame, which would hold up the next real one.
      applyMsg msg = do
        old <- readIORef stateRef
        case appUpdate msg old of
          (new, cmd) -> do
            unless (same old new) $ do
              writeIORef stateRef $! new
              markDirty
              void (tryPutMVar modelChanged ())
            schedule cmd

  cmdThread <- forkIO $ forever $ do
    cmd@(Cmd _ _ q) <- readChan cmdChan
    executeCmd cmd >>= mapM_ (writeChan eventChan . AppMsg)
    when q $ writeChan eventChan AppExit

  schedule initialCmd

  lastFrame     <- newIORef ([], [])
  onScreen      <- newIORef []   -- rows as last written, alignment included
  sentKittyIds  <- newIORef IntSet.empty
  hitMap        <- newIORef []   -- click regions of what's on screen
  mouseOn       <- newIORef False
  resized       <- newIORef False

  let drawFrame = do
        (termH, termW) <- readIORef termSize
        redraw <- atomicModifyIORef' resized (\r -> (False, r))
        -- Full-screen apps get the terminal as room for 'fill'. Inline ones don't
        -- know which row they start on, so they get no room and no mouse.
        let room | fullScreen = Room (Just termW) (Just termH) False
                 | otherwise  = natural
        focused <- readIORef focusRef
        block <- drawView room . resolveFocus focused . appView <$> readIORef stateRef
        let frame = (bRaw block, bRows block)
        prev <- readIORef lastFrame
        shown <- readIORef onScreen
        wasOn <- readIORef mouseOn
        -- upload each kitty image once, not every frame
        sent <- readIORef sentKittyIds
        let (uploads, sent') = dedupeTransmits sent (T.concat (bRaw block))
            indent = case optAlignment opts of
              AppAlignLeft   -> 0
              AppAlignCenter -> max 0 ((termW - blockWidth block) `div` 2)
              AppAlignRight  -> max 0 (termW - blockWidth block)
            rows      = map (replicate indent blank <>) (snd frame)
            hits      = moveHits 0 indent (bHits block)
            wantMouse = fullScreen && not (null hits)
            mouseMode
              | wantMouse == wasOn = ""
              | wantMouse          = mouseEnable
              | otherwise          = mouseDisable
            body
              | fullScreen && redraw = repaintRows termW termH rows
              | fullScreen = diffRows shown rows   -- only the cells that changed
              | otherwise =
                  (if null shown then "" else "\ESC[" <> T.show (length shown) <> "A\r")
                  <> foldMap ((<> "\ESC[K\n") . rowText) rows
                  <> T.replicate (max 0 (length shown - length rows)) "\ESC[K\n"
            -- Full screen, the diff already says whether anything changed; inline
            -- rewrites every row, so compare the frames instead.
            changed
              | redraw     = True
              | fullScreen = not (T.null body && T.null mouseMode) || fst frame /= fst prev
              | otherwise  = frame /= prev

        when changed $ do
          putText $ "\ESC[?2026h" <> mouseMode <> uploads <> body <> "\ESC[?2026l"
          hFlush stdout
          writeIORef sentKittyIds sent'
          writeIORef mouseOn wantMouse
          writeIORef hitMap hits
          -- keep focus on something that's on screen
          case focusNames hits of
            first : _ | maybe True (`notElem` focusNames hits) focused -> setFocus (Just first)
            _ -> pure ()
          writeIORef onScreen rows
          writeIORef lastFrame frame


      animating = (\(Sub _ _ _ frames) -> not (null frames)) . appSubscriptions <$> readIORef stateRef

  -- Draws whenever something changed, at most once a frame. While the app
  -- wants animation frames this loop also keeps the beat: each frame the app
  -- is handed that frame's own time, evenly spaced, and the frame is drawn
  -- once that update is in. One clock, so motion doesn't judder from two
  -- loops drifting in and out of step.
  renderThread <- forkIO $
    let loop due = animating >>= \case
          False -> do
            takeMVar dirty
            frameStart <- getMonotonicTimeNSec
            drawFrame
            -- changes arriving before the next frame is due share it
            spent <- (`div` 1000) . subtract frameStart <$> getMonotonicTimeNSec
            threadDelay (max 0 (1000000 `div` max 1 (optMaxFps opts) - fromIntegral spent))
            loop Nothing
          True -> do
            now <- getMonotonicTimeNSec
            -- on the beat, unless this is the first frame or one ran long
            let t = case due of
                  Just d | d + frameNs > now -> d
                  _                          -> now
            when (t > now) $ threadDelay (fromIntegral ((t - now) `div` 1000))
            applied <- newEmptyMVar
            writeChan eventChan (AppFrame (fromIntegral t / 1e9) applied)
            takeMVar applied
            _ <- tryTakeMVar dirty
            drawFrame
            loop (Just (t + frameNs))
    in loop Nothing

  -- Deadlines are tracked per interval so re-subscribing each frame doesn't
  -- reset the clock. Sleeps until the next deadline, waking at least every
  -- 20ms to pick up changed subscriptions. With no ticks at all it sleeps
  -- until the model changes, so an idle app doesn't wake up.
  tickThread <- forkIO $
    let loop deadlines = do
          _ <- tryTakeMVar modelChanged
          Sub _ ticks _ _ <- appSubscriptions <$> readIORef stateRef
          if null ticks then takeMVar modelChanged >> loop [] else do
            now <- getMonotonicTimeNSec
            let ns ms     = fromIntegral ms * 1000 * 1000
                armed     = [ (i, fromMaybe (now + ns i) (lookup i deadlines)) | i <- nub (map fst ticks) ]
                due       = [ i | (i, d) <- armed, d <= now ]
                deadlines' =
                  [ (i, if i `elem` due then let nd = d + ns i in if nd <= now then now + ns i else nd else d)
                  | (i, d) <- armed ]
            mapM_ (writeChan eventChan . AppMsg) [ msg | (i, msg) <- ticks, i `elem` due ]
            let nextNs = minimum (now + 20000000 : map snd deadlines')
            threadDelay (max 1000 (min 20000 (fromIntegral ((nextNs - now) `div` 1000))))
            loop deadlines'
    in loop []

  -- EOF on stdin quits, same as Ctrl-D.
  inputThread <- forkIO $
    let loop = readInput >>= \case
          InputSize r c -> do
            writeIORef termSize (r, c)
            writeIORef resized True
            markDirty
            loop
          InputMouse ev -> do
            hits <- readIORef hitMap
            when (mePress ev && meButton ev .&. 0x43 == 0) $   -- a left click focuses
              mapM_ (setFocus . Just) (listToMaybe (mapMaybe hFocus (hitsAt (mePos ev) hits)))
            mapM_ send (mouseMsg ev hits)
            loop
          InputKey key
            | quits key -> writeChan eventChan AppExit
            | otherwise -> do
                hits    <- readIORef hitMap
                focused <- readIORef focusRef
                let order = focusNames hits
                    step  = if key == KeyBackTab then -1 else 1
                case order of
                  first : _ | key `elem` [KeyTab, KeyBackTab] ->
                    setFocus $ Just $ case focused >>= (`elemIndex` order) of
                      Just i  -> order !! ((i + step) `mod` length order)
                      Nothing -> first
                  _ -> case keyMsg focused hits key of
                    Just m  -> send m
                    Nothing -> do
                      Sub keys _ _ _ <- appSubscriptions <$> readIORef stateRef
                      mapM_ send (mapMaybe ($ key) keys)
                loop
    in loop `catch` \(_ :: IOException) -> writeChan eventChan AppExit

  let cleanup = do
        mapM_ killThread [renderThread, cmdThread, tickThread, inputThread]
        readIORef mouseOn >>= \on -> when on (putText mouseDisable)

      dispatch = readChan eventChan >>= \case
        AppExit    -> pure ()
        AppMsg msg -> applyMsg msg >> dispatch
        AppFrame t applied -> do
          Sub _ _ _ frames <- appSubscriptions <$> readIORef stateRef
          mapM_ (applyMsg . ($ t)) frames
          putMVar applied ()
          dispatch

      -- Ctrl-C is a normal quit, everything else rethrows. withTerm puts
      -- the tty back either way.
      onAsync UserInterrupt = pure ()
      onAsync e             = throwIO e

  (dispatch `catch` onAsync) `finally` cleanup

  readIORef stateRef

-- | Left press without modifiers is a click; codes 64\/65 are the wheel.
mouseMsg :: MouseEvent -> [(Rect, Handlers m)] -> Maybe m
mouseMsg MouseEvent{..} hits
  | not mePress            = Nothing
  | meButton .&. 64 /= 0   = dispatchAt (\h -> ($ notch) <$> hScroll h) mePos hits
  | meButton .&. 0x3f == 0 = dispatchAt hClick mePos hits
  | otherwise              = Nothing
  where notch = if meButton .&. 1 == 0 then -1 else 1

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
    fgCode = "\ESC[38;2;" <> T.show ((theId `shiftR` 16) .&. 0xff) <> ";"
         <> T.show ((theId `shiftR` 8) .&. 0xff) <> ";" <> T.show (theId .&. 0xff) <> "m"
    placeholder = T.singleton kittyPlaceholderChar
    diacritic i = T.singleton (chr (rowColumnDiacritics !! i))
    rowLine r = fgCode <> placeholder <> diacritic r <> diacritic 0
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
kittyImage :: [Word8] -> Int -> Int -> View m
kittyImage bytes cols rows = Leaf (mkKitty bytes formatPng cols rows 0 0 "")

-- | Raw RGB pixels, @pxW@ x @pxH@, shown in a @cols@ x @rows@ cell box.
kittyRGB :: [Word8] -> Int -> Int -> Int -> Int -> View m
kittyRGB pixels pxW pxH cols rows = Leaf (mkKitty pixels formatRgb cols rows pxW pxH "")

-- | 'kittyRGB' with an alpha channel.
kittyRGBA :: [Word8] -> Int -> Int -> Int -> Int -> View m
kittyRGBA pixels pxW pxH cols rows = Leaf (mkKitty pixels formatRgba cols rows pxW pxH "")

-- | Load a PNG from disk.
kittyImageFile :: FilePath -> Int -> Int -> IO (View m)
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
loaderBlocks :: LoaderStyle
loaderBlocks = LoaderStyle '█' '░' "" "" "" True SpinnerGrow ColorCyan

-- | @[██████░░░░]@
loaderBar :: LoaderStyle
loaderBar = LoaderStyle '█' '░' "[" "]" "" False SpinnerDots ColorGreen

-- | @[====>    ]@
loaderAscii :: LoaderStyle
loaderAscii = LoaderStyle '=' ' ' "[" "]" ">" False SpinnerLine ColorYellow

-- | @●●●●∙∙∙∙@
loaderDots :: LoaderStyle
loaderDots = LoaderStyle '●' '∙' "" "" "" False SpinnerBounce ColorMagenta

-- | @━━━━────@
loaderLine :: LoaderStyle
loaderLine = LoaderStyle '━' '─' "" "" "" False SpinnerLine ColorBlue

-- | @▰▰▰▱▱▱@
loaderPipes :: LoaderStyle
loaderPipes = LoaderStyle '▰' '▱' "" "" "" False SpinnerArrow ColorBrightMagenta

loaderStyles :: [LoaderStyle]
loaderStyles = [loaderBlocks, loaderBar, loaderAscii, loaderDots, loaderLine, loaderPipes]

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
loader = loaderStyled loaderBlocks

loaderStyled :: LoaderStyle -> String -> [a] -> (a -> IO b) -> IO [b]
loaderStyled style labelS items action = do
  counter <- newIORef 0
  let frame = boundedFrame style (T.pack labelS) (length items) counter
  runLoader (const frame) frame counter items action

-- | Like 'loader' but for when the length isn't worth forcing: a spinner and
-- a running count.
loaderStream :: String -> [a] -> (a -> IO b) -> IO [b]
loaderStream = loaderStreamStyled loaderBlocks

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
visibleSlice :: Int -> Int -> [a] -> (Int, [a])
visibleSlice rows idx xs = (top, take h (drop top xs))
  where
    total = length xs
    h     = max 0 (min rows total)
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
renderChooseManyFrame prompt items idx picked limit renderItem =
  header <> T.intercalate "\n" rows
  where
    rows = [ let mark = if i `elem` picked then wrapAnsi ColorGreen "[x]" else dimText "[ ]"
                 line = mark <> " " <> renderItem item
             in if i == idx then wrapAnsi ColorCyan "› " <> line else "  " <> line
           | (item, i) <- zip items [0 ..] ]
    n = T.show (length picked)
    tag | limit > 0          = " (" <> n <> "/" <> T.show limit <> ")"
        | not (null picked) = " (" <> n <> ")"
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
    (top, visible) = visibleSlice viewHeight idx matches
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
    go Empty _ _ slack _ = Just slack
    go _ Empty _ _ _   = Nothing
    go q@(qc :< qs) (sc :< ss) lastHit slack i
      | qc == sc  = go qs ss i (if lastHit >= 0 then slack + (i - lastHit - 1) else slack) (i + 1)
      | otherwise = go q ss lastHit slack (i + 1)

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
    (top, visible) = visibleSlice viewHeight idx entries
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
askInput promptS placeholderS initialS = withTerm Inline $ do
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
askConfirm questionS def affirmativeS negativeS = withTerm Inline $ do
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
askChoose promptS items renderItemS = withTerm Inline $ do
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
askChooseMany promptS items limit renderItemS = withTerm Inline $ do
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
  let (mark, col) = either (const ("✗", ColorRed)) (const ("✓", ColorGreen)) outcome
  putText ("\r" <> wrapAnsi col (mark <> " " <> label) <> askClearEol <> "\n")
  showCursor
  hFlush stdout
  either throwIO pure outcome

-- | Multi-line input. Enter adds a newline, Ctrl-D submits, Esc cancels.
askWrite :: String   -- ^ prompt
         -> String   -- ^ placeholder
         -> String   -- ^ initial value
         -> String   -- ^ hint line
         -> IO (Maybe String)
askWrite promptS placeholderS initialS hintS = withTerm Inline $ do
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
askFilter promptS items viewHeight renderItemS = withTerm Inline $ do
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
askFile start viewHeight = withTerm Inline (loop start 0 0)
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
askPager contentS viewHeight lineNumbers = withTerm Inline $ do
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
