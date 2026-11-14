{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (foldM_, forM_, unless, void, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, freeze, newArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (inits, isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, listToMaybe)
import Data.Ord (Down (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import GHC.Clock (getMonotonicTime)
import Layoutz (Cmd, Key (..), LayoutzApp (..), Options (..), cmdQuit, cmdTask, defaultOptions, run, sized, subAnimationFrame, subKeyPress, text)

type RGB = Int

rgb :: Int -> Int -> Int -> RGB
rgb r g b = r `shiftL` 16 .|. g `shiftL` 8 .|. b

-- | From @a@ toward @b@, @t@ of the way.
mix :: RGB -> RGB -> Double -> RGB
mix a b t = rgb (channel 16) (channel 8) (channel 0)
  where
    channel s =
      let x = (a `shiftR` s) .&. 255
          y = (b `shiftR` s) .&. 255
      in max 0 (min 255 (round (fromIntegral x + fromIntegral (y - x) * t)))

ground, rim, faint, dim, ink, bright, gold :: RGB
ground = rgb 10 12 22
rim    = rgb 92 112 168
faint  = rgb 62 70 102
dim    = rgb 108 118 156
ink    = rgb 196 204 232
bright = rgb 236 240 255
gold   = rgb 255 204 122

--------------------------------------------------------------------------------
-- The Ainur

data Node = Node
  { nName   :: String
  , nHue    :: RGB
  , nWeight :: Double
  , nCard   :: Maybe Card   -- Nothing for a group
  , nKids   :: [Node]       -- heaviest first
  , nSlots  :: [Slot]       -- where each kid sits inside this node
  }

data Card = Card
  { cTitle  :: String
  , cKind   :: String
  , cFacts  :: [(String, String)]
  , cStory  :: String
  , cSource :: String
  }

isGroup :: Node -> Bool
isGroup = isNothing . nCard

-- | A group passes its colour down to its cards, and keeps them heaviest
-- first, the order the treemap wants.
group :: String -> RGB -> [Node] -> Node
group name hue members = Node
  { nName = name, nHue = hue, nWeight = sum (map nWeight kids)
  , nCard = Nothing, nKids = kids, nSlots = treemap (map nWeight kids) }
  where kids = sortOn (Down . nWeight) [ k { nHue = hue } | k <- members ]

-- | A card: how much room it gets, then its name, title, kind, facts, story
-- and sources.
card :: Double -> String -> String -> String -> [(String, String)] -> String -> String -> Node
card weight name title kind facts story source = Node
  { nName = name, nHue = rim, nWeight = weight
  , nCard = Just (Card title kind facts story source), nKids = [], nSlots = [] }

-- Weights are an editorial call: the eight Aratar get the most room, the
-- Maiar whom the books barely mention the least.
ainur :: Node
ainur = group "The Ainur" rim
  [ group "Manwë and Varda" (rgb 132 172 255)
      [ card 6 "Manwë" "King of Arda, Lord of the Breath of Arda" "Vala · first of the Aratar"
          [ ("dwells", "Ilmarin, upon Taniquetil")
          , ("spouse", "Varda")
          , ("kin", "brother of Melkor in the thought of Ilúvatar")
          , ("names", "Súlimo") ]
          "Dearest to Ilúvatar, and the one who best understood his purpose. The winds and the upper airs are his, and the hawks and eagles bring him word of everything. Being without evil himself he could not really comprehend it, which is how Melkor talked his way out of the Halls of Mandos."
          "Silmarillion · Valaquenta; Of Fëanor and the Unchaining of Melkor"
      , card 6 "Varda" "Queen of the Stars" "Valië · of the Aratar"
          [ ("dwells", "Ilmarin, with Manwë")
          , ("spouse", "Manwë")
          , ("names", "Elentári; Tintallë the Kindler; Elbereth Gilthoniel to the Sindar") ]
          "She made the stars before the Elves awoke, so starlight was the first thing they ever saw, and they have loved her above all the Valar since. Melkor feared her more than any of them. When Frodo cries out to Elbereth on Weathertop, she is the one he means."
          "Silmarillion · Valaquenta; Fellowship I.12"
      , card 2.6 "Eönwë" "Herald of Manwë" "Maia · banner-bearer of the West"
          [ ("serves", "Manwë") ]
          "None in Arda is his match in arms. He led the host of Valinor in the War of Wrath and kept the last two Silmarils until the sons of Fëanor stole them from his camp. Sauron came to him after the war to beg pardon; Eönwë had no power to grant it, and sent him west to be judged. Sauron did not go."
          "Silmarillion · Valaquenta; Of the Voyage of Eärendil"
      , card 1.6 "Ilmarë" "Handmaid of Varda" "Maia"
          [ ("serves", "Varda") ]
          "Named in the Valaquenta as chief of Varda's handmaidens and hardly anywhere else. Tolkien leaves a good many of the Ainur like this: a name, a place and a dignity, and then they stay in the background, in the light."
          "Silmarillion · Valaquenta"
      ]
  , group "Ulmo" (rgb 84 176 206)
      [ card 6 "Ulmo" "Lord of Waters, King of the Sea" "Vala · of the Aratar"
          [ ("dwells", "the Outer Sea, and the waters under the world")
          , ("spouse", "none")
          , ("people", "Ossë, Uinen, Salmar") ]
          "He keeps no hall in Valinor and seldom comes to its councils. Of all the Valar he never gave up on Middle-earth, and speaks through its springs and rivers to anyone who will listen. He rose out of the sea at Vinyamar to send Tuor to Gondolin."
          "Silmarillion · Valaquenta; Of Tuor and the Fall of Gondolin"
      , card 2.4 "Ossë" "Master of the waves" "Maia · vassal of Ulmo"
          [ ("spouse", "Uinen")
          , ("seas", "those that wash the shores of Middle-earth") ]
          "He delights in storms. For a time Melkor won him over, and Uinen brought him back to Ulmo. He taught the Falathrim of the coasts their seacraft, and it was Ossë who raised the land of Númenor up out of the deep."
          "Silmarillion · Valaquenta; Akallabêth"
      , card 2.2 "Uinen" "Lady of the Seas" "Maia"
          [ ("spouse", "Ossë") ]
          "Her hair lies spread through all the waters under heaven. She loves everything that lives in the salt streams and calms whatever Ossë stirs up. Sailors call on her, and the Númenóreans held her as high as any of the Valar."
          "Silmarillion · Valaquenta"
      , card 1.6 "Salmar" "Maker of the Ulumúri" "Maia"
          [ ("serves", "Ulmo") ]
          "He came into the world with Ulmo and made the great horns on which Ulmo is heard. Whoever hears them never stops longing for the sea afterwards. That is all we are told about him."
          "Silmarillion · Valaquenta"
      ]
  , group "Aulë and Yavanna" (rgb 196 168 108)
      [ card 6 "Aulë" "The Smith" "Vala · of the Aratar"
          [ ("dwells", "his halls in the midst of Valinor")
          , ("spouse", "Yavanna")
          , ("people", "Curumo; Sauron, before his fall") ]
          "Lord of the substances of Arda and of every craft that shapes them. Too impatient to wait for the Children, he made the Seven Fathers of the Dwarves in secret; found out, he raised his hammer to destroy them, and Ilúvatar spared them. The Noldor learned from him, and so, early on, did Sauron."
          "Silmarillion · Valaquenta; Of Aulë and Yavanna"
      , card 6 "Yavanna" "Giver of Fruits, Queen of the Earth" "Valië · of the Aratar"
          [ ("spouse", "Aulë")
          , ("kin", "elder sister of Vána")
          , ("names", "Kementári") ]
          "Everything that grows is hers. She sang the Two Trees out of the green mound Ezellohar. When Aulë confessed the Dwarves she feared for her forests and asked for guardians, and the Shepherds of the Trees were the answer."
          "Silmarillion · Valaquenta; Of Aulë and Yavanna"
      , card 3 "Curumo" "Saruman, chief of the Istari" "Maia · of Aulë's people"
          [ ("names", "Saruman; Curunír, the man of skill") ]
          "Sent with the other Istari to stand against Sauron, and forbidden to meet his power with power. He studied the Enemy's craft until he wanted the Ring for himself, and ended in the Shire, killed on a doorstep by his own servant."
          "Unfinished Tales · The Istari; Return of the King VI.8"
      , card 2.4 "Aiwendil" "Radagast the Brown" "Maia · of Yavanna's people · Istar"
          [ ("dwelt", "Rhosgobel, by the eaves of Mirkwood") ]
          "Yavanna begged Curumo to take him along, and Curumo did, with a bad grace. He grew fonder of birds and beasts than of Elves and Men. The errand he honestly carried for Saruman is what brought Gwaihir to the roof of Orthanc to carry Gandalf off."
          "Unfinished Tales · The Istari; Fellowship II.2"
      ]
  , group "Oromë and Vána" (rgb 116 190 142)
      [ card 6 "Oromë" "The Huntsman, Lord of Forests" "Vala · of the Aratar"
          [ ("spouse", "Vána")
          , ("kin", "brother of Nessa")
          , ("names", "Aldaron; Tauron; Araw to the Sindar") ]
          "Through the long dark he kept riding into Middle-earth on his white horse Nahar, hunting Melkor's creatures, and so he was the one who found the Elves by the water of Cuiviénen. His horn is the Valaróma. The wild white kine near the Sea of Rhûn were said to come from his herds."
          "Silmarillion · Valaquenta; Of the Coming of the Elves; RotK App. A"
      , card 4 "Vána" "The Ever-young" "Valië"
          [ ("spouse", "Oromë")
          , ("kin", "younger sister of Yavanna") ]
          "Flowers open as she passes and birds sing at her coming. Little more is said of her, except through those who served her: Arien kept the golden flowers of her gardens, and Melian was of her people."
          "Silmarillion · Valaquenta"
      , card 2.4 "Arien" "Bearer of the Sun" "Maia · a spirit of fire"
          [ ("carries", "the Sun, the last fruit of Laurelin") ]
          "She tended the golden flowers in the gardens of Vána and watered them with the dew of Laurelin. Melkor had never managed to draw her to him. The Noldor called the Sun Vása, the Heart of Fire."
          "Silmarillion · Of the Sun and Moon"
      , card 2.4 "Tilion" "Steersman of the Moon" "Maia · a hunter of Oromë"
          [ ("carries", "the Moon, the last flower of Telperion") ]
          "A young hunter with a silver bow, who loved silver. He asked for the Moon and was given it. He loved Arien too, and would stray toward her and be scorched, which is why the Moon's path wavers and its face is marked."
          "Silmarillion · Of the Sun and Moon"
      , card 2.4 "Alatar & Pallando" "The Blue Wizards" "Maiar · Istari"
          [ ("names", "Ithryn Luin")
          , ("chosen", "Alatar by Oromë; Pallando went as his friend") ]
          "They went into the East and did not come back, and the West never learned what became of them. Tolkien changed his mind: in one letter they failed and founded secret cults, in later notes they hindered Sauron in the East more than anyone knew."
          "Unfinished Tales · The Istari; Letters 211"
      ]
  , group "The Fëanturi and Nienna" (rgb 162 146 232)
      [ card 6 "Námo" "Mandos, Doomsman of the Valar" "Vala · of the Aratar"
          [ ("dwells", "the Halls of Mandos, in the west of Valinor")
          , ("spouse", "Vairë")
          , ("kin", "brother of Irmo and Nienna") ]
          "He forgets nothing, and knows all that is to come except what Ilúvatar keeps to himself. He speaks the judgements of the Valar, and the Doom of the Noldor was spoken in his voice. He was moved to pity once: by Lúthien, singing to him of her grief and Beren's."
          "Silmarillion · Valaquenta; Of Beren and Lúthien"
      , card 4 "Vairë" "The Weaver" "Valië"
          [ ("spouse", "Námo")
          , ("dwells", "the Halls of Mandos") ]
          "She weaves everything that has ever happened in Time into storied webs. The halls of Mandos widen as the ages pass, and her webs hang on every wall."
          "Silmarillion · Valaquenta"
      , card 4 "Irmo" "Lórien, master of visions and dreams" "Vala"
          [ ("dwells", "the gardens of Lórien")
          , ("spouse", "Estë")
          , ("kin", "brother of Námo and Nienna") ]
          "His gardens are the fairest place in the world and full of spirits; the Valar themselves go there to rest. He and his brother are the Fëanturi, the masters of spirits."
          "Silmarillion · Valaquenta"
      , card 4 "Estë" "The Gentle, healer of hurt and weariness" "Valië"
          [ ("spouse", "Irmo")
          , ("dwells", "an island in the lake Lórellin") ]
          "She dresses in grey and her gift is rest. By day she sleeps on her island in the shaded lake of Lórellin, and the weary of Valinor go to her to be made whole."
          "Silmarillion · Valaquenta"
      , card 6 "Nienna" "Lady of pity and mourning" "Valië · of the Aratar"
          [ ("dwells", "at the western edge of the world")
          , ("kin", "sister of the Fëanturi") ]
          "She grieves for every hurt Melkor did to Arda, and in those who listen to her, grief turns to wisdom. When the Two Trees were dying, Yavanna's song and Nienna's tears drew from them a last flower and a last fruit, and those became the Moon and the Sun."
          "Silmarillion · Valaquenta; Of the Sun and Moon"
      , card 3 "Olórin" "Gandalf, Mithrandir" "Maia · Istar"
          [ ("dwelt", "in Lórien, often at Nienna's house")
          , ("names", "Mithrandir; Tharkûn; Incánus") ]
          "The wisest of the Maiar, who learned pity and patience from Nienna. Asked to go to Middle-earth, he said he was too weak and that he feared Sauron. Manwë said that was all the more reason to go. He came last of the five and was the one who saw the errand through."
          "Unfinished Tales · The Istari"
      , card 3 "Melian" "Queen of Doriath" "Maia"
          [ ("spouse", "Elwë Thingol")
          , ("child", "Lúthien") ]
          "Nightingales sang about her in the gardens of Lórien. Elwë heard her in the woods of Nan Elmoth and the two of them stood there, silent, while the trees grew tall. Her Girdle kept Doriath hidden for an age, and was lifted when Thingol died and she left Middle-earth."
          "Silmarillion · Of Thingol and Melian; Of the Ruin of Doriath"
      ]
  , group "Tulkas and Nessa" (rgb 232 156 112)
      [ card 4 "Tulkas" "Astaldo, the Valiant" "Vala"
          [ ("spouse", "Nessa") ]
          "Last of the Valar to come into Arda, drawn by the noise of the first war, and Melkor fled from his laughter. He fights bare-handed and needs no horse. No use at all in a council, but the friend you want in a fight. He wrestled Melkor down to be bound in the chain Angainor."
          "Silmarillion · Valaquenta; Ainulindalë; Of the Coming of the Elves"
      , card 4 "Nessa" "The Dancer" "Valië"
          [ ("spouse", "Tulkas")
          , ("kin", "sister of Oromë") ]
          "Quick as an arrow. The deer follow her through the wild and cannot keep up. She dances on the lawns of Valmar, which are always green."
          "Silmarillion · Valaquenta"
      ]
  , group "The Fallen" (rgb 214 92 92)
      [ card 6 "Melkor" "Morgoth, the Black Foe of the World" "Ainu · once the mightiest of them"
          [ ("kin", "brother of Manwë in the thought of Ilúvatar")
          , ("names", "Morgoth Bauglir, the Constrainer") ]
          "He was given the greatest share of power and knowledge, and wanted above all to bring into being things of his own. Failing that, he set out to master or ruin what others made. Fëanor named him Morgoth after he killed Finwë and took the Silmarils. He is not counted among the Valar."
          "Silmarillion · Ainulindalë; Valaquenta; Of the Flight of the Noldor"
      , card 3 "Sauron" "Mairon, Gorthaur, Annatar" "Maia · once of Aulë's people"
          [ ("served", "Melkor") ]
          "He began by loving order and design and came to love only his own will, imposed on everything. Morgoth's lieutenant in the First Age; in the Second, Annatar, Lord of Gifts, who taught the smiths of Eregion and in secret forged the One Ring. Unlike his master he never wanted the world destroyed. He wanted it his."
          "Silmarillion · Valaquenta; Of the Rings of Power"
      , card 2.2 "Gothmog" "Lord of Balrogs" "Maia · a Valarauko"
          [ ("served", "Melkor") ]
          "High-captain of Angband. He struck down Fëanor and slew Fingon, and at the fall of Gondolin he fought Ecthelion of the Fountain, and the two of them went down together into the fountain in the King's square."
          "Silmarillion · Of the Fifth Battle; Of Tuor and the Fall of Gondolin"
      , card 2.2 "Durin's Bane" "The Balrog of Moria" "Maia · a Valarauko"
          [ ("served", "Melkor") ]
          "It fled the ruin of Angband and hid at the roots of the mountains. The Dwarves of Khazad-dûm, mining too deep after mithril, woke it; it killed two of their kings and emptied the city. Gandalf broke the bridge beneath it, they fell together, and fought all the way up to the peak of Zirakzigil."
          "Fellowship II.5; Two Towers III.5; RotK App. A"
      ]
  , group "Of no known kind" (rgb 168 176 198)
      [ card 2.8 "Tom Bombadil" "Eldest, master of wood, water and hill" "of no known kind"
          [ ("dwells", "by the Withywindle, in the Old Forest")
          , ("spouse", "Goldberry") ]
          "He remembers the first raindrop and the first acorn. The Ring has no hold on him: he puts it on and does not vanish. Readers have made him a Maia, a Vala, Eru, the spirit of Arda itself. Tolkien wrote that even a mythology needs some enigmas, and left him one on purpose."
          "Fellowship I.7; Letters 144"
      , card 1.8 "Goldberry" "The River-daughter" "of no known kind"
          [ ("spouse", "Tom Bombadil")
          , ("kin", "daughter of the River-woman of the Withywindle") ]
          "Some read her as a Maia of the waters, some as the turning of the seasons. The books never say what she is, and she does not seem to mind."
          "Fellowship I.7; The Adventures of Tom Bombadil"
      , card 2.4 "Ungoliant" "Gloomweaver" "of no known kind"
          [ ("child", "the last of her brood was Shelob") ]
          "The Eldar did not know where she came from; some said she was a spirit Melkor corrupted before the world began. With him she killed the Two Trees and drank their light, then turned on him for the jewels, and only his Balrogs saved him."
          "Silmarillion · Of the Darkening of Valinor; Of the Flight of the Noldor"
      ]
  ]

--------------------------------------------------------------------------------
-- A card, set like a page

data Role = Name | Title | Kind | Label | Value | Story | Source deriving Eq

-- | How wide a card's text runs when there's room.
measure :: Int
measure = 58

wrap :: Int -> String -> [String]
wrap width = go . words
  where
    go [] = []
    go (w : ws) = let (line, rest) = fill w ws in line : go rest
    fill line (w : ws) | length line + 1 + length w <= width = fill (line ++ " " ++ w) ws
    fill line ws = (line, ws)

-- | A card's lines at a given width, each a run of pieces in some role.
setCard :: Int -> Node -> Card -> [[(Role, String)]]
setCard width n c =
  [[(Name, nName n)], [(Title, cTitle c)], [], [(Kind, cKind c)]]
  ++ concatMap fact (cFacts c)
  ++ [[]] ++ [ [(Story, l)] | l <- wrap width (cStory c) ]
  ++ [[]] ++ [ [(Source, l)] | l <- wrap width (cSource c) ]
  where
    fact (label, value) = case wrap (width - labelWidth) value of
      first : more -> [(Label, pad label), (Value, first)] : [ [(Label, pad ""), (Value, l)] | l <- more ]
      []           -> []
    labelWidth = 10
    pad s = s ++ replicate (labelWidth - length s) ' '

roleColour :: RGB -> Role -> RGB
roleColour hue = \case
  Name   -> bright
  Title  -> mix hue bright 0.3
  Kind   -> dim
  Label  -> faint
  Value  -> ink
  Story  -> ink
  Source -> dim

regular, bold, italic :: Int
regular = 0
bold    = 1
italic  = 2

roleStyle :: Role -> Int
roleStyle = \case
  Name   -> bold
  Kind   -> italic
  Source -> italic
  _      -> regular

--------------------------------------------------------------------------------
-- Squarified treemap (Bruls, Huizing and van Wijk)

-- | Part of a parent's inner area, as fractions of it.
data Slot = Slot !Double !Double !Double !Double

-- | Every group is laid out as if it filled a screen this much wider than
-- tall, in square units (a cell is about half as wide as it is tall).
aspect :: Double
aspect = 1.9

treemap :: [Double] -> [Slot]
treemap [] = []
treemap weights = [ Slot (x / aspect) y (w / aspect) h | (x, y, w, h) <- squarify areas (0, 0, aspect, 1) ]
  where areas = map (* (aspect / sum weights)) weights

type Box = (Double, Double, Double, Double)

-- | Lay areas (largest first) into a box, a strip at a time along its
-- shorter side, adding to each strip while that makes its worst tile less
-- skinny.
squarify :: [Double] -> Box -> [Box]
squarify [] _ = []
squarify (a : others) box@(_, _, w, h) = placed ++ squarify rest box'
  where
    side = min w h
    (strip, rest) = grow [a] others
    grow acc (next : more) | worst (next : acc) <= worst acc = grow (next : acc) more
    grow acc more = (reverse acc, more)
    worst acc = let s = sum acc in max (side * side * maximum acc / (s * s)) (s * s / (side * side * minimum acc))
    (placed, box') = layStrip strip box

layStrip :: [Double] -> Box -> ([Box], Box)
layStrip strip (x, y, w, h)
  | w >= h    = let cw = total / h in (zipWith (\o a -> (x, y + o, cw, a / cw)) (offsets cw) strip, (x + cw, y, w - cw, h))
  | otherwise = let rh = total / w in (zipWith (\o a -> (x + o, y, a / rh, rh)) (offsets rh) strip, (x, y + rh, w, h - rh))
  where
    total = sum strip
    offsets d = scanl (+) 0 (map (/ d) strip)

--------------------------------------------------------------------------------
-- Geometry

-- | A rectangle on screen, in cells, before rounding.
data Rect = Rect !Double !Double !Double !Double

-- | Which kid to take at each level, from the top.
type Path = [Int]

nodeAt :: Path -> Node
nodeAt = foldl (\n i -> nKids n !! i) ainur

place :: Rect -> Slot -> Rect
place (Rect x y w h) (Slot a b c d) = Rect (x + a * w) (y + b * h) (c * w) (d * h)

-- | The area that puts a slot at this rectangle.
unplace :: Rect -> Slot -> Rect
unplace (Rect x y w h) (Slot a b c d) = Rect (x - a * pw) (y - b * ph) pw ph
  where pw = w / c; ph = h / d

-- | Inside a box's border, and the box around such an inside.
inside, around :: Rect -> Rect
inside (Rect x y w h) = Rect (x + 1) (y + 1) (w - 2) (h - 2)
around (Rect x y w h) = Rect (x - 1) (y - 1) (w + 2) (h + 2)

-- | Where a node lands when the top is at @r@.
descend :: Rect -> Path -> Rect
descend = go ainur
  where
    go _ r [] = r
    go n r (i : is) = go (nKids n !! i) (place (inside r) (nSlots n !! i)) is

-- | Where the top must be for the node at @path@ to land on @r@.
rootFor :: Path -> Rect -> Rect
rootFor path r = foldr up r (zip (scanl (\n i -> nKids n !! i) ainur path) path)
  where up (parent, i) inner = around (unplace inner (nSlots parent !! i))

-- | A card keeps a column of air to its right and a row below, once it's
-- big enough to spare them.
cardArea :: Rect -> Rect
cardArea (Rect x y w h) = Rect x y (w - spare w) (h - spare h)
  where spare d = if d >= 4 then 1 else 0

nearest :: Double -> Int
nearest v = floor (v + 0.5)

-- | Rounded to whole cells: left, top, right, bottom (the last two exclusive).
cells :: Rect -> (Int, Int, Int, Int)
cells (Rect x y w h) = (nearest x, nearest y, nearest (x + w), nearest (y + h))

--------------------------------------------------------------------------------
-- Motion

-- | Between two rectangles the way a camera zooms: about the one point that
-- stays put, the size changing by the same factor each moment, so it reads
-- as moving closer rather than as a box stretching.
zoom :: Double -> Rect -> Rect -> Rect
zoom t (Rect x0 y0 w0 h0) (Rect x1 y1 w1 h1) = Rect x y w h
  where
    (x, w) = axis x0 w0 x1 w1
    (y, h) = axis y0 h0 y1 h1
    axis a0 s0 a1 s1
      | abs (k - 1) < 1e-4 = (a0 + (a1 - a0) * t, s0 + (s1 - s0) * t)
      | otherwise          = (p + (a0 - p) * k ** t, s0 * k ** t)
      where
        k = s1 / s0
        p = (a1 - a0 * k) / (1 - k)   -- the point that stays put

lerpSlot :: Double -> Slot -> Slot -> Slot
lerpSlot t (Slot a b c d) (Slot a' b' c' d') = Slot (l a a') (l b b') (l c c') (l d d')
  where l p q = p + (q - p) * t

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

-- | Most of the way at once, then settling.
easeOut :: Double -> Double
easeOut t = 1 - (1 - t) ^ (5 :: Int)

-- | The camera's curve: 'easeOut' with a little constant speed mixed in, so
-- it comes in to land instead of crawling. A crawl is invisible in itself,
-- but the odd cell it still crosses is not: a tile rounding a column wider
-- re-wraps its text a beat after everything looked still. With this the last
-- cells go by at about one a frame, and then it stops.
land :: Double -> Double
land t = (easeOut t + 0.15 * t) / 1.15

zoomTime, hurriedTime, openingTime, glideTime, fadeTime, oneFrame :: Double
zoomTime    = 0.22
hurriedTime = 0.11
openingTime = 0.5
glideTime   = 0.12
fadeTime    = 0.1
oneFrame    = 1 / 60

-- | Into a kid, or back out of it. Either way the camera holds on the kid,
-- moving it between where it sits in its group and filling the screen.
data Move = Move
  { mvKid    :: Path
  , mvInto   :: Bool
  , mvLength :: Double
  , mvStart  :: Maybe Double   -- once it reaches the front of the queue
  }

--------------------------------------------------------------------------------
-- The app

data Model = Model
  { mAt             :: Path             -- where the camera is, or is heading
  , mSelected       :: Int              -- the selected kid, when that's a group
  , mRemembered     :: M.Map Path Int   -- what was selected in each group, for coming back
  , mMoves          :: [Move]           -- camera moves still to play
  , mNow            :: Double
  , mOpenedAt       :: Maybe Double     -- when the opening zoom began
  , mFocusWas       :: (Path, Int)      -- where focus was, to fade from
  , mFocusMovedAt   :: Double
  , mOutlineFrom    :: Slot             -- where the selection outline set off from
  , mOutlineMovedAt :: Double
  , mOutlineLitAt   :: Double           -- when it last faded in
  }

data Msg
  = Frame Double           -- an animation frame, at its time
  | Pressed Key
  | PressedAt Key Double   -- the same key, stamped with the time

start :: Model
start = Model
  { mAt = [], mSelected = 0, mRemembered = M.empty, mMoves = [], mNow = 0, mOpenedAt = Nothing
  , mFocusWas = ([], 0), mFocusMovedAt = 0
  , mOutlineFrom = Slot 0 0 0 0, mOutlineMovedAt = 0, mOutlineLitAt = 0 }

app :: LayoutzApp Model Msg
app = LayoutzApp
  { appInit = (start, mempty)
  , appUpdate = update
  , appSubscriptions = \m -> subKeyPress (Just . Pressed) <> (if moving m then subAnimationFrame Frame else mempty)
  , appView = \m -> sized (\w h -> text (TL.unpack (scene m (max 30 w) (max 8 h))))
  }

update :: Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
  Frame t -> (frame t m, mempty)
  Pressed (KeyChar 'q') -> (m, cmdQuit)
  -- Stamping the key first means whatever it starts starts now, not back at
  -- the last frame, which may have been a while ago.
  Pressed key -> (m, cmdTask (PressedAt key <$> getMonotonicTime))
  -- Drawn one frame in, so the first frame after a press already moves.
  PressedAt key t -> ((advance (navigate key (advance m { mNow = t }))) { mNow = t + oneFrame }, mempty)

frame :: Double -> Model -> Model
frame t m = case mOpenedAt m of
  Nothing -> m { mNow = t, mOpenedAt = Just t, mOutlineLitAt = t + openingTime }
  Just _  -> advance m { mNow = max t (mNow m) }

here :: Model -> Node
here m = nodeAt (mAt m)

-- | How far along something started at @t0@ is, on a curve.
since :: (Double -> Double) -> Model -> Double -> Double -> Double
since curve m t0 duration = curve (clamp01 ((mNow m - t0) / duration))

-- | Anything still on its way. Frames are only asked for until it has all
-- arrived, so a still screen costs nothing.
moving :: Model -> Bool
moving m = case mOpenedAt m of
  Nothing -> True
  Just t0 -> not (null (mMoves m)) || mNow m < maximum
    [ t0 + openingTime, mFocusMovedAt m + fadeTime
    , mOutlineMovedAt m + glideTime, mOutlineLitAt m + fadeTime ]

-- | The node the camera holds on, where it is on screen, and where the
-- move under way takes it from and to, if one is.
camera :: Model -> Rect -> (Path, Rect, Maybe (Rect, Rect))
camera m screen = case (mMoves m, mOpenedAt m) of
  (Move kid into duration started : _, _) ->
    let sits = place (inside screen) (nSlots (nodeAt (init kid)) !! last kid)
        t = maybe 0 (\t0 -> since land m t0 duration) started
    in if into then travel kid t sits screen else travel kid t screen sits
  ([], Nothing) -> ([], speck, Nothing)
  ([], Just t0) -> travel (mAt m) (since land m t0 openingTime) speck screen
  where
    travel held t from to = (held, zoom t from to, if t < 1 then Just (from, to) else Nothing)
    Rect x y w h = screen
    speck = Rect (x + w / 2 - 2) (y + h / 2 - 1) 4 2

selectedSlot :: Model -> Slot
selectedSlot m = nSlots (here m) !! mSelected m

-- | Where the selection outline is, gliding toward the selected tile.
outlineNow :: Model -> Slot
outlineNow m = lerpSlot (since easeOut m (mOutlineMovedAt m) glideTime) (mOutlineFrom m) (selectedSlot m)

-- | Start the move at the front of the queue, or retire it once it's done.
-- When the last one stops the selection lights up, so nothing moves after.
advance :: Model -> Model
advance m = case mMoves m of
  [] -> m
  mv : rest -> case mvStart mv of
    Nothing -> advance m { mMoves = mv { mvStart = Just (mNow m) } : rest }
    Just t0
      | mNow m < t0 + mvLength mv -> m
      | next : more <- rest       -> m { mMoves = next { mvStart = Just (t0 + mvLength mv) } : more }
      | otherwise                 -> lightUp m { mMoves = [] }

-- | The outline fades in where it belongs. Growing it from the middle would
-- sweep its edges across the text just as everything else settled.
lightUp :: Model -> Model
lightUp m
  | isGroup (here m) = m { mOutlineFrom = selectedSlot m, mOutlineMovedAt = mNow m, mOutlineLitAt = mNow m }
  | otherwise        = m

-- | Queue a move. Keys pressed while one is under way hurry the rest along,
-- so quick fingers never wait on the animation.
enqueue :: Move -> Model -> Model
enqueue mv m = m { mMoves = case mMoves m of
  []         -> [mv]
  cur : rest -> hurry cur : map quick rest ++ [quick mv] }
  where
    hurry c = case mvStart c of
      Just t0 -> let ran = mNow m - t0 in c { mvLength = max ran (min (mvLength c) (ran + 0.06)) }
      Nothing -> quick c
    quick c = c { mvLength = hurriedTime }

-- | Note where focus was, so the change can fade in.
refocus :: Model -> Model
refocus m = m { mFocusWas = (mAt m, mSelected m), mFocusMovedAt = mNow m }

navigate :: Key -> Model -> Model
navigate key m = case key of
  KeyUp        -> arrow (0, -1)
  KeyDown      -> arrow (0, 1)
  KeyLeft      -> arrow (-1, 0)
  KeyRight     -> arrow (1, 0)
  KeyEnter     | isGroup (here m) -> enter (mSelected m) m
  KeyEscape    -> leave m
  KeyBackspace -> leave m
  _            -> m
  where
    -- In a group the outline glides on from wherever it is. On an open card
    -- the camera steps back out and into the neighbour.
    arrow dir
      | isGroup (here m) = maybe m select (neighbour dir (here m) (mSelected m))
      | null (mAt m)     = m
      | otherwise        = maybe m (\i -> enter i (leave m)) (neighbour dir (nodeAt (init (mAt m))) (last (mAt m)))
    select i = (refocus m) { mSelected = i, mOutlineFrom = outlineNow m, mOutlineMovedAt = mNow m }

enter :: Int -> Model -> Model
enter i m = enqueue (Move kid True zoomTime Nothing) (refocus m)
  { mAt = kid
  , mSelected = M.findWithDefault 0 kid (mRemembered m)
  , mRemembered = M.insert (mAt m) i (mRemembered m) }
  where kid = mAt m ++ [i]

leave :: Model -> Model
leave m
  | null (mAt m) = m
  | otherwise = enqueue (Move (mAt m) False zoomTime Nothing) (refocus m)
      { mAt = init (mAt m), mSelected = last (mAt m) }

-- | The nearest kid in a direction, between centres, in square units.
neighbour :: (Double, Double) -> Node -> Int -> Maybe Int
neighbour (dx, dy) n from = snd <$> listToMaybe (sortOn fst candidates)
  where
    slots = nSlots n
    centre (Slot x y w h) = ((x + w / 2) * aspect, y + h / 2)
    (cx, cy) = centre (slots !! from)
    candidates =
      [ (along + 2.5 * abs across, i)
      | (i, s) <- zip [0 ..] slots, i /= from
      , let (ox, oy) = centre s
            along = (ox - cx) * dx + (oy - cy) * dy
            across = (ox - cx) * dy - (oy - cy) * dx
      , along > 1e-3 ]

--------------------------------------------------------------------------------
-- A grid of cells to draw into, then turned into one ANSI string

data Grid s = Grid
  { gWidth, gHeight :: !Int
  , gClip           :: !(Int, Int, Int, Int)   -- left, top, right, bottom; the last two exclusive
  , gChars          :: STUArray s Int Char
  , gFg, gBg        :: STUArray s Int RGB
  , gStyle          :: STUArray s Int Int
  }

type Frozen = (UArray Int Char, UArray Int RGB, UArray Int RGB, UArray Int Int)

newGrid :: Int -> Int -> ST s (Grid s)
newGrid w h = Grid w h (0, 0, w, h) <$> new ' ' <*> new ink <*> new ground <*> new regular
  where new value = newArray (0, w * h - 1) value

freezeGrid :: Grid s -> ST s Frozen
freezeGrid g = (,,,) <$> freeze (gChars g) <*> freeze (gFg g) <*> freeze (gBg g) <*> freeze (gStyle g)

colsIn, rowsIn :: Grid s -> Int -> Int -> [Int]
colsIn g a b = let (x0, _, x1, _) = gClip g in [max a x0 .. min b x1 - 1]
rowsIn g a b = let (_, y0, _, y1) = gClip g in [max a y0 .. min b y1 - 1]

inClip :: Grid s -> Int -> Int -> Bool
inClip g x y = let (x0, y0, x1, y1) = gClip g in x >= x0 && y >= y0 && x < x1 && y < y1

index :: Grid s -> Int -> Int -> Int
index g x y = y * gWidth g + x

-- | A character in a colour and style, over whatever background is there.
put :: Grid s -> Int -> Int -> Char -> RGB -> Int -> ST s ()
put g x y c fg st = when (inClip g x y) $ do
  unsafeWrite (gChars g) (index g x y) c
  unsafeWrite (gFg g) (index g x y) fg
  unsafeWrite (gStyle g) (index g x y) st

charAt :: Grid s -> Int -> Int -> ST s (Maybe Char)
charAt g x y
  | inClip g x y = Just <$> unsafeRead (gChars g) (index g x y)
  | otherwise    = pure Nothing

tintBg :: Grid s -> Int -> Int -> (RGB -> RGB) -> ST s ()
tintBg g x y f = when (inClip g x y) $
  unsafeRead (gBg g) (index g x y) >>= unsafeWrite (gBg g) (index g x y) . f

fillRect :: Grid s -> Int -> Int -> Int -> Int -> RGB -> ST s ()
fillRect g x0 y0 x1 y1 bg = forM_ (rowsIn g y0 y1) $ \y -> forM_ (colsIn g x0 x1) $ \x -> do
  unsafeWrite (gChars g) (index g x y) ' '
  unsafeWrite (gBg g) (index g x y) bg
  unsafeWrite (gStyle g) (index g x y) regular

-- | Text, cut off at @right@. Returns the column after it.
write :: Grid s -> Int -> Int -> Int -> String -> RGB -> Int -> ST s Int
write g x0 y right str fg st = go x0 str
  where
    go !x (c : cs) | x < right = put g x y c fg st >> go (x + 1) cs
    go x _ = pure x

-- | A rounded outline. With @keepTitle@ its top edge goes around any text
-- already there, the way a title sits in a border.
outline :: Grid s -> Bool -> Int -> Int -> Int -> Int -> RGB -> ST s ()
outline g keepTitle x0 y0 x1 y1 colour = do
  let xr = x1 - 1
      yb = y1 - 1
      edge = maybe False (`elem` " ─╭╮")
  forM_ (colsIn g (x0 + 1) xr) $ \x -> do
    this <- charAt g x y0
    left <- charAt g (x - 1) y0
    right <- charAt g (x + 1) y0
    when (not keepTitle || this == Just '─' || (this == Just ' ' && edge left && edge right)) $
      put g x y0 '─' colour regular
    put g x yb '─' colour regular
  forM_ (rowsIn g (y0 + 1) yb) $ \y -> put g x0 y '│' colour regular >> put g xr y '│' colour regular
  put g x0 y0 '╭' colour regular >> put g xr y0 '╮' colour regular
  put g x0 yb '╰' colour regular >> put g xr yb '╯' colour regular

-- | The grid as lines of text. The pen (foreground, background, style) only
-- changes where it has to, and a space has no foreground to change.
toAnsi :: Int -> Int -> Frozen -> TL.Text
toAnsi width height (chars, fgs, bgs, styles) = TB.toLazyText (foldMap line [0 .. height - 1])
  where
    line y = cellsFrom (y * width) (y * width + width) (-1) (-1) (-1)
             <> (if y < height - 1 then TB.singleton '\n' else mempty)
    cellsFrom :: Int -> Int -> RGB -> RGB -> Int -> TB.Builder
    cellsFrom !i !end !fg !bg !st
      | i >= end  = str "\ESC[0m"
      | otherwise = change <> TB.singleton c <> cellsFrom (i + 1) end fg' bg' st'
      where
        c = unsafeAt chars i
        f = unsafeAt fgs i
        b = unsafeAt bgs i
        s = unsafeAt styles i
        blank = c == ' '
        (change, fg', bg', st')
          | not blank && s /= st            = (pen (reset s <> colour 38 f <> str ";" <> colour 48 b), f, b, s)
          | not blank && f /= fg && b /= bg = (pen (colour 38 f <> str ";" <> colour 48 b), f, b, st)
          | b /= bg                         = (pen (colour 48 b), fg, b, st)
          | not blank && f /= fg            = (pen (colour 38 f), f, bg, st)
          | otherwise                       = (mempty, fg, bg, st)
    str = TB.fromString
    pen p = str "\ESC[" <> p <> str "m"
    reset s = str "0;" <> (if s .&. bold /= 0 then str "1;" else mempty) <> (if s .&. italic /= 0 then str "3;" else mempty)
    colour :: Int -> RGB -> TB.Builder
    colour layer c = TB.decimal layer <> str ";2;" <> TB.decimal (c `shiftR` 16) <> str ";"
                     <> TB.decimal ((c `shiftR` 8) .&. 255) <> str ";" <> TB.decimal (c .&. 255)

--------------------------------------------------------------------------------
-- Drawing

-- | How the map is drawn this frame.
data Look = Look
  { lWidth  :: Int
  , lRecede :: Path -> Double   -- how far a node has faded back
  , lLit    :: Maybe Path       -- the selected tile, once the camera has stopped
  , lEnds   :: Maybe (Rect, Rect)  -- where the top starts and ends the move under way
  }

scene :: Model -> Int -> Int -> TL.Text
scene m width height = toAnsi width height $ runST $ do
  g <- newGrid width height
  let screen = Rect 1 1 (fromIntegral width - 2) (fromIntegral height - 3)
      (held, heldAt, ends) = camera m screen
      top = rootFor held heldAt
      settled = null (mMoves m) && maybe False (\t0 -> mNow m >= t0 + openingTime) (mOpenedAt m)
      showOutline = settled && isGroup (here m)
      look = Look
        { lWidth = width
        , lRecede = recede m
        , lLit = if showOutline then Just (mAt m ++ [mSelected m]) else Nothing
        , lEnds = fmap (\(a, b) -> (rootFor held a, rootFor held b)) ends }
      mapArea = g { gClip = (0, 1, width, height - 1) }
  drawNode mapArea look [] ainur top
  when showOutline $ drawSelection mapArea m (descend top (mAt m))
  drawPath g m
  drawHints g m
  freezeGrid g

-- | How far a node fades back. Outside the open group it fades most; inside,
-- everything but the selection a little.
recede :: Model -> Path -> Double
recede m p = from + (to - from) * since easeOut m (mFocusMovedAt m) fadeTime
  where
    from = fadeFor (mFocusWas m)
    to = fadeFor (mAt m, mSelected m)
    fadeFor (at, sel)
      | p `isPrefixOf` at = 0
      | at `isPrefixOf` p = if isGroup (nodeAt at) && p !! length at /= sel then 0.22 else 0
      | otherwise         = 0.65

drawNode :: Grid s -> Look -> Path -> Node -> Rect -> ST s ()
drawNode g look path n r@(Rect x y w h)
  | x + w < 0 || y + h < 0 || x > fromIntegral (gWidth g) || y > fromIntegral (gHeight g) = pure ()
  | c1 - c0 < 1 || r1 - r0 < 1 = pure ()
  | Just c <- nCard n = drawCard g look path n c (cardArea r)
  | otherwise = drawGroup g look path n r
  where (c0, r0, c1, r1) = cells r

drawGroup :: Grid s -> Look -> Path -> Node -> Rect -> ST s ()
drawGroup g look path n r = do
  let (c0, r0, c1, r1) = cells r
      tone shade = mix shade ground (lRecede look path)
      hue = nHue n
  fillRect g c0 r0 c1 r1 (tone (mix ground hue 0.04))
  if c1 - c0 >= 4 && r1 - r0 >= 3
    then do
      outline g False c0 r0 c1 r1 (tone (mix ground hue 0.5))
      -- the outermost box goes unnamed: the path along the top names it
      unless (null path) $
        void (write g (c0 + 2) r0 (c1 - 2) (" " ++ nName n ++ " ") (tone (mix hue bright 0.35)) bold)
      forM_ (zip3 [0 ..] (nKids n) (nSlots n)) $ \(i, kid, slot) ->
        drawNode g look (path ++ [i]) kid (place (inside r) slot)
    else void (write g c0 r0 c1 (nName n) (tone (mix hue ground 0.3)) regular)

-- | A card is set as a page, centred once there's room. A small tile shows
-- only the name and title; the rest comes up as it grows, wrapped to fit.
--
-- While the camera moves, the card keeps the page it has at the bigger end
-- of the move, so nothing reflows on the way. Its corner rides along at
-- the same place in the tile, so the text goes wherever the tile goes, never
-- back. Anything that doesn't fit yet is cut off at the tile's edges.
drawCard :: Grid s -> Look -> Path -> Node -> Card -> Rect -> ST s ()
drawCard g look path n c area@(Rect x y w h) = do
  let (c0, r0, c1, r1) = cells area
      cols = c1 - c0
      tone shade = mix shade ground (lRecede look path)
      lit = lLit look == Just path
      bg = tone (mix ground (nHue n) (if lit then 0.15 else 0.08))
      (shown, left, top) = case lEnds look of
        Nothing -> let s = settle n c area in (sShown s, nearest (sLeft s), nearest (sTop s))
        Just (topA, topB) ->
          let ra@(Rect xa ya wa ha) = cardArea (descend topA path)
              rb@(Rect xb yb wb hb) = cardArea (descend topB path)
              a = settle n c ra
              b = settle n c rb
          in ( sShown (if wb > wa then b else a)
             , ride (xa, wa, sLeft a) (xb, wb, sLeft b) (x, w)
             , ride (ya, ha, sTop a) (yb, hb, sTop b) (y, h) )
      right = c1 - 1
      clarity = clamp01 (fromIntegral (cols - 30) / 24)
      headline role = role == Name || role == Title
      colour role
        | headline role = tone (roleColour (nHue n) role)
        | otherwise     = tone (mix bg (roleColour (nHue n) role) clarity)
      fit s | length s <= right - left = s
            | otherwise                = take (max 0 (right - left - 1)) s ++ "…"
      (x0, y0, x1, y1) = gClip g
      page = g { gClip = (max x0 (c0 + 1), max y0 r0, min x1 right, min y1 r1) }
  fillRect g c0 r0 c1 r1 bg
  forM_ (zip [top ..] shown) $ \(row, pieces) ->
    foldM_ (\col (role, s) -> write page col row right (if headline role then fit s else s) (colour role) (roleStyle role))
           left pieces

-- | How a card sits in a tile once the camera has stopped: the lines it
-- shows, and the corner of the block they make, before rounding. Kept
-- unrounded so a corner held against the tile's edge rounds with that edge
-- all the way through a move, rather than a cell outside it now and then.
data Setting = Setting { sShown :: [[(Role, String)]], sLeft, sTop :: Double }

settle :: Node -> Card -> Rect -> Setting
settle n c area@(Rect x y w h) = Setting shown left top
  where
    (c0, r0, c1, r1) = cells area
    cols = c1 - c0
    rows = r1 - r0
    -- wrapped in steps of four, so a tile a column wider than another
    -- still sets the same page
    width = max 16 (min measure ((cols - 3) `div` 4 * 4))
    page = setCard width n c
    shown = if cols < 30 || rows < 6 then take 2 page else page
    -- centred on where the tile really is rather than on its rounded size
    left = max (x + 1) (x + (w - fromIntegral width) / 2)
    top = max y (y + (h - fromIntegral (length shown)) / 2)

-- | A point in a tile that moves from @pa@ to @pb@ as the tile goes from
-- @(xa, wa)@ to @(xb, wb)@, kept at the same fraction of the tile plus the
-- same offset. The camera moves every such point one way only, so this one
-- never turns back.
ride :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double) -> Int
ride (xa, wa, pa) (xb, wb, pb) (x, w)
  | abs (wb - wa) < 1e-9 = nearest (x + offA)
  | otherwise            = nearest (x + f * w + offA - f * wa)
  where
    offA = pa - xa
    f = (pb - xb - offA) / (wb - wa)

-- | A gold outline around the selected tile, with a soft glow, fading in.
drawSelection :: Grid s -> Model -> Rect -> ST s ()
drawSelection g m groupRect = when (x1 - x0 >= 2 && y1 - y0 >= 2) $ do
  forM_ [y0 - 1 .. y1] $ \y -> forM_ [x0 - 2 .. x1 + 1] $ \x -> do
    let dx = max (x0 - x) (x - x1 + 1)
        dy = max (y0 - y) (y - y1 + 1)
        glow | dx <= 0 && dy <= 0 = 0
             | dx <= 1 && dy <= 1 = 0.09
             | otherwise          = 0.035
    when (glow > 0) $ tintBg g x y (\b -> mix b gold (glow * lit))
  outline g True x0 y0 x1 y1 (mix ground gold lit)
  where
    area = place (inside groupRect) (outlineNow m)
    tile = if isGroup (nKids (here m) !! mSelected m) then area else cardArea area
    (x0, y0, x1, y1) = cells tile
    lit = since easeOut m (mOutlineLitAt m) fadeTime

-- | Where we are, along the top.
drawPath :: Grid s -> Model -> ST s ()
drawPath g m = foldM_ crumb 2 (zip [1 ..] names)
  where
    names = [ nName (nodeAt p) | p <- inits (mAt m) ]
    crumb x (i, name)
      | i == length names = write g x 0 (gWidth g) name bright bold
      | otherwise = write g x 0 (gWidth g) name dim regular >>= \x' -> write g x' 0 (gWidth g) "  ›  " faint regular

-- | What the keys do, along the bottom.
drawHints :: Grid s -> Model -> ST s ()
drawHints g m = void (write g 2 (gHeight g - 1) (gWidth g) hints faint regular)
  where
    hints | isGroup (here m) = "arrows move    enter open    esc back    q quit"
          | otherwise        = "arrows next    esc back    q quit"

main :: IO ()
main = void (run defaultOptions { optQuitKeys = [KeyCtrl 'D'] } app)
