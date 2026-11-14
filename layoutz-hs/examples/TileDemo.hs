{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Tile Demo - a tiling workspace of agent sessions

Every tile is a chat with an agent. The replies are canned, but the shell
commands the agent runs are real.

- Ctrl-N        split the focused tile and open a new session
- Ctrl-W        close the focused tile
- Ctrl-F        zoom the focused tile (again to unzoom)
- Ctrl-P        fuzzy-find sessions, files, symbols and commits
- Tab / click   move focus
- Ctrl-D        quit

  cabal run tile-demo
  cabal run tile-demo -- --stacked    -- first split top/bottom, for tall terminals
-}

module Main where

import Control.Exception (IOException, handle)
import Control.Monad (void)
import Data.Char (isAlphaNum, isLower, isUpper, toLower)
import Data.List (isInfixOf, sortOn, stripPrefix, tails)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import Layoutz

------------------------------------------------------------------------------
-- Layout tree

data Axis = H | V deriving Eq

-- A split has a resting ratio and the ratio it's currently drawn at. The
-- drawn one eases toward where it should be, which is what animates new
-- tiles and zooming.
data Layout = Tile Int | Split Axis Double Double Layout Layout

leaves :: Layout -> [Int]
leaves (Tile i)          = [i]
leaves (Split _ _ _ x y) = leaves x <> leaves y

-- Split tile i in two, new tile second. It starts out at zero size and
-- slides in.
grow :: Axis -> Int -> Int -> Layout -> Layout
grow axis i new = go
  where
    go (Tile j) | j == i = Split axis 0.5 1 (Tile j) (Tile new)
    go (Split a r c x y) = Split a r c (go x) (go y)
    go l                 = l

-- Remove tile i. Its sibling gets the space.
close :: Int -> Layout -> Layout
close i = go
  where
    go (Split _ _ _ (Tile j) y) | j == i = y
    go (Split _ _ _ x (Tile j)) | j == i = x
    go (Split a r c x y)                 = Split a r c (go x) (go y)
    go l                                 = l

-- Where a split wants to be. When zoomed, every split above the focused
-- tile opens all the way towards it.
target :: Bool -> Int -> Layout -> Double
target _ _ (Tile _) = 0
target zoomedIn f (Split _ r _ x y)
  | zoomedIn && f `elem` leaves x = 1
  | zoomedIn && f `elem` leaves y = 0
  | otherwise                     = r

ease :: Double -> Bool -> Int -> Layout -> Layout
ease _ _ _ l@(Tile _) = l
ease dt z f s@(Split a r c x y) = Split a r c' (ease dt z f x) (ease dt z f y)
  where
    t     = target z f s
    moved = t + (c - t) * exp (-easeRate * dt)
    c'    = if abs (moved - t) < 0.002 then t else moved

easeRate :: Double
easeRate = 14

settled :: Bool -> Int -> Layout -> Bool
settled _ _ (Tile _) = True
settled z f s@(Split _ _ c x y) = c == target z f s && settled z f x && settled z f y

-- Each new split goes the other way from the one it's inside, so tiles
-- spiral inwards like bspwm's dwindle layout.
splitAxis :: Axis -> Int -> Layout -> Axis
splitAxis firstAxis i = go firstAxis
  where
    go a (Tile _) = a
    go _ (Split a _ _ x y)
      | i `elem` leaves x = go (flipAxis a) x
      | otherwise         = go (flipAxis a) y

flipAxis :: Axis -> Axis
flipAxis H = V
flipAxis V = H

-- Draw the layout into exactly w by h cells. Tiles squeezed down to nothing
-- still get drawn (at size 0) so they stay in the focus order.
drawLayout :: (Int -> Int -> Int -> View msg) -> Int -> Int -> Layout -> View msg
drawLayout drawTile w h = \case
  Tile i -> drawTile i w h
  Split H _ c x y ->
    let k = cut c w
    in row [drawLayout drawTile k h x, drawLayout drawTile (w - k) h y] & gap 0
  Split V _ c x y ->
    let k = cut c h
    in vstack [drawLayout drawTile w k x, drawLayout drawTile w (h - k) y]
  where
    cut c n = max 0 (min n (round (c * fromIntegral n)))

------------------------------------------------------------------------------
-- Agent sessions

-- Things the agent is going to do
data Step = Think Int | Say String | Run String | Edit String [String]

-- Things already in the transcript
data Entry
  = You String
  | Said String
  | Ran String (Maybe [String])   -- Nothing while the command is running
  | Edited String [String]

data Chat = Chat
  { input    :: String
  , past     :: [Entry]
  , script   :: [Step]   -- what the agent still has to do
  , typed    :: Int      -- how far into the current step it is
  , scrolled :: Int      -- lines scrolled back from the bottom
  }

emptyChat :: Chat
emptyChat = Chat { input = "", past = [], script = [], typed = 0, scrolled = 0 }

addEntry :: Entry -> Chat -> Chat
addEntry e c = c { past = past c <> [e] }

-- A session is named after the first thing you asked it
topic :: Chat -> String
topic c = case [ q | You q <- past c ] of
  q : _ -> q
  []    -> "new session"

label :: Int -> Chat -> String
label i c = "#" <> show i <> " " <> topic c

status :: Chat -> String
status c = case script c of
  Think _ : _  -> "thinking"
  Say _ : _    -> "writing"
  Run _ : _    -> "running"
  Edit _ _ : _ -> "editing"
  []
    | null (past c) -> ""
    | otherwise     -> "idle"

suggestions :: [String]
suggestions =
  [ "is there a space leak in the stream loop?"
  , "typecheck this with -Wall -Wextra"
  , "what changed lately?"
  ]

replyTo :: String -> [Step]
replyTo q
  | has "leak" =
      [ Think 16
      , Say "Checking how a transcript grows."
      , Run "grep -n -A1 '^addEntry ::' examples/TileDemo.hs"
      , Think 10
      , Say "Probably not a leak: every field gets forced when the tile redraws, so thunks don't pile up. The real cost is past c <> [e], which copies the whole list on every append. A Seq makes that O(1):"
      , Edit "examples/TileDemo.hs"
          [ "+import Data.Sequence (Seq, (|>))"
          , "-  , past     :: [Entry]"
          , "+  , past     :: Seq Entry"
          , "-addEntry e c = c { past = past c <> [e] }"
          , "+addEntry e c = c { past = past c |> e }"
          ]
      , Say "The other uses of past are folds, so they don't change. Want me to apply it?"
      ]
  | has "functor" || has "tolist" =
      [ Think 12
      , Say "Yes. Parameterise the leaf and let GHC derive the rest:"
      , Edit "examples/TileDemo.hs"
          [ "+{-# LANGUAGE DeriveTraversable #-}"
          , "+import Data.Foldable (toList)"
          , "-data Layout = Tile Int | Split Axis Double Double Layout Layout"
          , "+data Layout a = Tile a | Split Axis Double Double (Layout a) (Layout a)"
          , "+  deriving (Functor, Foldable, Traversable)"
          , "-leaves :: Layout -> [Int]"
          , "-leaves (Tile i)          = [i]"
          , "-leaves (Split _ _ _ x y) = leaves x <> leaves y"
          , "+leaves :: Layout a -> [a]"
          , "+leaves = toList"
          ]
      , Run "grep -c 'Layout ->' examples/TileDemo.hs"
      , Say "That many signatures become Layout Int. grow and close keep their bodies."
      ]
  | has "wall" || has "warn" || has "typecheck" =
      [ Think 8
      , Run "ghc -fno-code -fforce-recomp -Wall -Wextra -i. examples/TileDemo.hs 2>&1 | tail -4"
      , Say "Clean under -Wall -Wextra."
      ]
  | has "zoom" =
      [ Think 14
      , Say "Let me find where the zoom is animated."
      , Run "grep -n -A8 '^ease ::' examples/TileDemo.hs"
      , Think 8
      , Say "Splits ease toward their target at a rate of 14 per second. Bumping it to 22 makes zooming feel snappier without looking jumpy:"
      , Edit "examples/TileDemo.hs" [" easeRate :: Double", "-easeRate = 14", "+easeRate = 22"]
      , Say "For scale, this whole demo is"
      , Run "wc -l examples/TileDemo.hs"
      ]
  | has "big" || has "size" || has "lines" =
      [ Think 10
      , Say "Counting the library and its examples."
      , Run "wc -l Layoutz.hs examples/*.hs | sort -n | tail -6"
      , Say "The whole library is one file, and only depends on text and containers."
      ]
  | otherwise =
      [ Think 10
      , Say "Here's the recent history of this branch."
      , Run "git log --oneline -8"
      , Run "git diff --stat"
      , Say "Want me to dig into any of these?"
      ]
  where
    has k = k `isInfixOf` map toLower q

-- Move session i's agent along one tick: think, type a few more characters,
-- or start a command (it reports back with ToolDone).
advance :: Int -> Chat -> (Chat, Cmd Msg)
advance i c = case script c of
  Think n : rest
    | typed c < n -> (c { typed = typed c + 1 }, cmdNone)
    | otherwise   -> (nextStep rest, cmdNone)
  Say s : rest
    | typed c < length s -> (c { typed = typed c + 3 }, cmdNone)
    | otherwise          -> (addEntry (Said s) (nextStep rest), cmdNone)
  Run cmd : _
    | typed c == 0 -> (addEntry (Ran cmd Nothing) (c { typed = 1 }), cmdTask (ToolDone i <$> shell cmd))
  Edit file diff : rest -> (addEntry (Edited file diff) (nextStep rest), cmdNone)
  _ -> (c, cmdNone)
  where
    nextStep rest = c { script = rest, typed = 0 }

-- Fill in the output of the command that's running
toolDone :: [String] -> Chat -> Chat
toolDone out c = case reverse (past c) of
  Ran cmd Nothing : earlier ->
    c { past = reverse (Ran cmd (Just out) : earlier), script = drop 1 (script c), typed = 0 }
  _ -> c

typeKey :: Key -> Chat -> Chat
typeKey k c = case k of
  KeyChar ch   -> c { input = input c <> [ch] }
  KeyBackspace -> c { input = take (length (input c) - 1) (input c) }
  KeyEnter
    | not (null (input c)) && null (script c) ->
        (addEntry (You (input c)) c) { input = "", script = replyTo (input c), typed = 0, scrolled = 0 }
  _ -> c

-- Run a command for the agent, stdout and stderr together
shell :: String -> IO [String]
shell cmd = handle (\(e :: IOException) -> pure [show e]) $ do
  (_, out, err) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  pure (lines (out <> err))

-- Run a command for ourselves, nothing if it fails
stdoutOf :: String -> IO [String]
stdoutOf cmd = handle (\(_ :: IOException) -> pure []) $ do
  (code, out, _) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  pure (if code == ExitSuccess then lines out else [])

------------------------------------------------------------------------------
-- Ctrl-P

-- Fuzzy match: the query's letters have to appear in order. Returns a cost
-- (lower is better) and the matched positions for highlighting. Gaps between
-- letters cost, landing on the start of a word is a bonus, and long strings
-- or late starts cost a bit. Every place the first letter shows up is tried.
fuzzy :: String -> String -> Maybe (Int, [Int])
fuzzy [] _ = Just (0, [])
fuzzy q@(q0 : _) s = listToMaybe (sortOn fst candidates)
  where
    chars = zip3 [0 :: Int ..] (map toLower s) (zipWith wordStart (' ' : s) s)
    wordStart prev ch = not (isAlphaNum prev) || (isLower prev && isUpper ch)

    candidates =
      [ match
      | start@((_, ch, _) : _) <- tails chars
      , ch == toLower q0
      , Just match <- [walk (map toLower q) start Nothing 0 []]
      ]

    walk [] _ _ cost hits = Just (cost + length s `div` 10 + min 4 (last hits), reverse hits)
    walk _ [] _ _ _ = Nothing
    walk (c : cs) ((i, ch, isStart) : rest) prev cost hits
      | c == ch   = walk cs rest (Just i) (cost + gapCost - startBonus) (i : hits)
      | otherwise = walk (c : cs) rest prev cost hits
      where
        gapCost    = maybe 0 (\p -> 2 * (i - p - 1)) prev
        startBonus = if isStart then 3 else 0

-- Something Ctrl-P can find. Picking a session jumps to it, picking anything
-- else mentions it in the focused session.
data Kind = Session Int | File | Symbol | Commit

data Item = Item
  { kind  :: Kind
  , key   :: String   -- the part the query matches against
  , extra :: String   -- shown after the key but not matched, e.g. a type
  , tag   :: String
  }

data Palette = Palette { query :: String, picked :: Int }

items :: Model -> [Item]
items m = sessions <> index m
  where
    sessions = [ Item (Session i) (label i c) "" (status c) | (i, c) <- M.toList (chats m) ]

matches :: Model -> String -> [(Item, [Int])]
matches m q = map snd (sortOn fst scored)
  where
    scored = [ (cost, (it, hits)) | it <- items m, Just (cost, hits) <- [fuzzy q (key it)] ]

-- Files in the repo, top-level signatures in the library and examples, and
-- the git log
loadIndex :: IO [Item]
loadIndex = do
  files   <- stdoutOf "git -C .. ls-files --cached --others --exclude-standard"
  sigs    <- stdoutOf "grep -HE \"^[a-z][A-Za-z0-9_']* ::\" Layoutz.hs examples/*.hs"
  commits <- stdoutOf "git log --format='%h %s'"
  pure $  [ Item File f "" "file" | f <- files, not (isScratch f) ]
       <> concatMap symbol sigs
       <> [ Item Commit c "" "commit" | c <- commits ]
  where
    isScratch = any (`elem` "#~")   -- editor autosaves and backups

    -- "examples/Foo.hs:bar :: Int" becomes bar, " :: Int", tagged Foo
    symbol l = case break (== ':') l of
      (path, _ : decl) ->
        let (name', ty) = break (== ' ') decl
        in [Item Symbol name' ty (moduleName path)]
      _ -> []

    moduleName = takeWhile (/= '.') . reverse . takeWhile (/= '/') . reverse

------------------------------------------------------------------------------
-- App

data Model = Model
  { layout     :: Layout
  , chats      :: M.Map Int Chat
  , focus      :: Int
  , nextId     :: Int
  , zoomed     :: Bool
  , palette    :: Maybe Palette
  , lastFrame  :: Maybe Double   -- set while something is animating
  , tick       :: Int            -- drives the spinners
  , index      :: [Item]         -- what Ctrl-P searches besides the sessions
  , firstSplit :: Axis
  }

data Msg
  = NewTile
  | CloseTile
  | Zoom
  | Focused String
  | FocusOn Int
  | Frame Double
  | Stream
  | Typed Int Key
  | Scroll Int Int
  | ToolDone Int [String]
  | OpenPalette
  | PaletteKey Key
  | Pick Item
  | Indexed [Item]

initialModel :: Axis -> Model
initialModel axis = Model
  { layout     = Tile 0
  , chats      = M.singleton 0 emptyChat
  , focus      = 0
  , nextId     = 1
  , zoomed     = False
  , palette    = Nothing
  , lastFrame  = Nothing
  , tick       = 0
  , index      = []
  , firstSplit = axis
  }

update :: Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
  NewTile ->
    let i    = nextId m
        axis = splitAxis (firstSplit m) (focus m) (layout m)
    in ( m { layout = grow axis (focus m) i (layout m)
           , chats  = M.insert i emptyChat (chats m)
           , nextId = i + 1
           , focus  = i
           , zoomed = False
           }
       , cmdFocus (name i) )

  CloseTile
    | length (leaves (layout m)) == 1 -> (m, cmdNone)
    | otherwise ->
        let layout' = close (focus m) (layout m)
            -- focus the tile before the closed one, or the first if there isn't one
            before  = reverse (takeWhile (/= focus m) (leaves (layout m)))
            f       = case before <> leaves layout' of
              t : _ -> t
              []    -> 0
        in ( m { layout = layout', chats = M.delete (focus m) (chats m), focus = f, zoomed = False }
           , cmdFocus (name f) )

  Zoom -> (m { zoomed = not (zoomed m) }, cmdNone)

  Focused n -> case stripPrefix "tile" n >>= readMaybe of
    Just i  -> (m { focus = i }, cmdNone)
    Nothing -> (m, cmdNone)

  FocusOn i -> (m { focus = i, palette = Nothing }, cmdFocus (name i))

  Frame t ->
    let dt      = maybe (1 / 60) (\t0 -> min 0.05 (t - t0)) (lastFrame m)
        layout' = ease dt (zoomed m) (focus m) (layout m)
        done    = settled (zoomed m) (focus m) layout'
    in (m { layout = layout', lastFrame = if done then Nothing else Just t }, cmdNone)

  Stream ->
    let stepped = M.mapWithKey advance (chats m)
    in ( m { chats = M.map fst stepped, tick = tick m + 1 }
       , mconcat (map snd (M.elems stepped)) )

  Typed i k      -> (onChat i (typeKey k) m, cmdNone)
  Scroll i d     -> (onChat i (\c -> c { scrolled = max 0 (scrolled c - d) }) m, cmdNone)
  ToolDone i out -> (onChat i (toolDone out) m, cmdNone)

  OpenPalette -> case palette m of
    Nothing -> (m { palette = Just (Palette "" 0) }, cmdFocus "palette")
    Just _  -> (m { palette = Nothing }, cmdFocus (name (focus m)))

  PaletteKey k -> case palette m of
    Just p  -> paletteKey k p m
    Nothing -> (m, cmdNone)

  Pick it ->
    let mention s = onChat (focus m) (\c -> c { input = input c <> s <> " " }) m
        m' = case kind it of
          Session i -> m { focus = i }
          File      -> mention ("@" <> key it)
          Symbol    -> mention (key it)
          Commit    -> mention (takeWhile (/= ' ') (key it))   -- just the hash
    in (m' { palette = Nothing }, cmdFocus (name (focus m')))

  Indexed is -> (m { index = is }, cmdNone)

paletteKey :: Key -> Palette -> Model -> (Model, Cmd Msg)
paletteKey k p m = case k of
  KeyChar ch   -> setQuery (query p <> [ch])
  KeyBackspace -> setQuery (take (length (query p) - 1) (query p))
  KeyUp        -> move (-1)
  KeyDown      -> move 1
  KeyEscape    -> (m { palette = Nothing }, cmdFocus (name (focus m)))
  KeyEnter     -> case drop (picked p) found of
    (it, _) : _ -> update (Pick it) m
    []          -> (m, cmdNone)
  _ -> (m, cmdNone)
  where
    found      = matches m (query p)
    setQuery q = (m { palette = Just (Palette q 0) }, cmdNone)
    move d     =
      let i = max 0 (min (length found - 1) (picked p + d))
      in (m { palette = Just p { picked = i } }, cmdNone)

onChat :: Int -> (Chat -> Chat) -> Model -> Model
onChat i f m = m { chats = M.adjust f i (chats m) }

subscriptions :: Model -> Sub Msg
subscriptions m = mconcat
  [ subKeyPress $ \case
      KeyCtrl 'W' -> Just CloseTile
      KeyCtrl 'F' -> Just Zoom
      KeyCtrl 'P' -> Just OpenPalette
      _           -> Nothing
  , subFocus Focused
  , if anyBusy then subEveryMs 30 Stream else subNone
  , if animating then subAnimationFrame Frame else subNone
  ]
  where
    anyBusy   = any (not . null . script) (chats m)
    animating = not (settled (zoomed m) (focus m) (layout m))

------------------------------------------------------------------------------
-- View

ink, faint, sky, amber, good, bad, rimOn, rimOff :: Color
ink    = ColorTrue 212 216 232
faint  = ColorTrue 108 116 148
sky    = ColorTrue 122 200 255
amber  = ColorTrue 250 200 110
good   = ColorTrue 130 220 150
bad    = ColorTrue 245 120 120
rimOn  = sky
rimOff = ColorTrue 64 70 96

ln :: Color -> String -> View m
ln c s = text s & fg c

name :: Int -> String
name i = "tile" <> show i

view :: Model -> View Msg
view m = sized $ \w h ->
  let tiles = vstack [drawLayout (pane m) w (h - 1) (layout m), statusBar m w]
  in case palette m of
       Nothing -> tiles
       Just p  -> overlay (tiles & styleDim) (paletteView m w p)

statusBar :: Model -> Int -> View Msg
statusBar m w = row [text title & styleReverse & fg sky, ln faint hints'] & gap 0
  where
    title  = " agents · " <> show (M.size (chats m)) <> (if zoomed m then " · zoomed " else " ")
    hints  = "  ^N new  ^P find  ^F zoom  ^W close  Tab/click focus  ^D quit"
    hints' = take (w - length title) (hints <> repeat ' ')

pane :: Model -> Int -> Int -> Int -> View Msg
pane m i w h = case M.lookup i (chats m) of
  Just c | w >= 12 && h >= 4 ->
    let active = focus m == i && null (palette m)
        title  = take (w - 6) (" " <> (if active then "● " else "") <> label i c <> " ")
        hint   = suggestions !! (i `mod` length suggestions)
        body   = chatView active (tick m) hint (w - 4) (h - 2) c
    in box title [vstack body & fg ink]
         & border BorderRound
         & fg (if active then rimOn else rimOff)
         & fixedWidth w & fixedHeight h
         & focusable (name i)
         & onClick (FocusOn i)
         & onKey (tileKey i)
         & onScroll (Scroll i)
  -- too small to draw, but it still needs to be focusable
  _ -> text "" & focusable (name i) & fixedWidth w & fixedHeight h

tileKey :: Int -> Key -> Maybe Msg
tileKey i = \case
  KeyCtrl 'N'   -> Just NewTile
  k@(KeyChar _) -> Just (Typed i k)
  KeyBackspace  -> Just (Typed i KeyBackspace)
  KeyEnter      -> Just (Typed i KeyEnter)
  _             -> Nothing

paletteView :: Model -> Int -> Palette -> View Msg
paletteView m w p =
  box " find " ([header, ln rimOff (replicate innerW '─')] <> rows)
    & border BorderRound
    & fg rimOn
    & fixedWidth (innerW + 4)
    & focusable "palette"
    & onKey (Just . PaletteKey)
  where
    innerW  = max 24 (min 76 (w - 10))
    found   = matches m (query p)
    count   = show (length found) <> "/" <> show (length (items m))
    spaces  = max 1 (innerW - 3 - length (query p) - length count)
    header  = row [ln sky "› ", ln ink (query p <> "█"), text (replicate spaces ' '), ln faint count] & gap 0

    visible = 12
    top     = max 0 (picked p - visible + 1)
    listed
      | null found = [ln faint "nothing matches"]
      | otherwise  = zipWith (\n r -> paletteRow innerW (n == picked p) r) [top ..] (take visible (drop top found))
    -- pad to a fixed height so the box doesn't jump around as you type
    rows = take visible (listed <> repeat (text ""))

paletteRow :: Int -> Bool -> (Item, [Int]) -> View Msg
paletteRow innerW isPicked (it, hits) =
  row [marker, letters, padding, ln tagColor (tag it)] & gap 0 & onClick (Pick it)
  where
    room    = innerW - length (tag it) - 3
    shown   = take room (key it <> extra it)
    marker  = if isPicked then ln sky "▌ " else text "  "
    letters = row [ ln (shade j) [ch] | (j, ch) <- zip [0 ..] shown ] & gap 0
    padding = text (replicate (room - length shown + 1) ' ')

    shade j
      | j `elem` hits        = amber
      | j >= length (key it) = rimOff
      | isPicked             = ink
      | otherwise            = faint

    tagColor = case kind it of
      Session _ | tag it /= "idle" -> good
      Symbol                       -> sky
      _                            -> faint

spinChar :: Int -> Char
spinChar n = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" !! (n `mod` 10)

-- A session drawn into exactly w by h: transcript, a rule, then the prompt
chatView :: Bool -> Int -> String -> Int -> Int -> Chat -> [View m]
chatView active spin hint w h c =
  shown <> replicate (room - length shown) (text "") <> [ln rimOff (replicate w '─'), prompt]
  where
    room   = max 0 (h - 2)
    lines' = if null (past c) then welcome else concatMap entry (past c) <> inProgress
    shown  = takeEnd room (dropEnd (scrolled c) lines')
    cursor = if active then "█" else ""
    prompt = row [ln sky "› ", ln ink (takeEnd (w - 3) (input c) <> cursor)] & gap 0

    welcome = map (ln faint) (wrapText w ("Try: " <> hint))

    inProgress = case script c of
      Think _ : _ -> [ln amber (spinChar spin : " thinking")]
      Say s : _   -> map (ln ink) (wrapText w (take (typed c) s <> "▌"))
      _           -> []

    entry = \case
      You s ->
        zipWith (sentLine w) ("› " : repeat "  ") (wrapText (w - 3) s) <> [text ""]
      Said s ->
        map (ln ink) (wrapText w s) <> [text ""]
      Ran cmd Nothing ->
        [ln amber (take w (spinChar spin : " bash  " <> cmd))]
      Ran cmd (Just out) ->
        ln good (take w ("✓ bash  " <> cmd)) : map (ln faint . take w . ("  │ " <>)) out <> [text ""]
      Edited file diff ->
        ln sky (take w ("✎ edit  " <> file)) : map diffLine diff <> [text ""]

    diffLine l = ln (diffColor l) (take w ("  │ " <> l))
    diffColor ('+' : _) = good
    diffColor ('-' : _) = bad
    diffColor _         = faint

-- One line of something you sent, on a highlighted strip the width of the
-- tile. layoutz has no background colour combinator yet, so this writes the
-- SGR codes by hand.
sentLine :: Int -> String -> String -> View m
sentLine w lead s =
  text (sgr "48;2;44;46;64" <> sgr "38;2;122;200;255" <> " " <> lead
        <> sgr "38;2;212;216;232" <> s <> padding <> sgr "0")
  where
    padding = replicate (w - 1 - length lead - length s) ' '
    sgr p   = "\ESC[" <> p <> "m"

------------------------------------------------------------------------------
-- Helpers

wrapText :: Int -> String -> [String]
wrapText w = go . words
  where
    go []       = [""]
    go (x : xs) = line (take w x) xs

    line cur [] = [cur]
    line cur (y : ys)
      | length cur + 1 + length y <= w = line (cur <> " " <> y) ys
      | otherwise                      = cur : line (take w y) ys

takeEnd, dropEnd :: Int -> [a] -> [a]
takeEnd n xs = drop (length xs - n) xs
dropEnd n xs = take (length xs - n) xs

------------------------------------------------------------------------------

app :: Axis -> LayoutzApp Model Msg
app firstAxis = LayoutzApp
  { appInit          = (initialModel firstAxis, cmdFocus (name 0) <> cmdTask (Indexed <$> loadIndex))
  , appUpdate        = update
  , appSubscriptions = subscriptions
  , appView          = view
  }

main :: IO ()
main = do
  args <- getArgs
  let firstAxis = if "--stacked" `elem` args then V else H
  void (run defaultOptions { optQuitKeys = [KeyCtrl 'D'] } (app firstAxis))
