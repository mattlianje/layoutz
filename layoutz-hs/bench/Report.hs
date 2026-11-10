-- | Writes @results.svg@ and the results section of @README.md@. Only the
-- text between the two markers is replaced, the rest is hand-written.
module Report
  ( Chart (..)
  , Bar (..)
  , Run (..)
  , save
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.Version (showVersion)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Info (fullCompilerVersion)
import System.Process (readProcess)
import Text.Printf (printf)

-- | One small bar chart, one bar per library.
data Chart = Chart
  { chartTitle :: String
  , chartBars  :: [Bar]
  }

data Bar = Bar
  { barName  :: String
  , barValue :: Double   -- sets the length, NaN draws no bar
  , barLabel :: String   -- printed after the bar
  }

data Run = Run
  { runSetup   :: String
  , runCharts  :: [Chart]
  , runHeaders :: [String]
  , runRows    :: [[String]]
  }

save :: FilePath -> Run -> IO ()
save dir run = do
  createDirectoryIfMissing True dir
  date    <- shell "date" ["+%Y-%m-%d"]
  machine <- describeMachine
  writeFile (dir </> svgFile) (svg (date <> " on " <> intercalate ", " machine) run)
  let page = dir </> "README.md"
  exists <- doesFileExist page
  old <- if exists then readFile page else pure ""
  length old `seq` writeFile page (splice (section date machine run) old)
  putStrLn ("\nSaved " <> page <> " and " <> (dir </> svgFile))

svgFile :: FilePath
svgFile = "results.svg"

-- Markdown --------------------------------------------------------------------

startMarker, endMarker :: String
startMarker = "<!-- results:start, written by `make bench`, edits in here are lost -->"
endMarker   = "<!-- results:end -->"

splice :: [String] -> String -> String
splice new old = case break isStart (lines old) of
  (before, _ : rest) | (_, _ : after) <- break isEnd rest ->
    unlines (before <> new <> after)
  _ | null old  -> unlines (intro <> new)
    | otherwise -> old <> "\n" <> unlines new
  where
    isStart = ("<!-- results:start" `isPrefixOf`)
    isEnd   = (endMarker `isPrefixOf`)
    intro =
      [ "# Benchmarks"
      , ""
      , "layoutz, [Bubble Tea](https://github.com/charmbracelet/bubbletea),"
      , "[Brick](https://github.com/jtdaugherty/brick) and"
      , "[ratatui](https://github.com/ratatui/ratatui) drawing the same dashboard"
      , "([Dashboard.hs](Dashboard.hs), [bubbletea/main.go](bubbletea/main.go),"
      , "[brick/Main.hs](brick/Main.hs), [ratatui/src/main.rs](ratatui/src/main.rs))."
      , "How it is measured: [Main.hs](Main.hs)."
      , ""
      , "`make bench` (from `layoutz-hs/`, Linux only) re-runs everything and rewrites"
      , "the chart and the table below. Lower is better everywhere."
      , ""
      ]

section :: String -> [String] -> Run -> [String]
section date machine run = concat
  [ [startMarker, "", "Last run: " <> date, ""]
  , map ("- " <>) (machine <> [runSetup run])
  , ["", "![bench results](" <> svgFile <> ")", ""]
  , map mdRow (headers : map (const "---") headers : runRows run)
  , ["", endMarker]
  ]
  where
    headers = runHeaders run
    mdRow cells = "| " <> intercalate " | " (map escape cells) <> " |"
    escape = concatMap (\c -> if c == '|' then "\\|" else [c])

-- SVG -------------------------------------------------------------------------

svg :: String -> Run -> String
svg stamp run = unlines $
  [ printf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\" font-family=\"-apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif\" font-size=\"12\">" width height width height
  , style
  , textAt "title" 0 18 (intercalate " vs " names <> " (lower is better)")
  , textAt "muted" 0 38 (runSetup run)
  , textAt "muted" 0 56 stamp
  ] <> concat (zipWith chart [0 ..] charts) <> ["</svg>"]
  where
    charts = runCharts run
    names = case charts of
      c : _ -> map (takeWhile (/= ' ') . barName) (chartBars c)
      []    -> []
    columns = 2 :: Int
    panelW = 440 :: Int
    gap = 32 :: Int
    top = 84 :: Int
    rowH = 24 :: Int
    barH = 16 :: Int
    mostBars = maximum (1 : map (length . chartBars) charts)
    panelH = 26 + mostBars * rowH + 22
    panelRows = (length charts + columns - 1) `div` columns
    width = columns * panelW + (columns - 1) * gap
    height = top + panelRows * panelH
    nameW = 16 + 7 * maximum (1 : [ length (barName b) | c <- charts, b <- chartBars c ])
    labelW = 120
    barMax = panelW - nameW - labelW

    chart :: Int -> Chart -> [String]
    chart i (Chart title bars) =
      textAt "heading" x (y + 12) title
        : printf "<line class=\"axis\" x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\"/>" (x + nameW) (y + 22) (x + nameW) (y + 26 + length bars * rowH)
        : concat (zipWith bar [0 ..] bars)
      where
        x = (i `mod` columns) * (panelW + gap)
        y = top + (i `div` columns) * panelH
        largest = maximum (0 : [ v | Bar _ v _ <- bars, not (isNaN v) ])
        bar :: Int -> Bar -> [String]
        bar j (Bar name value label) =
          [ textAt "name" x (by + 12) name
          , if len > 0
              then printf "<rect class=\"%s\" x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"3\"/>" (series name) (x + nameW) by len barH
              else ""
          , textAt "muted" (x + nameW + len + 6) (by + 12) label
          ]
          where
            by = y + 26 + j * rowH
            len | isNaN value || largest <= 0 = 0
                | otherwise = max 2 (round (fromIntegral barMax * value / largest))

    -- same colour per library in every chart
    series name
      | "layoutz" `isPrefixOf` name   = "s1"
      | "bubbletea" `isPrefixOf` name = "s2"
      | "brick" `isPrefixOf` name     = "s3"
      | "ratatui" `isPrefixOf` name   = "s4"
      | otherwise                     = "s5"

textAt :: String -> Int -> Int -> String -> String
textAt cls x y s = printf "<text class=\"%s\" x=\"%d\" y=\"%d\">%s</text>" cls x y (escapeXml s)

escapeXml :: String -> String
escapeXml = concatMap $ \c -> case c of
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  _   -> [c]

style :: String
style = unlines
  [ "<style>"
  , "  .title { font-size: 15px; font-weight: 600; fill: #0b0b0b }"
  , "  .heading { font-weight: 600; fill: #0b0b0b }"
  , "  .name { fill: #0b0b0b }"
  , "  .muted { fill: #52514e }"
  , "  .axis { stroke: #c3c2b7; stroke-width: 1 }"
  , "  .s1 { fill: #2a78d6 } .s2 { fill: #eb6834 } .s3 { fill: #1baf7a } .s4 { fill: #6250d6 } .s5 { fill: #8a8984 }"
  , "  @media (prefers-color-scheme: dark) {"
  , "    .title, .heading, .name { fill: #f0efec }"
  , "    .muted { fill: #c3c2b7 }"
  , "    .axis { stroke: #52514e }"
  , "    .s1 { fill: #3987e5 } .s2 { fill: #d95926 } .s3 { fill: #199e70 } .s4 { fill: #9085e9 } .s5 { fill: #a3a29d }"
  , "  }"
  , "</style>"
  ]

-- Machine ---------------------------------------------------------------------

-- | e.g. ["AMD Ryzen 7 5800X (16 threads)", "Linux 5.15.0-56-generic", "GHC 9.10.3"]
describeMachine :: IO [String]
describeMachine = do
  cpuinfo <- either (const "") id <$> (try (readFile "/proc/cpuinfo") :: IO (Either IOException String))
  kernel  <- shell "uname" ["-r"]
  let ls = lines cpuinfo
      cpu = case [ trim (drop 1 (dropWhile (/= ':') l)) | l <- ls, "model name" `isPrefixOf` l ] of
        (m : _) -> unwords (words m)
        []      -> "unknown CPU"
      threads = length (filter ("processor" `isPrefixOf`) ls)
      cpuPart | threads > 0 = printf "%s (%d threads)" cpu threads
              | otherwise   = cpu
  pure (cpuPart : [ "Linux " <> kernel | not (null kernel) ]
                <> ["GHC " <> showVersion fullCompilerVersion])
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Output of a command, "" if it fails.
shell :: FilePath -> [String] -> IO String
shell cmd args = either (const "") (unwords . words)
  <$> (try (readProcess cmd args "") :: IO (Either IOException String))
