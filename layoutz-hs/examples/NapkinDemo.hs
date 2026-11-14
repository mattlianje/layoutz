{-# LANGUAGE OverloadedStrings #-}
import Layoutz

type Model = SelectList (String, Double -> Double)
newtype Msg = Move Model

app :: LayoutzApp Model Msg
app = LayoutzApp
  { appInit          = (selectListOf waves, cmdNone)
  , appUpdate        = \(Move m) _ -> (m, cmdNone)
  , appSubscriptions = const subNone
  , appView          = \m -> row
      [ box "waves" [selectList Move (const (Move m)) (wave m) m]
          & fixedWidth 14
      , box "plot" [sized (plot (selectedItem m)) & fill] & fill ] & gap 0 & fill
  }
  where
    waves =
      [ ("sin",    sin)
      , ("sinc",   \x -> sin x / x)
      , ("beat",   \x -> sin (x / 2) * sin (4 * x))
      , ("damped", \x -> exp (-abs x / 3) * cos (3 * x))
      , ("bell",   \x -> exp (-x * x / 4)) ]
    -- the selected wave gets a highlighted bar across the list
    wave m (name, _)
      | Just name == fmap fst (selectedItem m) = text (" " <> take 7 (name <> repeat ' ')) & styleReverse
      | otherwise                              = text (" " <> name)
    plot Nothing _ _ = ""
    plot (Just (name, f)) w h =
      plotLine (w - 6) (h - 2) [Series [(x, f x) | x <- xs] name ColorCyan]
      where
        n  = 40 * w + 1 :: Int
        xs = [2 * pi * (2 * fromIntegral i / fromIntegral n - 1) | i <- [0 .. n]]

main :: IO ()
main = runApp app
