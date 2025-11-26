{-# LANGUAGE OverloadedStrings #-}

{-|
Simple counter demo app using Layoutz TUI runtime.

Press '+' to increment, '-' to decrement.
Press ESC, Ctrl+C, or Ctrl+D to quit.
-}

module Main where

import Layoutz

-- | Messages that can change counter state
data CounterMsg = Inc | Dec

-- | The counter application
counterApp :: LayoutzApp Int CounterMsg
counterApp = LayoutzApp
  { appInit = (0, None)
  
  , appUpdate = \msg count -> case msg of
      Inc -> (count + 1, None)
      Dec -> (count - 1, None)
  
  , appSubscriptions = \_state ->
      onKeyPress $ \key -> case key of
        CharKey '+' -> Just Inc
        CharKey '-' -> Just Dec
        _           -> Nothing
  
  , appView = \count ->
      layout
        [ section "Counter" [text $ "Count: " <> show count]
        , br
        , ul ["Press '+' or '-'", "Press ESC to quit"]
        ]
  }

main :: IO ()
main = runApp counterApp

