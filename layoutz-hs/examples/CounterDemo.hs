{-# LANGUAGE OverloadedStrings #-}

{-|
Simple counter demo app using Layoutz TUI runtime.

Press '+' to increment, '-' to decrement.
Press ESC, Ctrl+C, or Ctrl+D to quit.
-}

module Main where

import Layoutz

data CounterMsg = Inc | Dec

counterApp :: LayoutzApp Int CounterMsg
counterApp = LayoutzApp
  { appInit = (0, cmdNone)
  
  , appUpdate = \msg count -> case msg of
      Inc -> (count + 1, cmdNone)
      Dec -> (count - 1, cmdNone)
  
  , appSubscriptions = \_state ->
      subKeyPress $ \key -> case key of
        KeyChar '+' -> Just Inc
        KeyChar '-' -> Just Dec
        _           -> Nothing
  
  , appView = \count ->
      vstack
        [ section "Counter" [text $ "Count: " <> show count]
        , br
        , ul ["Press '+' or '-'", "Press ESC to quit"]
        ]
  }

main :: IO ()
main = runApp counterApp

