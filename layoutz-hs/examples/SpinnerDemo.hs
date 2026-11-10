{-# LANGUAGE OverloadedStrings #-}

{-|
Spinner demo - Shows different spinner animations

Press any key to advance the animation frame
Press ESC to quit
-}

module Main where

import Layoutz

data SpinnerState = SpinnerState
  { frameCount :: Int
  } deriving (Show)

data SpinnerMsg = NextFrame

spinnerDemo :: LayoutzApp SpinnerState SpinnerMsg
spinnerDemo = LayoutzApp
  { appInit = (SpinnerState 0, cmdNone)

  , appUpdate = \msg state -> case msg of
      NextFrame -> (state { frameCount = frameCount state + 1 }, cmdNone)

  , appSubscriptions = \_state ->
      subEveryMs 100 NextFrame

  , appView = \state ->
      vstack
        [ section "Spinner Styles"
            [ text "Spinners animate automatically!"
            , text ""
            , row [ text "Dots:   ", spinner "Loading..." (frameCount state) SpinnerDots ]
            , row [ text "Line:   ", spinner "Processing" (frameCount state) SpinnerLine ]
            , row [ text "Clock:  ", spinner "Working" (frameCount state) SpinnerClock ]
            , row [ text "Bounce: ", spinner "Thinking" (frameCount state) SpinnerBounce ]
            ]
        , br
        , section "Examples"
            [ text "With colors:"
            , fg ColorGreen $ spinner "Success!" (frameCount state) SpinnerDots
            , fg ColorYellow $ spinner "Warning" (frameCount state) SpinnerLine
            , fg ColorRed $ spinner "Error" (frameCount state) SpinnerBounce
            , br
            , text "Without labels:"
            , row
                [ spinner "" (frameCount state) SpinnerDots
                , text " "
                , spinner "" (frameCount state) SpinnerLine
                , text " "
                , spinner "" (frameCount state) SpinnerClock
                , text " "
                , spinner "" (frameCount state) SpinnerBounce
                ]
            ]
        , br
        , text $ "Frame: " <> show (frameCount state)
        , text "Press ESC to quit"
        ]
  }

main :: IO ()
main = runApp spinnerDemo

