{-# LANGUAGE OverloadedStrings #-}

{-|
Mouse demo: click the buttons or a fruit, scroll the wheel over the list or
over a stepper. Esc quits.
-}

module Main where

import Layoutz

-- A component is just state, messages, update and view.
newtype Stepper = Stepper Int
data StepperMsg = Up | Down

updateStepper :: StepperMsg -> Stepper -> Stepper
updateStepper Up   (Stepper n) = Stepper (n + 1)
updateStepper Down (Stepper n) = Stepper (max 0 (n - 1))

stepper :: String -> Stepper -> View StepperMsg
stepper label (Stepper n) =
  row [button "-" Down, text (label <> " " <> show n), button "+" Up]
    & onScroll (\d -> if d > 0 then Down else Up)

data Model = Model
  { workers :: Stepper
  , retries :: Stepper
  , picked  :: Int
  , events  :: Int
  }

data Msg = Workers StepperMsg | Retries StepperMsg | Pick Int | Nudge Int | Reset

fruits :: [String]
fruits = ["apple", "banana", "cherry", "damson", "elderberry", "fig"]

update :: Msg -> Model -> Model
update msg m = bump $ case msg of
  Workers s -> m { workers = updateStepper s (workers m) }
  Retries s -> m { retries = updateStepper s (retries m) }
  Pick i    -> m { picked = i }
  Nudge d   -> m { picked = max 0 (min (length fruits - 1) (picked m + d)) }
  Reset     -> initial
  where bump m' = m' { events = events m + 1 }

initial :: Model
initial = Model (Stepper 4) (Stepper 2) 0 0

view :: Model -> View Msg
view m = vstack
  [ section "steppers"
      [ Workers <$> stepper "workers" (workers m)
      , Retries <$> stepper "retries" (retries m)
      ]
  , box "fruit" [ vstack (zipWith fruit [0 ..] fruits) & onScroll Nudge ]
  , row [button "reset" Reset, text ("events: " <> show (events m))]
  , text "click, scroll, Esc to quit" & styleDim
  ]
  where
    fruit i name
      | i == picked m = text ("› " <> padTo name) & fg ColorCyan & onClick (Pick i)
      | otherwise     = text ("  " <> padTo name) & onClick (Pick i)
    padTo s = s <> replicate (12 - length s) ' '

app :: LayoutzApp Model Msg
app = sandbox initial update view

main :: IO ()
main = runApp app
