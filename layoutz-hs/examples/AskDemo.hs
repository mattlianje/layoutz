{-# LANGUAGE OverloadedStrings #-}

{-|
Ask demo - a tour of the one-shot @ask*@ prompts. Each takes over the tty
briefly, returns a value, and leaves a single committed line behind.

Run with: cabal run ask-demo
-}

module Main where

import Layoutz
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Text.Printf (printf)

companions :: [String]
companions =
  [ "Bilbo", "Balin", "Dwalin", "Thorin"
  , "Gandalf", "Kili", "Fili", "Bombur"
  , "Bofur", "Gloin", "Oin", "Dori"
  ]

realms :: [String]
realms = ["The Shire", "Rivendell", "Mirkwood", "Lake-town", "Erebor"]

blank :: IO ()
blank = putStrLn ""

main :: IO ()
main = do
  putStr "\ESC[2J\ESC[H"

  _ <- askInput "Name › " "anonymous" ""
  blank
  _ <- askChoose "Choose a realm" realms id
  blank
  _ <- askChooseMany "Pack provisions (up to 3)"
         ["lembas", "pipe-weed", "waybread", "miruvor", "rope", "athelas"] 3 id
  blank
  _ <- askFilter "Search a companion › " companions 8 id
  blank
  _ <- askWrite "Pose a riddle" "This thing all things devours…" "" "Ctrl-D to save"
  blank
  _ <- askFile "." 12
  blank
  _ <- askConfirm "Venture on the quest?" True "Yes" "No"
  blank
  askPager roster 8 True
  blank
  void $ askSpin "Awaking Smaug…" SpinnerDots (threadDelay 1500000 >> pure ("ready" :: String))
  where
    roster = unlines
      [ printf "%2d.  %-12s %s"
          (i + 1) (companions !! (i `mod` length companions))
          (realms !! (i `mod` length realms))
      | i <- [0 .. 39 :: Int] ]
