{-# LANGUAGE OverloadedStrings #-}

{-|
Simple game demo - Collect gems, avoid devils!

Controls:
- WASD or Arrow Keys to move
- R to restart
- ESC to quit
-}

module Main where

import Layoutz

-- | Game state
data GameState = GameState
  { playerX    :: Int
  , playerY    :: Int
  , items      :: [(Int, Int)]
  , score      :: Int
  , message    :: String
  , gameOver   :: Bool
  , level      :: Int
  , enemies    :: [Enemy]
  , lives      :: Int
  , moveCount  :: Int  -- Track moves to slow down enemies
  } deriving (Show)

-- | Enemy that chases the player
data Enemy = Enemy
  { enemyX :: Int
  , enemyY :: Int
  } deriving (Show, Eq)

-- | Game messages
data GameMsg 
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Restart
  | Tick
  deriving (Show, Eq)

-- Constants
gameWidth, gameHeight :: Int
gameWidth = 20
gameHeight = 12

-- | Generate items for a level (deterministic pattern)
generateItems :: Int -> [(Int, Int)]
generateItems level =
  let itemCount = 3 + level
      -- Create a nice scattered pattern based on level
      positions = [ (x `mod` gameWidth, y `mod` gameHeight) 
                  | i <- [0..itemCount-1]
                  , let x = (i * 5 + level * 3) `mod` gameWidth
                  , let y = (i * 3 + level * 2) `mod` gameHeight
                  ]
  in take itemCount positions

-- | Generate enemies for a level (deterministic pattern)
generateEnemies :: Int -> [Enemy]
generateEnemies level =
  let enemyCount = 1 + level
      -- Place enemies in corners and edges, spread based on level
      positions = [ Enemy ((i * 7 + level) `mod` gameWidth) 
                          ((i * 5 + level * 2) `mod` gameHeight)
                  | i <- [0..enemyCount-1]
                  ]
  in positions

-- | Initial game state
initialState :: GameState
initialState = GameState
  { playerX = gameWidth `div` 2
  , playerY = gameHeight `div` 2
  , items = generateItems 1
  , score = 0
  , message = "Collect all gems! Avoid the devils!"
  , gameOver = False
  , level = 1
  , enemies = generateEnemies 1
  , lives = 3
  , moveCount = 0
  }

-- | Move an enemy towards the player (simple chase AI)
moveEnemy :: Enemy -> GameState -> Enemy
moveEnemy enemy state =
  let dx = if playerX state > enemyX enemy then 1
           else if playerX state < enemyX enemy then -1
           else 0
      dy = if playerY state > enemyY enemy then 1
           else if playerY state < enemyY enemy then -1
           else 0
      newX = max 0 (min (gameWidth - 1) (enemyX enemy + dx))
      newY = max 0 (min (gameHeight - 1) (enemyY enemy + dy))
  in Enemy newX newY

-- | Check if player collected an item and update state
processPlayerMove :: GameState -> GameState
processPlayerMove state =
  let playerPos = (playerX state, playerY state)
      newMoveCount = moveCount state + 1
      -- Check for item collection
      collectedItem = playerPos `elem` items state
      newItems = filter (/= playerPos) (items state)
      newScore = if collectedItem then score state + 10 else score state
      
      -- Move enemies towards player only every 3rd move
      newEnemies = if newMoveCount `mod` 3 == 0
                   then map (`moveEnemy` state) (enemies state)
                   else enemies state
      
      -- Check if player hit by enemy
      hitByEnemy = any (\e -> enemyX e == playerX state && enemyY e == playerY state) newEnemies
      
      -- Handle level completion
      levelComplete = collectedItem && null newItems
      
  in if levelComplete then
       let newLevel = level state + 1
       in state { items = generateItems newLevel
                , score = newScore
                , level = newLevel
                , enemies = generateEnemies newLevel
                , message = "Level " <> show newLevel <> "! More enemies!"
                , moveCount = newMoveCount
                }
     else if hitByEnemy then
       let newLives = lives state - 1
       in if newLives <= 0 then
            state { lives = 0
                  , gameOver = True
                  , enemies = newEnemies
                  , message = "💀 Game Over! Press R to restart"
                  , moveCount = newMoveCount
                  }
          else
            state { lives = newLives
                  , enemies = newEnemies
                  , message = "💥 Hit! Lives left: " <> show newLives
                  , moveCount = newMoveCount
                  }
     else if collectedItem then
       state { items = newItems
             , score = newScore
             , enemies = newEnemies
             , message = "Score: " <> show newScore <> " | Gems left: " <> show (length newItems)
             , moveCount = newMoveCount
             }
     else
       state { enemies = newEnemies
             , message = "Score: " <> show (score state) <> " | Lives: " <> show (lives state) <> " | Level: " <> show (level state)
             , moveCount = newMoveCount
             }

-- | Update game state based on message
updateGame :: GameMsg -> GameState -> GameState
updateGame msg state
  | gameOver state && msg /= Restart = state
  | otherwise = case msg of
      MoveUp -> 
        let newY = max 0 (playerY state - 1)
        in processPlayerMove $ state { playerY = newY }
      
      MoveDown ->
        let newY = min (gameHeight - 1) (playerY state + 1)
        in processPlayerMove $ state { playerY = newY }
      
      MoveLeft ->
        let newX = max 0 (playerX state - 1)
        in processPlayerMove $ state { playerX = newX }
      
      MoveRight ->
        let newX = min (gameWidth - 1) (playerX state + 1)
        in processPlayerMove $ state { playerX = newX }
      
      Tick ->
        -- On tick, increment move count and potentially move enemies
        let newMoveCount = moveCount state + 1
            newEnemies = if newMoveCount `mod` 3 == 0
                         then map (`moveEnemy` state) (enemies state)
                         else enemies state
            -- Check if player hit by enemy
            hitByEnemy = any (\e -> enemyX e == playerX state && enemyY e == playerY state) newEnemies
        in if hitByEnemy && lives state > 1
           then state { lives = lives state - 1
                      , enemies = newEnemies
                      , message = "Ouch! Lives left: " <> show (lives state - 1)
                      , moveCount = newMoveCount
                      }
           else if hitByEnemy
           then state { gameOver = True
                      , message = "💀 Game Over! Press R to restart"
                      , moveCount = newMoveCount
                      }
           else state { enemies = newEnemies
                      , moveCount = newMoveCount
                      }
      
      Restart -> initialState

-- | Render the game state
viewGame :: GameState -> L
viewGame state =
  let -- Render game board
      renderCell x y
        | x == playerX state && y == playerY state = "🧙"
        | any (\e -> enemyX e == x && enemyY e == y) (enemies state) = "👹"
        | (x, y) `elem` items state = "💎"
        | otherwise = "⬜"
      
      renderRow y = text $ unwords [renderCell x y | x <- [0..gameWidth - 1]]
      
      gameBoard = layout [renderRow y | y <- [0..gameHeight - 1]]
      
      -- Stats section
      hearts = concat (replicate (lives state) "💖")
      stats = layout
        [ text $ "Score: " <> show (score state)
        , text $ "Lives: " <> hearts
        , text $ "Level: " <> show (level state)
        , text $ "Gems left: " <> show (length (items state))
        ]
      
      -- Game over section
      gameOverSection = if gameOver state
        then section "💀 Game Over"
               [ text $ "Final Score: " <> show (score state)
               , text $ "Reached Level: " <> show (level state)
               , text "Press R to restart!"
               ]
        else text ""
      
      controls = section "Controls"
        [ ul [ "🧙 You | 👹 Devil | 💎 Gems"
             , "WASD or Arrow Keys - Move"
             , "R - Restart | ESC - Quit"
             , "Devils move automatically!"
             ]
        ]
      
  in layout
       [ gameBoard
       , br
       , row [section "Stats" [stats], section "Status" [text $ message state]]
       , gameOverSection
       , br
       , controls
       ]

-- | The game application
simpleGame :: LayoutzApp GameState GameMsg
simpleGame = LayoutzApp
  { appInit = (initialState, CmdNone)
  
  , appUpdate = \msg state -> (updateGame msg state, CmdNone)
  
  , appSubscriptions = \_state ->
      subBatch
        [ subEveryMs 100 Tick
        , subKeyPress $ \key -> case key of
            KeyChar 'w' -> Just MoveUp
            KeyChar 'W' -> Just MoveUp
            KeyUp  -> Just MoveUp
            
            KeyChar 's' -> Just MoveDown
            KeyChar 'S' -> Just MoveDown
            KeyDown -> Just MoveDown
            
            KeyChar 'a' -> Just MoveLeft
            KeyChar 'A' -> Just MoveLeft
            KeyLeft -> Just MoveLeft
            
            KeyChar 'd' -> Just MoveRight
            KeyChar 'D' -> Just MoveRight
            KeyRight -> Just MoveRight
            
            KeyChar 'r' -> Just Restart
            KeyChar 'R' -> Just Restart
            
            _ -> Nothing
        ]
  
  , appView = viewGame
  }

main :: IO ()
main = runApp simpleGame

