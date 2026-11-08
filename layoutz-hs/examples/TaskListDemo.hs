{-# LANGUAGE OverloadedStrings #-}

{-|
Interactive Task List Demo - Navigate, complete, and add tasks with progress tracking

- Navigate with Arrow Keys or W/S
- Press Enter to start a task (shows progress bar and spinner)
- Press any key to advance progress while task is running
- Press 'A' to add a new task (type name, then digits for duration)
- Press Enter/Tab to confirm new task, ESC to cancel
- Press ESC to quit
-}

module Main where

import Layoutz
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data TaskState = TaskState
  { tasks           :: [String]
  , selected        :: Int
  , completed       :: Set Int
  , spinFrame       :: Int
  , addingTask      :: Bool
  , newTaskName     :: String
  , newTaskDuration :: String
  , taskDurations   :: [(String, Int)]
  , isLoading       :: Bool
  , loadProgress    :: Int
  , loadingTaskIdx  :: Int
  } deriving (Show)

data TaskMsg
  = MoveUp
  | MoveDown
  | StartTask
  | AdvanceProgress
  | ToggleAddTask
  | TypeChar Char
  | TypeDigit Char
  | DeleteChar
  | ConfirmTask
  | CancelAdd
  deriving (Show, Eq)

-- | (name, steps to complete)
initialTasks :: [(String, Int)]
initialTasks =
  [ ("Process data", 15)
  , ("Generate reports", 20)
  , ("Backup files", 12)
  , ("Sync with server", 18)
  , ("Clean temp files", 8)
  , ("Update documentation", 10)
  ]

taskListApp :: App TaskState TaskMsg
taskListApp = App
  { appInit = 
      ( TaskState
          { tasks = map fst initialTasks
          , selected = 0
          , completed = Set.empty
          , spinFrame = 0
          , addingTask = False
          , newTaskName = ""
          , newTaskDuration = ""
          , taskDurations = initialTasks
          , isLoading = False
          , loadProgress = 0
          , loadingTaskIdx = -1
          }
      , cmdNone
      )
  
  , appView = \state ->
      let taskList = zipWith (renderTask state) [0..] (tasks state)
          
          completedCount = Set.size (completed state)
          totalCount = length (tasks state)
          
      in if addingTask state then
           vstack
             [ section "Task Manager" taskList
             , br
             , box "Add New Task"
                 [ text $ "Task name: " <> newTaskName state <> if null (newTaskDuration state) then "_" else ""
                 , text $ "Duration (steps): " <> newTaskDuration state <> if not (null (newTaskDuration state)) then "_" else ""
                 , br
                 , text "Type name (letters), then duration (digits)"
                 , text "Enter/Tab to add, ESC to cancel"
                 ]
             ]
         else if isLoading state then
           let taskName = tasks state !! loadingTaskIdx state
               duration = fromMaybe 1 (lookup taskName (taskDurations state))
               progress = fromIntegral (loadProgress state) / fromIntegral duration
           in vstack
                [ section "Task Manager" taskList
                , br
                , spinner "Processing..." (spinFrame state) SpinnerDots
                , inlineBar taskName progress
                , text $ "Progress: " <> show (loadProgress state) <> " / " <> show duration
                ]
         else
           vstack
             [ section "Task Manager" taskList
             , br
             , section "Status"
                 [ text $ "Completed: " <> show completedCount <> " / " <> show totalCount
                 , br
                 , if completedCount == totalCount
                   then fg ColorGreen $ text "🎉 All tasks completed!"
                   else text "Select a task and press Enter to start"
                 ]
             , br
             , section "Controls"
                 [ ul [ "↑/↓ or W/S - Navigate"
                      , "Enter - Start task"
                      , "A - Add new task"
                      ]
                 ]
             ]
  
  , appUpdate = \msg state -> (case msg of
      MoveUp | not (addingTask state) && not (isLoading state) ->
        let newSelected = if selected state > 0
                         then selected state - 1
                         else length (tasks state) - 1
        in state { selected = newSelected }
      
      MoveDown | not (addingTask state) && not (isLoading state) ->
        let newSelected = if selected state < length (tasks state) - 1
                         then selected state + 1
                         else 0
        in state { selected = newSelected }
      
      StartTask | not (addingTask state) && not (isLoading state) && not (Set.member (selected state) (completed state)) ->
        state { isLoading = True
              , loadProgress = 0
              , loadingTaskIdx = selected state
              , spinFrame = 0
              , addingTask = False
              }
      
      AdvanceProgress | isLoading state ->
        let taskName = tasks state !! loadingTaskIdx state
            duration = fromMaybe 1 (lookup taskName (taskDurations state))
            newProgress = loadProgress state + 1
        in if newProgress >= duration
           then state { isLoading = False
                      , loadProgress = 0
                      , completed = Set.insert (loadingTaskIdx state) (completed state)
                      , spinFrame = spinFrame state + 1
                      }
           else state { loadProgress = newProgress
                      , spinFrame = spinFrame state + 1
                      }
      
      ToggleAddTask | not (isLoading state) ->
        state { addingTask = True
              , newTaskName = ""
              , newTaskDuration = ""
              , isLoading = False
              }
      
      CancelAdd ->
        state { addingTask = False
              , newTaskName = ""
              , newTaskDuration = ""
              , isLoading = False
              }
      
      TypeChar c | addingTask state ->
        state { newTaskName = newTaskName state ++ [c] }
      
      TypeDigit d | addingTask state ->
        state { newTaskDuration = newTaskDuration state ++ [d] }
      
      DeleteChar | addingTask state ->
        if not (null (newTaskDuration state))
        then state { newTaskDuration = init (newTaskDuration state) }
        else if not (null (newTaskName state))
        then state { newTaskName = init (newTaskName state) }
        else state
      
      ConfirmTask | addingTask state && not (null (newTaskName state)) && not (null (newTaskDuration state)) ->
        let duration = read (newTaskDuration state) :: Int
        in state { tasks = tasks state ++ [newTaskName state]
                 , taskDurations = taskDurations state ++ [(newTaskName state, duration)]
                 , addingTask = False
                 , newTaskName = ""
                 , newTaskDuration = ""
                 , isLoading = False
                 }
      
      _ -> state
    , cmdNone)
  
  , appSubscriptions = \state ->
      if isLoading state then
        mconcat
          [ subEveryMs 100 AdvanceProgress
          , subKeyPress $ \_ -> Nothing
          ]
      else if addingTask state then
        subKeyPress $ \key -> case key of
          KeyEnter     -> Just ConfirmTask
          KeyTab       -> Just ConfirmTask
          KeyEscape    -> Just CancelAdd
          KeyBackspace -> Just DeleteChar
          KeyChar c | c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == ' ' -> Just (TypeChar c)
          KeyChar c | c >= '0' && c <= '9' -> Just (TypeDigit c)
          _            -> Nothing
      else
        subKeyPress $ \key -> case key of
          KeyChar 'w'  -> Just MoveUp
          KeyChar 'W'  -> Just MoveUp
          KeyUp   -> Just MoveUp
          
          KeyChar 's'  -> Just MoveDown
          KeyChar 'S'  -> Just MoveDown
          KeyDown -> Just MoveDown
          
          KeyEnter     -> Just StartTask
          
          KeyChar 'a'  -> Just ToggleAddTask
          KeyChar 'A'  -> Just ToggleAddTask
          
          _            -> Nothing
  }
  where
    renderTask :: TaskState -> Int -> String -> View m
    renderTask state idx task =
      let isSelected = idx == selected state
          isCompleted = Set.member idx (completed state)
          isCurrentlyLoading = isLoading state && idx == loadingTaskIdx state
          
          emoji = if isCompleted then "✅"
                  else if isCurrentlyLoading then "⚡"
                  else "📋"
          marker = if isSelected then "► " else "  "
          
          taskText = marker <> emoji <> " " <> task
          
      in if isSelected
         then fg ColorCyan $ text taskText
         else text taskText

main :: IO ()
main = runApp taskListApp

