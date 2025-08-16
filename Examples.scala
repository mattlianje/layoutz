package layoutz

/** Example applications demonstrating LayoutzApp usage */
object Examples {

  /** Simple counter application that responds to + and - key presses */
  object CounterApp extends LayoutzApp[Int, String] {
    def init = 0

    def update(msg: String, count: Int): Int = msg match {
      case "inc" => count + 1
      case "dec" => count - 1
      case _     => count
    }

    def onKey(k: Key): Option[String] = k match {
      case CharKey('+') => Some("inc")
      case CharKey('-') => Some("dec")
      case _            => None
    }

    def view(count: Int): Element =
      layout(
        section("Counter")(
          layout(
            Text(s"Current count: $count"),
            Text("Press + / - to adjust")
          )
        )
      )
  }

  // Todo app with proper text input
  case class TodoState(
      items: List[String],
      completed: Set[Int],
      inputText: String,
      inputMode: Boolean
  )

  sealed trait TodoMessage
  case class ToggleItem(index: Int) extends TodoMessage
  case class AddChar(c: Char) extends TodoMessage
  case object DeleteChar extends TodoMessage
  case object ToggleInputMode extends TodoMessage
  case object AddCurrentItem extends TodoMessage
  case object ClearInput extends TodoMessage

  /** Clean todo list application with proper text input */
  object TodoApp extends LayoutzApp[TodoState, TodoMessage] {

    def init = TodoState(
      items = List("Learn Scala", "Build awesome apps", "Drink coffee"),
      completed = Set(2), // Coffee is done!
      inputText = "",
      inputMode = false
    )

    def update(msg: TodoMessage, state: TodoState): TodoState = msg match {
      case ToggleItem(index)
          if index >= 0 && index < state.items.length && !state.inputMode =>
        val newCompleted =
          if (state.completed.contains(index)) state.completed - index
          else state.completed + index
        state.copy(completed = newCompleted)

      case AddChar(c) if state.inputMode =>
        state.copy(inputText = state.inputText + c)

      case DeleteChar if state.inputMode && state.inputText.nonEmpty =>
        state.copy(inputText = state.inputText.dropRight(1))

      case ToggleInputMode =>
        state.copy(inputMode = !state.inputMode, inputText = "")

      case AddCurrentItem if state.inputMode && state.inputText.trim.nonEmpty =>
        state.copy(
          items = state.items :+ state.inputText.trim,
          inputText = "",
          inputMode = false
        )

      case ClearInput if state.inputMode =>
        state.copy(inputText = "", inputMode = false)

      case _ => state
    }

    def onKey(k: Key): Option[TodoMessage] = k match {
      case CharKey(c) if c.isDigit && c != '0' =>
        val index = c.asDigit - 1
        Some(ToggleItem(index))
      case CharKey('n') | CharKey('N') => Some(ToggleInputMode)
      case CharKey('0')                => None // Explicitly ignore zero
      case CharKey(c) if c.isLetter || c == ' ' || ".,!?-_()[]{}".contains(c) =>
        Some(AddChar(c))
      case BackspaceKey => Some(DeleteChar)
      case EnterKey     => Some(AddCurrentItem)
      case EscapeKey    => Some(ClearInput)
      case _            => None
    }

    def view(state: TodoState): Element = {
      val todoItems = state.items.zipWithIndex.map { case (item, index) =>
        val status = if (state.completed.contains(index)) "✅" else "❌"
        val number = index + 1
        Text(s"$number. $status $item")
      }

      val inputSection = if (state.inputMode) {
        layout(
          textInput(
            "New Task",
            state.inputText,
            "Enter task description...",
            active = true
          ),
          Text("Press Enter to add, Esc to cancel")
        )
      } else {
        layout(
          Text("Press 'n' to add new task"),
          Text("Press 1-9 to toggle completion")
        )
      }

      val stats = {
        val total = state.items.length
        val completed = state.completed.size
        Text(s"Progress: $completed/$total completed")
      }

      layout(
        section("Todo List")(
          layout(todoItems: _*)
        ),
        section("Add New Task")(inputSection),
        section("Stats")(stats)
      )
    }

  }

  // Loading demo app types
  case class LoadingState(
      selectedTask: Int,
      isLoading: Boolean,
      progress: Double,
      spinnerFrame: Int,
      completed: Set[Int],
      elapsedTime: Long,
      startTime: Long
  )

  sealed trait LoadingMessage
  case class SelectTask(taskId: Int) extends LoadingMessage
  case object StartLoading extends LoadingMessage
  case object ProgressTick extends LoadingMessage
  case object SpinnerTick extends LoadingMessage
  case object CompleteTask extends LoadingMessage
  case object ResetApp extends LoadingMessage

  /** Interactive loading demo with progress bars and spinners */
  object LoadingApp extends LayoutzApp[LoadingState, LoadingMessage] {

    private val tasks = List(
      "Download large file (5 seconds)",
      "Process database (3 seconds)",
      "Generate report (7 seconds)",
      "Backup data (4 seconds)",
      "Sync with server (6 seconds)"
    )

    private val taskDurations = Map(
      0 -> 5000,
      1 -> 3000,
      2 -> 7000,
      3 -> 4000,
      4 -> 6000
    )

    def init = LoadingState(
      selectedTask = 0,
      isLoading = false,
      progress = 0.0,
      spinnerFrame = 0,
      completed = Set.empty,
      elapsedTime = 0,
      startTime = 0
    )

    def update(msg: LoadingMessage, state: LoadingState): LoadingState =
      msg match {
        case SelectTask(taskId)
            if !state.isLoading && taskId >= 0 && taskId < tasks.length =>
          state.copy(selectedTask = taskId)

        case StartLoading if !state.isLoading =>
          state.copy(
            isLoading = true,
            progress = 0.0,
            startTime = System.currentTimeMillis()
          )

        case ProgressTick if state.isLoading =>
          val currentTime = System.currentTimeMillis()
          val elapsed = currentTime - state.startTime
          val duration = taskDurations(state.selectedTask)
          val newProgress = math.min(1.0, elapsed.toDouble / duration)

          if (newProgress >= 1.0) {
            state.copy(
              isLoading = false,
              progress = 1.0,
              completed = state.completed + state.selectedTask,
              elapsedTime = elapsed
            )
          } else {
            state.copy(progress = newProgress, elapsedTime = elapsed)
          }

        case SpinnerTick =>
          state.copy(spinnerFrame = state.spinnerFrame + 1)

        case CompleteTask =>
          state.copy(
            isLoading = false,
            progress = 1.0,
            completed = state.completed + state.selectedTask
          )

        case ResetApp =>
          init.copy(completed = Set.empty)

        case _ => state
      }

    def onKey(k: Key): Option[LoadingMessage] = k match {
      case CharKey(c) if c.isDigit && c != '0' =>
        val taskId = c.asDigit - 1
        Some(SelectTask(taskId))
      case CharKey(' ') | EnterKey     => Some(StartLoading)
      case CharKey('r') | CharKey('R') => Some(ResetApp)
      case ProgressTickKey             => Some(ProgressTick)
      case SpinnerTickKey              => Some(SpinnerTick)
      case _                           => None
    }

    def view(state: LoadingState): Element = {
      val taskList = tasks.zipWithIndex.map { case (task, index) =>
        val isSelected = index == state.selectedTask
        val isCompleted = state.completed.contains(index)
        val marker = if (isSelected) ">" else " "
        val status = if (isCompleted) "✅" else "  "
        val number = index + 1
        Text(s"$marker $number. $status $task")
      }

      val progressSection = if (state.isLoading) {
        val currentSpinner = spinner(
          label = "Processing...",
          frame = state.spinnerFrame,
          style = SpinnerStyle.Dots
        )

        val progressBar = inlineBar("Progress", state.progress)
        val timeElapsed = state.elapsedTime / 1000.0
        val timeText = Text(f"Elapsed: ${timeElapsed}%.1f seconds")

        layout(
          currentSpinner,
          progressBar,
          timeText,
          Text(""),
          Text("Please wait...")
        )
      } else if (state.progress >= 1.0) {
        val completionTime = state.elapsedTime / 1000.0
        layout(
          Text("✅ Task completed!"),
          Text(f"Finished in ${completionTime}%.1f seconds"),
          Text(""),
          Text("Press SPACE to start another task")
        )
      } else {
        layout(
          Text("Press SPACE or ENTER to start the selected task"),
          Text("Press 1-5 to select a different task"),
          Text("Press 'r' to reset all completed tasks")
        )
      }

      val statsSection = {
        val totalCompleted = state.completed.size
        val totalTasks = tasks.length
        Text(s"Completed: $totalCompleted/$totalTasks tasks")
      }

      layout(
        section("🚀 Loading Demo")(
          layout(taskList: _*)
        ),
        section("📊 Progress")(progressSection),
        section("📈 Stats")(statsSection)
      )
    }
  }

  // Navigation demo with arrow keys
  case class NavigationState(
    selectedItem: Int,
    items: List[String],
    message: String
  )

  sealed trait NavigationMessage
  case object MoveUp extends NavigationMessage
  case object MoveDown extends NavigationMessage
  case object MoveLeft extends NavigationMessage
  case object MoveRight extends NavigationMessage
  case object SelectItem extends NavigationMessage
  case object ResetSelection extends NavigationMessage

  /** Arrow key navigation demo */
  object NavigationApp extends LayoutzApp[NavigationState, NavigationMessage] {
    
    private val menuItems = List(
      "🏠 Home",
      "📄 Documents", 
      "⚙️ Settings",
      "📊 Analytics",
      "👤 Profile",
      "🚪 Exit"
    )

    def init = NavigationState(
      selectedItem = 0,
      items = menuItems,
      message = "Use arrow keys to navigate!"
    )

    def update(msg: NavigationMessage, state: NavigationState): NavigationState = msg match {
      case MoveUp =>
        val newSelected = if (state.selectedItem > 0) state.selectedItem - 1 else state.items.length - 1
        state.copy(selectedItem = newSelected, message = "Moved up")
        
      case MoveDown =>
        val newSelected = if (state.selectedItem < state.items.length - 1) state.selectedItem + 1 else 0
        state.copy(selectedItem = newSelected, message = "Moved down")
        
      case MoveLeft =>
        state.copy(message = "← Left arrow pressed")
        
      case MoveRight =>
        state.copy(message = "→ Right arrow pressed")
        
      case SelectItem =>
        val selected = state.items(state.selectedItem)
        state.copy(message = s"✅ Selected: $selected")
        
      case ResetSelection =>
        state.copy(selectedItem = 0, message = "Reset to top")
    }

    def onKey(k: Key): Option[NavigationMessage] = k match {
      // WASD navigation (easier to handle)
      case CharKey('w') | CharKey('W') => Some(MoveUp)
      case CharKey('s') | CharKey('S') => Some(MoveDown)
      case CharKey('a') | CharKey('A') => Some(MoveLeft)
      case CharKey('d') | CharKey('D') => Some(MoveRight)
      
      // Arrow keys (when escape sequence parsing works)
      case ArrowUpKey    => Some(MoveUp)
      case ArrowDownKey  => Some(MoveDown)
      case ArrowLeftKey  => Some(MoveLeft)
      case ArrowRightKey => Some(MoveRight)
      
      // Other controls
      case EnterKey      => Some(SelectItem)
      case CharKey('r') | CharKey('R') => Some(ResetSelection)
      case _             => None
    }

    def view(state: NavigationState): Element = {
      val menuList = state.items.zipWithIndex.map { case (item, index) =>
        val marker = if (index == state.selectedItem) "►" else " "
        val style = if (index == state.selectedItem) s"$marker $item ◄" else s"$marker $item"
        Text(style)
      }

      layout(
        section("🎮 Navigation Demo")(
          layout(menuList: _*)
        ),
        section("Status")(
          Text(state.message)
        ),
        section("Controls")(
          bullets(
            "W/S keys - Navigate up/down",
            "A/D keys - Move left/right", 
            "↑↓←→ Arrow keys - Also work (when supported)",
            "Enter - Select current item",
            "R - Reset to top",
            "Ctrl+Q - Quit"
          )
        )
      )
    }
  }

}

/** Simple launchers for each demo */
object RunCounterDemo {
  def main(args: Array[String]): Unit = {
    LayoutzRuntime.run(Examples.CounterApp)
  }
}

object RunTodoDemo {
  def main(args: Array[String]): Unit = {
    LayoutzRuntime.run(Examples.TodoApp)
  }
}

object RunLoadingDemo {
  def main(args: Array[String]): Unit = {
    LayoutzRuntime.run(Examples.LoadingApp)
  }
}

object RunNavigationDemo {
  def main(args: Array[String]): Unit = {
    LayoutzRuntime.run(Examples.NavigationApp)
  }
}
