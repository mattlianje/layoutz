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
            s"Current count: $count",
            "Press + / - to adjust"
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
          "Press Enter to add, Esc to cancel"
        )
      } else {
        layout(
          "Press 'n' to add new task",
          "Press 1-9 to toggle completion"
        )
      }

      val stats = {
        val total = state.items.length
        val completed = state.completed.size
        s"Progress: $completed/$total completed"
      }

      layout(
        section("Todo List")(
          Layout(todoItems)
        ),
        section("Add New Task")(inputSection),
        section("Stats")(stats)
      )
    }

  }

  // Combined navigation and loading demo types
  case class NavLoadState(
      selectedTask: Int,
      isLoading: Boolean,
      progress: Double,
      spinnerFrame: Int,
      completed: Set[Int],
      elapsedTime: Long,
      startTime: Long,
      message: String,
      addingTask: Boolean,
      newTaskName: String,
      newTaskDuration: String,
      customTasks: List[(String, Int)]
  )

  sealed trait NavLoadMessage
  case object MoveUp extends NavLoadMessage
  case object MoveDown extends NavLoadMessage
  case object StartTask extends NavLoadMessage
  case object ProgressTick extends NavLoadMessage
  case object SpinnerTick extends NavLoadMessage
  case object ResetApp extends NavLoadMessage
  case object ToggleAddTask extends NavLoadMessage
  case class HandleChar(c: Char) extends NavLoadMessage
  case object DeleteTaskChar extends NavLoadMessage
  case object ConfirmNewTask extends NavLoadMessage
  case object CancelNewTask extends NavLoadMessage

  /** Combined navigation and loading demo with progress tracking */
  object NavLoadApp extends LayoutzApp[NavLoadState, NavLoadMessage] {

    private val tasks = List(
      "Download large file",
      "Process database",
      "Generate report",
      "Backup data",
      "Sync with server",
      "Clean temp files"
    )

    private def getTaskEmoji(
        taskIndex: Int,
        isCompleted: Boolean,
        isRunning: Boolean
    ): String = {
      if (isCompleted) return "✅"
      if (isRunning) return "⚡"

      taskIndex match {
        case 0 => "📁" // Download
        case 1 => "🗄️" // Database
        case 2 => "📊" // Report
        case 3 => "💾" // Backup
        case 4 => "🌐" // Sync
        case 5 => "🧹" // Clean
        case _ => "📋" // Custom tasks
      }
    }

    private val taskDurations = Map(
      0 -> 5000,
      1 -> 3000,
      2 -> 7000,
      3 -> 4000,
      4 -> 6000,
      5 -> 2000
    )

    def init = NavLoadState(
      selectedTask = 0,
      isLoading = false,
      progress = 0.0,
      spinnerFrame = 0,
      completed = Set.empty,
      elapsedTime = 0,
      startTime = 0,
      message = "Navigate with arrows, Enter to start task",
      addingTask = false,
      newTaskName = "",
      newTaskDuration = "",
      customTasks = List.empty
    )

    private def allTasks(state: NavLoadState): List[String] =
      tasks ++ state.customTasks.map(_._1)

    private def allTaskDurations(state: NavLoadState): Map[Int, Int] = {
      val customDurations = state.customTasks.zipWithIndex.map {
        case ((_, duration), index) =>
          (tasks.length + index) -> duration
      }.toMap
      taskDurations ++ customDurations
    }

    def update(msg: NavLoadMessage, state: NavLoadState): NavLoadState =
      msg match {
        // Arrow key navigation (when not in text input mode)
        case MoveUp if !state.isLoading && !state.addingTask =>
          val currentTasks = allTasks(state)
          val newSelected =
            if (state.selectedTask > 0) state.selectedTask - 1
            else currentTasks.length - 1
          state.copy(
            selectedTask = newSelected,
            message = s"Selected: ${currentTasks(newSelected)}"
          )

        case MoveDown if !state.isLoading && !state.addingTask =>
          val currentTasks = allTasks(state)
          val newSelected =
            if (state.selectedTask < currentTasks.length - 1)
              state.selectedTask + 1
            else 0
          state.copy(
            selectedTask = newSelected,
            message = s"Selected: ${currentTasks(newSelected)}"
          )

        case StartTask if !state.isLoading && !state.addingTask =>
          val currentTasks = allTasks(state)
          val taskName = currentTasks(state.selectedTask)
          state.copy(
            isLoading = true,
            progress = 0.0,
            startTime = System.currentTimeMillis(),
            message = s"Starting: $taskName"
          )

        case ProgressTick if state.isLoading =>
          val currentTime = System.currentTimeMillis()
          val elapsed = currentTime - state.startTime
          val durations = allTaskDurations(state)
          val duration = durations(state.selectedTask)
          val newProgress = math.min(1.0, elapsed.toDouble / duration)

          if (newProgress >= 1.0) {
            val currentTasks = allTasks(state)
            val taskName = currentTasks(state.selectedTask)
            state.copy(
              isLoading = false,
              progress = 1.0,
              completed = state.completed + state.selectedTask,
              elapsedTime = elapsed,
              message = s"✅ Completed: $taskName"
            )
          } else {
            state.copy(progress = newProgress, elapsedTime = elapsed)
          }

        case SpinnerTick =>
          state.copy(spinnerFrame = state.spinnerFrame + 1)

        case ToggleAddTask if !state.isLoading =>
          state.copy(
            addingTask = !state.addingTask,
            newTaskName = "",
            newTaskDuration = "",
            message =
              if (!state.addingTask) "Adding new task..."
              else "Cancelled adding task"
          )

        // Handle all character input based on current state
        case HandleChar(c) =>
          if (state.addingTask) {
            // When adding task, characters go to text input
            if (c.isLetter || c == ' ' || c == '-' || c == '_') {
              state.copy(newTaskName = state.newTaskName + c)
            } else if (c.isDigit) {
              state.copy(newTaskDuration = state.newTaskDuration + c)
            } else {
              state // ignore other characters
            }
          } else if (!state.isLoading) {
            // When not adding task and not loading, characters are navigation/commands
            c match {
              case 'w' | 'W' =>
                val currentTasks = allTasks(state)
                val newSelected =
                  if (state.selectedTask > 0) state.selectedTask - 1
                  else currentTasks.length - 1
                state.copy(
                  selectedTask = newSelected,
                  message = s"Selected: ${currentTasks(newSelected)}"
                )

              case 's' | 'S' =>
                val currentTasks = allTasks(state)
                val newSelected =
                  if (state.selectedTask < currentTasks.length - 1)
                    state.selectedTask + 1
                  else 0
                state.copy(
                  selectedTask = newSelected,
                  message = s"Selected: ${currentTasks(newSelected)}"
                )

              case 'r' | 'R' =>
                init.copy(
                  selectedTask = state.selectedTask,
                  customTasks = state.customTasks,
                  message = "Reset all completed tasks"
                )

              case 'n' | 'N' =>
                state.copy(
                  addingTask = true,
                  newTaskName = "",
                  newTaskDuration = "",
                  message = "Adding new task..."
                )

              case ' ' =>
                val currentTasks = allTasks(state)
                val taskName = currentTasks(state.selectedTask)
                state.copy(
                  isLoading = true,
                  progress = 0.0,
                  startTime = System.currentTimeMillis(),
                  message = s"Starting: $taskName"
                )

              case _ => state // ignore other characters
            }
          } else {
            state // ignore input when loading
          }

        case DeleteTaskChar if state.addingTask =>
          if (state.newTaskDuration.nonEmpty) {
            state.copy(newTaskDuration = state.newTaskDuration.dropRight(1))
          } else if (state.newTaskName.nonEmpty) {
            state.copy(newTaskName = state.newTaskName.dropRight(1))
          } else state

        case ConfirmNewTask
            if state.addingTask && state.newTaskName.trim.nonEmpty && state.newTaskDuration.trim.nonEmpty =>
          val duration =
            try { state.newTaskDuration.toInt * 1000 }
            catch { case _: NumberFormatException => 3000 }
          state.copy(
            addingTask = false,
            customTasks =
              state.customTasks :+ (state.newTaskName.trim, duration),
            newTaskName = "",
            newTaskDuration = "",
            message =
              s"Added task: ${state.newTaskName.trim} (${duration / 1000}s)"
          )

        case CancelNewTask if state.addingTask =>
          state.copy(
            addingTask = false,
            newTaskName = "",
            newTaskDuration = "",
            message = "Cancelled adding task"
          )

        case _ => state
      }

    def onKey(k: Key): Option[NavLoadMessage] = k match {
      // System ticks (always work)
      case ProgressTickKey => Some(ProgressTick)
      case SpinnerTickKey  => Some(SpinnerTick)

      // Special keys
      case EscapeKey    => Some(CancelNewTask)
      case TabKey       => Some(ConfirmNewTask)
      case BackspaceKey => Some(DeleteTaskChar)
      case ArrowUpKey   => Some(MoveUp)
      case ArrowDownKey => Some(MoveDown)
      case EnterKey     => Some(StartTask)

      // All character input goes through HandleChar - state will decide what to do
      case CharKey(c) => Some(HandleChar(c))

      case _ => None
    }

    def view(state: NavLoadState): Element = {
      val currentTasks = allTasks(state)
      val durations = allTaskDurations(state)

      val taskList = currentTasks.zipWithIndex.map { case (task, index) =>
        val isSelected = index == state.selectedTask
        val isCompleted = state.completed.contains(index)
        val isCurrentlyLoading = state.isLoading && index == state.selectedTask

        val marker = if (isSelected) "►" else " "
        val emoji = getTaskEmoji(index, isCompleted, isCurrentlyLoading)

        val style =
          if (isSelected) s"$marker $emoji $task ◄"
          else s"$marker $emoji $task"
        Text(style)
      }

      val statusSection = if (state.addingTask) {
        // Switch active field based on recent input - if duration has content, focus there
        val nameActive = state.newTaskDuration.isEmpty
        val durationActive = state.newTaskDuration.nonEmpty

        box("Add New Task")(
          textInput(
            "Task Name",
            state.newTaskName,
            "Enter task name...",
            active = nameActive
          ),
          textInput(
            "Duration (seconds)",
            state.newTaskDuration,
            "e.g. 5",
            active = durationActive
          ),
          br,
          layout("Type letters for name, digits for duration"),
          layout("Press Tab to add, Esc to cancel")
        )
      } else if (state.isLoading) {
        val currentSpinner = spinner(
          label = "Processing",
          frame = state.spinnerFrame,
          style = SpinnerStyle.Dots
        )

        val progressBar = inlineBar("Progress", state.progress)
        val timeElapsed = state.elapsedTime / 1000.0
        val timeText = f"Elapsed: ${timeElapsed}%.1f seconds"

        layout(
          currentSpinner,
          progressBar,
          timeText
        )
      } else {
        layout(state.message)
      }

      val simpleStats = {
        val totalCompleted = state.completed.size
        val totalTasks = currentTasks.length
        val completionPercentage =
          if (totalTasks > 0) (totalCompleted * 100) / totalTasks else 0

        layout(
          s"Progress: $totalCompleted/$totalTasks tasks ($completionPercentage%)",
          if (totalCompleted == totalTasks) "🎉 All tasks completed!" else ""
        )
      }

      layout(
        section("Task Navigator")(
          Layout(taskList)
        ),
        br,
        section("Status")(statusSection),
        br,
        section("Progress")(simpleStats),
        br,
        section("🎮 Controls")(
          ul(
            "W/S or ↑↓ - Navigate tasks",
            "SPACE/Enter - Start selected task",
            "N - Add new task",
            "R - Reset completed tasks",
            "Ctrl+Q - Quit"
          )
        )
      )
    }
  }

  // OLD LOADING DEMO - REPLACED BY NavLoadApp
  // (Keep for backwards compatibility with tests that still reference it)
  type LoadingState = NavLoadState
  type LoadingMessage = NavLoadMessage
  val LoadingApp = NavLoadApp

  // OLD NAVIGATION DEMO - REPLACED BY NavLoadApp
  // (Keep for backwards compatibility)
  type NavigationState = NavLoadState
  type NavigationMessage = NavLoadMessage
  val NavigationApp = NavLoadApp

  // Margin demo application
  case class MarginDemoState(selectedExample: Int)
  sealed trait MarginMessage
  case class SelectExample(index: Int) extends MarginMessage

  /** Demo showcasing margin functionality with different styles */
  object MarginApp extends LayoutzApp[MarginDemoState, MarginMessage] {

    def init = MarginDemoState(selectedExample = 0)

    def update(msg: MarginMessage, state: MarginDemoState): MarginDemoState =
      msg match {
        case SelectExample(index) if index >= 0 && index < examples.length =>
          state.copy(selectedExample = index)
        case _ => state
      }

    def onKey(k: Key): Option[MarginMessage] = k match {
      case CharKey(c) if c.isDigit && c != '0' =>
        val index = c.asDigit - 1
        Some(SelectExample(index))
      case _ => None
    }

    private val examples = List(
      (
        "Basic Margin",
        () =>
          layout(
            "Content without margin",
            br(2),
            margin("    ")("Content with left margin"),
            space(4),
            "inline spacing",
            br,
            margin("        ")("Content with bigger margin")
          )
      ),
      (
        "Status Margins",
        () =>
          layout(
            margin.error("This is an error message"),
            br,
            margin.warn("This is a warning message"),
            br,
            margin.success("This is a success message"),
            br,
            "Regular text without margin"
          )
      ),
      (
        "Custom Margins",
        () =>
          layout(
            margin.error("Error with custom margin"),
            br,
            margin("  ")("Mixed with boxes:"),
            box("Status")(
              margin.success("Everything is working"),
              margin.warn("Minor issues detected"),
              margin.error("Critical error found")
            )
          )
      ),
      (
        "Complex Layout",
        () =>
          layout(
            section("🎨 Margin Demo Dashboard")(
              layout(
                margin("  ")("Welcome to the margin showcase!"),
                br(2),
                row(
                  statusCard("API", "LIVE"),
                  space(2),
                  margin("  ")(statusCard("DB", "SLOW")),
                  space(2),
                  margin("    ")(statusCard("Cache", "DOWN"))
                ),
                br(3),
                margin("  ")(
                  box("System Status")(
                    ul(
                      margin.success("Production: All systems operational"),
                      margin.warn("Staging: Performance degraded"),
                      margin.error("Development: Service unavailable")
                    )
                  )
                )
              )
            )
          )
      )
    )

    def view(state: MarginDemoState): Element = {
      val selectedExample = examples(state.selectedExample)

      layout(
        section("🎨 Margin Examples")(
          layout(
            s"Currently showing: ${selectedExample._1}",
            br,
            selectedExample._2()
          )
        ),
        section("📋 Available Examples")(
          Layout(examples.zipWithIndex.map { case ((name, _), index) =>
            val marker = if (index == state.selectedExample) "►" else " "
            val number = index + 1
            Text(s"$marker $number. $name")
          })
        ),
        section("🎮 Controls")(
          layout(
            "Press 1-4 to switch between examples",
            "Ctrl+Q to quit"
          )
        )
      )
    }
  }

}

/** Simple launchers for each demo */
object RunCounterDemo {
  def main(args: Array[String]): Unit = {
    Examples.CounterApp.run()
  }
}

object RunTodoDemo {
  def main(args: Array[String]): Unit = {
    Examples.TodoApp.run()
  }
}

object RunNavLoadDemo {
  def main(args: Array[String]): Unit = {
    Examples.NavLoadApp.run()
  }
}

object RunMarginDemo {
  def main(args: Array[String]): Unit = {
    Examples.MarginApp.run()
  }
}
