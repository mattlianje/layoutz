import layoutz._

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
case object UpdateTick extends NavLoadMessage
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

      case UpdateTick if state.isLoading =>
        val currentTime = System.currentTimeMillis()
        val elapsed = currentTime - state.startTime
        val durations = allTaskDurations(state)
        val duration = durations(state.selectedTask)
        val newProgress = math.min(1.0, elapsed.toDouble / duration)

        val newState = if (newProgress >= 1.0) {
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

        // Also update spinner frame
        newState.copy(spinnerFrame = newState.spinnerFrame + 1)

      case UpdateTick =>
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
          customTasks = state.customTasks :+ (state.newTaskName.trim, duration),
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
    case Tick => Some(UpdateTick)

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
