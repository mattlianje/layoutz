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

  // init: ( Model, Cmd Msg )
  def init = (
    NavLoadState(
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
    ),
    Cmd.none
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

  // update: Msg -> Model -> ( Model, Cmd Msg )
  def update(
      msg: NavLoadMessage,
      state: NavLoadState
  ): (NavLoadState, Cmd[NavLoadMessage]) =
    msg match {
      // Arrow key navigation (when not in text input mode)
      case MoveUp if !state.isLoading && !state.addingTask =>
        val currentTasks = allTasks(state)
        val newSelected =
          if (state.selectedTask > 0) state.selectedTask - 1
          else currentTasks.length - 1
        (
          state.copy(
            selectedTask = newSelected,
            message = s"Selected: ${currentTasks(newSelected)}"
          ),
          Cmd.none
        )

      case MoveDown if !state.isLoading && !state.addingTask =>
        val currentTasks = allTasks(state)
        val newSelected =
          if (state.selectedTask < currentTasks.length - 1)
            state.selectedTask + 1
          else 0
        (
          state.copy(
            selectedTask = newSelected,
            message = s"Selected: ${currentTasks(newSelected)}"
          ),
          Cmd.none
        )

      case StartTask if !state.isLoading && !state.addingTask =>
        val currentTasks = allTasks(state)
        val taskName = currentTasks(state.selectedTask)
        (
          state.copy(
            isLoading = true,
            progress = 0.0,
            startTime = System.currentTimeMillis(),
            message = s"Starting: $taskName"
          ),
          Cmd.none
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
        (newState.copy(spinnerFrame = newState.spinnerFrame + 1), Cmd.none)

      case UpdateTick =>
        (state.copy(spinnerFrame = state.spinnerFrame + 1), Cmd.none)

      case ToggleAddTask if !state.isLoading =>
        (
          state.copy(
            addingTask = !state.addingTask,
            newTaskName = "",
            newTaskDuration = "",
            message =
              if (!state.addingTask) "Adding new task..."
              else "Cancelled adding task"
          ),
          Cmd.none
        )

      // Handle all character input based on current state
      case HandleChar(c) =>
        if (state.addingTask) {
          // When adding task, characters go to text input
          if (c.isLetter || c == ' ' || c == '-' || c == '_') {
            (state.copy(newTaskName = state.newTaskName + c), Cmd.none)
          } else if (c.isDigit) {
            (state.copy(newTaskDuration = state.newTaskDuration + c), Cmd.none)
          } else {
            (state, Cmd.none) // ignore other characters
          }
        } else if (!state.isLoading) {
          // When not adding task and not loading, characters are navigation/commands
          c match {
            case 'w' | 'W' =>
              val currentTasks = allTasks(state)
              val newSelected =
                if (state.selectedTask > 0) state.selectedTask - 1
                else currentTasks.length - 1
              (
                state.copy(
                  selectedTask = newSelected,
                  message = s"Selected: ${currentTasks(newSelected)}"
                ),
                Cmd.none
              )

            case 's' | 'S' =>
              val currentTasks = allTasks(state)
              val newSelected =
                if (state.selectedTask < currentTasks.length - 1)
                  state.selectedTask + 1
                else 0
              (
                state.copy(
                  selectedTask = newSelected,
                  message = s"Selected: ${currentTasks(newSelected)}"
                ),
                Cmd.none
              )

            case 'r' | 'R' =>
              (
                init._1.copy(
                  selectedTask = state.selectedTask,
                  customTasks = state.customTasks,
                  message = "Reset all completed tasks"
                ),
                Cmd.none
              )

            case 'n' | 'N' =>
              (
                state.copy(
                  addingTask = true,
                  newTaskName = "",
                  newTaskDuration = "",
                  message = "Adding new task..."
                ),
                Cmd.none
              )

            case ' ' =>
              val currentTasks = allTasks(state)
              val taskName = currentTasks(state.selectedTask)
              (
                state.copy(
                  isLoading = true,
                  progress = 0.0,
                  startTime = System.currentTimeMillis(),
                  message = s"Starting: $taskName"
                ),
                Cmd.none
              )

            case _ => (state, Cmd.none) // ignore other characters
          }
        } else {
          (state, Cmd.none) // ignore input when loading
        }

      case DeleteTaskChar if state.addingTask =>
        if (state.newTaskDuration.nonEmpty) {
          (
            state.copy(newTaskDuration = state.newTaskDuration.dropRight(1)),
            Cmd.none
          )
        } else if (state.newTaskName.nonEmpty) {
          (state.copy(newTaskName = state.newTaskName.dropRight(1)), Cmd.none)
        } else (state, Cmd.none)

      case ConfirmNewTask
          if state.addingTask && state.newTaskName.trim.nonEmpty && state.newTaskDuration.trim.nonEmpty =>
        val duration =
          try state.newTaskDuration.toInt * 1000
          catch { case _: NumberFormatException => 3000 }
        (
          state.copy(
            addingTask = false,
            customTasks =
              state.customTasks :+ (state.newTaskName.trim, duration),
            newTaskName = "",
            newTaskDuration = "",
            message =
              s"Added task: ${state.newTaskName.trim} (${duration / 1000}s)"
          ),
          Cmd.none
        )

      case CancelNewTask if state.addingTask =>
        (
          state.copy(
            addingTask = false,
            newTaskName = "",
            newTaskDuration = "",
            message = "Cancelled adding task"
          ),
          Cmd.none
        )

      case _ => (state, Cmd.none)
    }

  // subscriptions: Model -> Sub Msg
  def subscriptions(state: NavLoadState): Sub[NavLoadMessage] =
    Sub.batch(
      // Subscribe to time updates (for progress updates and spinners)
      Sub.time.everyMs(100, UpdateTick),

      // Subscribe to keyboard input
      Sub.onKeyPress {
        case Key.Escape    => Some(CancelNewTask)
        case Key.Tab       => Some(ConfirmNewTask)
        case Key.Backspace => Some(DeleteTaskChar)
        case Key.Up        => Some(MoveUp)
        case Key.Down      => Some(MoveDown)
        case Key.Enter     => Some(StartTask)
        case Key.Char(c)   => Some(HandleChar(c))
        case _             => None
      }
    )

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
      val timeText = f"Elapsed: $timeElapsed%.1f seconds"

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
