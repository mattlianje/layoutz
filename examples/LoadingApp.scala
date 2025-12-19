import layoutz._

case class LoadingState(
    currentTask: Int,
    progress: Double,
    tasks: List[String],
    completedTasks: List[String],
    done: Boolean = false
)

sealed trait LoadingMsg
case object Tick extends LoadingMsg

object LoadingApp extends LayoutzApp[LoadingState, LoadingMsg] {
  private val allTasks = List(
    "Downloading assets",
    "Compiling sources",
    "Running tests",
    "Building artifacts",
    "Deploying to server"
  )

  def init = (
    LoadingState(
      currentTask = 0,
      progress = 0.0,
      tasks = allTasks,
      completedTasks = List.empty
    ),
    Cmd.none
  )

  def update(msg: LoadingMsg, state: LoadingState) = msg match {
    case Tick if state.done =>
      (state, Cmd.exit)

    case Tick =>
      val newProgress = state.progress + 0.02

      if (newProgress >= 1.0) {
        val completedTask = state.tasks(state.currentTask)
        val newCompleted = state.completedTasks :+ completedTask
        val nextTask = state.currentTask + 1

        if (nextTask >= state.tasks.length) {
          (state.copy(progress = 1.0, completedTasks = newCompleted, done = true), Cmd.none)
        } else {
          (
            state.copy(
              currentTask = nextTask,
              progress = 0.0,
              completedTasks = newCompleted
            ),
            Cmd.none
          )
        }
      } else {
        (state.copy(progress = newProgress), Cmd.none)
      }
  }

  def subscriptions(state: LoadingState) = Sub.time.every(50, Tick)

  def view(state: LoadingState) = {
    val completed = state.completedTasks.map(t => Text(s"[==========] $t"))
    val current = state.tasks.lift(state.currentTask).map { taskName =>
      val filled = (state.progress * 10).toInt
      val empty = 10 - filled
      val bar = "=" * filled + "-" * empty
      Text(s"[$bar] $taskName")
    }
    val pending = state.tasks
      .drop(state.currentTask + 1)
      .map(t => Text(s"[----------] $t"))

    val pct = (state.progress * 100).toInt

    layout(
      br,
      Text("Build Progress"),
      Text("─" * 30),
      br,
      Layout(completed ++ current.toList ++ pending),
      br,
      Text(s"Current: $pct%"),
      Text(s"Overall: ${state.completedTasks.length}/${state.tasks.length} tasks"),
      br
    )
  }

}

object LoadingAppRunner {

  def main(args: Array[String]): Unit = {
    LoadingApp.run(
      showQuitMessage = false,
      clearOnStart = false,
      clearOnExit = false,
      alignment = Alignment.Center
    )
    println("Build complete!")
    println()
  }

}
