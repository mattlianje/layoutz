import layoutz._

case class TaskState(status: String = "idle", count: Int = 0)

sealed trait Msg
case object RunTask extends Msg
case class TaskDone(result: Either[String, String]) extends Msg

object SideEffectApp extends LayoutzApp[TaskState, Msg] {
  def init = (TaskState(), Cmd.none)

  def update(msg: Msg, state: TaskState) = msg match {
    case RunTask =>
      (
        state.copy(status = "running..."),
        Cmd.task {
          Thread.sleep(500)
          if (scala.util.Random.nextDouble() < 0.3)
            throw new Exception("Launch failure")
          "completed"
        }(TaskDone)
      )

    case TaskDone(Right(_)) =>
      state.copy(status = "success", count = state.count + 1)

    case TaskDone(Left(err)) =>
      state.copy(status = s"error: $err")
  }

  def subscriptions(state: TaskState) = Sub.onKeyPress {
    case Key.Char('r') => Some(RunTask)
    case _             => None
  }

  def view(state: TaskState) = layout(
    section("Side Effect Demo")(
      kv("Status" -> state.status, "Count" -> state.count.toString)
    ),
    "r: run task".color(Color.BrightBlack)
  )
}

// Use `Cmd.fire` for fire-and-forget effects (logging, analytics, etc.):
//   Cmd.fire(println("User clicked button"))

object SideEffectAppRunner {
  def main(args: Array[String]): Unit = SideEffectApp.run
}
