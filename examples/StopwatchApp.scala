import layoutz._

case class TimerState(seconds: Int, running: Boolean)
sealed trait Msg
case object Tick extends Msg
case object ToggleTimer extends Msg
case object ResetTimer extends Msg

object StopwatchApp extends LayoutzApp[TimerState, Msg] {
  def init = (TimerState(0, false), Cmd.none)

  def update(msg: Msg, state: TimerState) = msg match {
    case Tick =>
      (state.copy(seconds = state.seconds + 1), Cmd.none)
    case ToggleTimer =>
      (state.copy(running = !state.running), Cmd.none)
    case ResetTimer =>
      (TimerState(0, running = false), Cmd.none)
  }

  def subscriptions(state: TimerState) = Sub.batch(
    if (state.running) Sub.time.everyMs(1000, Tick) else Sub.none,
    Sub.onKeyPress {
      case Key.Char(' ') => Some(ToggleTimer)
      case Key.Char('r') => Some(ResetTimer)
      case _             => None
    }
  )

  def view(state: TimerState) = {
    val minutes = state.seconds / 60
    val secs = state.seconds % 60
    val timeDisplay = f"$minutes%02d:$secs%02d"

    val statusColor = if (state.running) Color.BrightGreen else Color.BrightYellow
    val statusText = if (state.running) "RUNNING" else "PAUSED"

    layout(
      underlineColored("=", Color.BrightCyan)("Stopwatch").style(Style.Bold),
      "",
      box("Time")(
        timeDisplay.style(Style.Bold).center(20)
      ).color(statusColor).border(Border.Double),
      "",
      kv(
        "Status"  -> statusText,
        "Elapsed" -> s"${state.seconds}s"
      ).color(Color.BrightBlue),
      "",
      ul(
        "space: start/pause",
        "r: reset"
      ).color(Color.BrightBlack)
    )
  }
}

object StopwatchAppRunner {
  def main(args: Array[String]): Unit = StopwatchApp.run
}
