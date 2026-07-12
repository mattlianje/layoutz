import layoutz._

case class ClipState(content: String = "", status: String = "Press 'r' to read clipboard")
sealed trait Msg
case class ClipRead(result: Either[String, String]) extends Msg
case class ClipWritten(result: Either[String, Unit]) extends Msg
case object ReadClip extends Msg
case object WriteClip extends Msg

object ClipboardApp extends LayoutzApp[ClipState, Msg] {
  def init = (ClipState(), Cmd.none)

  def update(msg: Msg, state: ClipState) = msg match {
    case ReadClip =>
      (state.copy(status = "Reading..."), Cmd.clipboard.read(ClipRead))
    case ClipRead(Right(text)) =>
      (state.copy(content = text.take(200), status = "Read OK"), Cmd.none)
    case ClipRead(Left(err)) =>
      (state.copy(status = s"Error: $err"), Cmd.none)
    case WriteClip =>
      (state.copy(status = "Writing..."), Cmd.clipboard.write("Hello from layoutz!", ClipWritten))
    case ClipWritten(Right(_)) =>
      (state.copy(status = "Written to clipboard!"), Cmd.none)
    case ClipWritten(Left(err)) =>
      (state.copy(status = s"Error: $err"), Cmd.none)
  }

  def subscriptions(state: ClipState) = Sub.onKeyPress {
    case Key.Char('r') => Some(ReadClip)
    case Key.Char('w') => Some(WriteClip)
    case _             => None
  }

  def view(state: ClipState) = layout(
    section("Clipboard Demo")(
      kv("Status" -> state.status),
      box("Content")(wrap(state.content, 60)).border(Border.Round)
    ),
    ul("r: read clipboard", "w: write to clipboard").color(Color.BrightBlack)
  )
}

object ClipboardAppRunner {
  def main(args: Array[String]): Unit = ClipboardApp.run
}
