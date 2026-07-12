import layoutz._

case class FileState(content: String, error: Option[String])
sealed trait Msg
case class FileLoaded(result: Either[String, String]) extends Msg

object FileViewer extends LayoutzApp[FileState, Msg] {
  val filename = "README.md"

  def init = (FileState("Loading...", None), Cmd.file.read(filename, FileLoaded))

  def update(msg: Msg, state: FileState) = msg match {
    case FileLoaded(Right(content)) =>
      (state.copy(content = content.take(500), error = None), Cmd.none)
    case FileLoaded(Left(err)) =>
      (state.copy(error = Some(err)), Cmd.none)
  }

  def subscriptions(state: FileState) =
    Sub.file.watch(filename, FileLoaded)

  def view(state: FileState) = {
    val display = state.error match {
      case Some(err) => Color.BrightRed(s"Error: $err")
      case None      => wrap(state.content, 60)
    }

    layout(
      underlineColored("=", Color.BrightMagenta)("File Viewer").style(Style.Bold),
      kv("File" -> filename).color(Color.BrightBlue),
      box("Content")(display).border(Border.Round),
      "Auto-reloads on file change".color(Color.BrightBlack)
    )
  }
}

object FileViewerRunner {
  def main(args: Array[String]): Unit = FileViewer.run
}
