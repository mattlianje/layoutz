import layoutz._

case class MonitorState(
    github: String = "...",
    httpbin: String = "...",
    placeholder: String = "..."
)

sealed trait Msg
case class GithubResp(result: Either[String, String]) extends Msg
case class HttpbinResp(result: Either[String, String]) extends Msg
case class PlaceholderResp(result: Either[String, String]) extends Msg

object MultiMonitor extends LayoutzApp[MonitorState, Msg] {
  def init = (MonitorState(), Cmd.none)

  def update(msg: Msg, state: MonitorState) = msg match {
    case GithubResp(Right(data))   => (state.copy(github = data.take(20)), Cmd.none)
    case GithubResp(Left(e))       => (state.copy(github = s"ERROR: $e"), Cmd.none)
    case HttpbinResp(Right(_))     => (state.copy(httpbin = "UP"), Cmd.none)
    case HttpbinResp(Left(e))      => (state.copy(httpbin = s"ERROR: $e"), Cmd.none)
    case PlaceholderResp(Right(_)) => (state.copy(placeholder = "UP"), Cmd.none)
    case PlaceholderResp(Left(e))  => (state.copy(placeholder = s"ERROR: $e"), Cmd.none)
  }

  def subscriptions(state: MonitorState) = Sub.batch(
    Sub.http.pollMs("https://api.github.com/zen", 4000, GithubResp),
    Sub.http.pollMs("https://httpbin.org/get", 5000, HttpbinResp),
    Sub.http.pollMs("https://jsonplaceholder.typicode.com/posts/1", 6000, PlaceholderResp)
  )

  def view(state: MonitorState) = layout(
    underlineColored("~", Color.BrightGreen)("Multi-API Monitor").style(Style.Bold),
    br,
    table(
      Seq("Service", "Status"),
      Seq(
        Seq("GitHub", state.github),
        Seq("HTTPBin", state.httpbin),
        Seq("JSONPlaceholder", state.placeholder)
      )
    ).border(Border.Round),
    br,
    "Auto-polls all endpoints".color(Color.BrightBlack)
  )
}

object MultiMonitorRunner {
  def main(args: Array[String]): Unit = MultiMonitor.run
}
