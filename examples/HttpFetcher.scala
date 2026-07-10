import layoutz._

case class FetchState(data: String, loading: Boolean, count: Int)
sealed trait Msg
case object Fetch extends Msg
case class Response(result: Either[String, String]) extends Msg

object HttpFetcher extends LayoutzApp[FetchState, Msg] {
  def init = (FetchState("Press 'f' to fetch", false, 0), Cmd.none)

  def update(msg: Msg, state: FetchState) = msg match {
    case Fetch =>
      (state.copy(loading = true, count = state.count + 1),
       Cmd.http.get("https://api.github.com/zen", Response))
    case Response(Right(data)) =>
      (state.copy(data = data, loading = false), Cmd.none)
    case Response(Left(err)) =>
      (state.copy(data = s"Error: $err", loading = false), Cmd.none)
  }

  def subscriptions(state: FetchState) = Sub.onKeyPress {
    case Key.Char('f') => Some(Fetch)
    case _             => None
  }

  def view(state: FetchState) = {
    val status: Element = if (state.loading) spinner("Fetching", state.count % 10)
                          else Text(s"Fetched ${state.count} times")

    layout(
      underlineColored("=", Color.BrightCyan)("HTTP Fetcher").style(Style.Bold),
      box("Zen Quote")(wrap(state.data, 50)).border(Border.Round).color(Color.BrightGreen),
      status,
      "f: fetch".color(Color.BrightBlack)
    )
  }
}

object HttpFetcherRunner {
  def main(args: Array[String]): Unit = HttpFetcher.run
}
