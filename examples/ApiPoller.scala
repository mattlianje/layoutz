import layoutz._

case class ApiState(response: String, lastUpdate: String, error: Option[String])
sealed trait Msg
case class ApiResponse(result: Either[String, String]) extends Msg

object ApiPoller extends LayoutzApp[ApiState, Msg] {
  val apiUrl = "https://api.github.com/zen"

  def init = (ApiState("Loading...", "Never", None), Cmd.none)

  def update(msg: Msg, state: ApiState) = msg match {
    case ApiResponse(Right(data)) =>
      val now = java.time.LocalTime.now().toString.take(8)
      (state.copy(response = data, lastUpdate = now, error = None), Cmd.none)
    case ApiResponse(Left(err)) =>
      (state.copy(error = Some(err)), Cmd.none)
  }

  def subscriptions(state: ApiState) =
    Sub.http.pollMs(apiUrl, 3000, ApiResponse)

  def view(state: ApiState) = {
    val display = state.error match {
      case Some(err) => Color.BrightRed(s"Error: $err")
      case None      => wrap(state.response, 60).color(Color.BrightGreen)
    }

    layout(
      underlineColored("~", Color.BrightCyan)("API Poller").style(Style.Bold),
      kv("Endpoint" -> apiUrl, "Last Update" -> state.lastUpdate).color(Color.BrightBlue),
      box("Response")(display).border(Border.Round),
      "Polls every 3s".color(Color.BrightBlack)
    )
  }
}

object ApiPollerRunner {
  def main(args: Array[String]): Unit = ApiPoller.run
}
