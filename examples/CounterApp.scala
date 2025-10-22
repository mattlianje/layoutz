import layoutz._

object CounterApp extends LayoutzApp[Int, String] {
  // init: ( Model, Cmd Msg )
  def init = (0, Cmd.none)

  // update: Msg -> Model -> ( Model, Cmd Msg )
  def update(msg: String, count: Int) = msg match {
    case "inc" => (count + 1, Cmd.none)
    case "dec" => (count - 1, Cmd.none)
    case _     => (count, Cmd.none)
  }

  // subscriptions: Model -> Sub Msg
  def subscriptions(count: Int) =
    Sub.onKeyPress {
      case CharKey('+') => Some("inc")
      case CharKey('-') => Some("dec")
      case _            => None
    }

  // view: Model -> Html Msg
  def view(count: Int) = layout(
    section("Counter")(s"Count: $count"),
    br,
    ul("Press `+` or `-` to change counter"),
    ul("Press Ctrl+Q to quit")
  )
}
