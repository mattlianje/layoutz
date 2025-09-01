import layoutz._

object CounterApp extends LayoutzApp[Int, String] {
  def init = 0

  def update(msg: String, count: Int) = msg match {
    case "inc" => count + 1
    case "dec" => count - 1
    case _     => count
  }

  def onKey(k: Key) = k match {
    case CharKey('+') => Some("inc")
    case CharKey('-') => Some("dec")
    case _            => None
  }

  def view(count: Int) = layout(
    section("Counter")(s"Count: $count"),
    br,
    ul("Press `+` or `-` to change counter"),
    ul("Press Ctrl+Q to quit")
  )
}
