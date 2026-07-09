import layoutz._

object LoaderExample {
  def main(args: Array[String]): Unit = {
    print("[2J[H") // clear the REPL banner
    Thread.sleep(1000)

    // Every built-in bar style (bracketed ones last, so the open bars line up on top)
    for (_ <- loader("Blocks ", 1 to 60).blocks) Thread.sleep(16)
    for (_ <- loader("Dots   ", 1 to 60).dots) Thread.sleep(16)
    for (_ <- loader("Line   ", 1 to 60).line) Thread.sleep(16)
    for (_ <- loader("Pipes  ", 1 to 60).pipes) Thread.sleep(16)
    for (_ <- loader("Bar    ", 1 to 60).bar) Thread.sleep(16)
    for (_ <- loader("Ascii  ", 1 to 60).ascii) Thread.sleep(16)

    // No label — just the bar
    for (_ <- loader(1 to 40)) Thread.sleep(16)

    // Unknown size — spinner + running count
    val it: Iterator[Int] = Iterator.from(1).take(90)
    for (_ <- loader.stream("Streaming", it).styled(spinner = SpinnerStyle.Dots)) Thread.sleep(45)
  }
}
