import layoutz._

object InlineDemo {
  case class LoadState(progress: Double, doneTicks: Int)
  sealed trait Msg
  case object Tick extends Msg

  object Loader extends LayoutzApp[LoadState, Msg] {
    def init = (LoadState(0, 0), Cmd.none)

    def update(msg: Msg, state: LoadState) = msg match {
      case Tick =>
        if (state.doneTicks > 30) (state, Cmd.exit)
        else if (state.progress >= 1.0) (state.copy(doneTicks = state.doneTicks + 1), Cmd.none)
        else {
          val next = math.min(1.0, state.progress + 0.008)
          (state.copy(progress = next), Cmd.none)
        }
    }

    def subscriptions(state: LoadState) = Sub.time.everyMs(16, Tick)

    def view(state: LoadState) = {
      val width = 40
      val filled = (state.progress * width).toInt
      val bar: Seq[Element] = (0 until width).map { i =>
        if (i < filled) {
          val ratio = i.toDouble / width
          "█".color(Color.True(
            (ratio * 180).toInt + 50,
            ((1 - ratio) * 200).toInt + 55,
            255
          ))
        } else "░".color(Color.BrightBlack)
      }
      val pct = f"${state.progress * 100}%.0f%%"
      layout(
        rowTight(bar: _*),
        s"Linking... $pct".color(Color.BrightCyan)
      )
    }
  }

  def run(): Unit = {
    print("\u001b[2J\u001b[H")
    Thread.sleep(400)
    println("hello from a normal process")
    Thread.sleep(800)
    println("doing some work...")
    Thread.sleep(700)
    println("now watch this:")
    Thread.sleep(800)
    println()
    Loader.run(clearOnStart = false, clearOnExit = false, showQuitMessage = false)
    Thread.sleep(600)
    println()
    println("back to normal output")
    Thread.sleep(800)
  }
}
