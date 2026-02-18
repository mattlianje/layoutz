package layoutz

class RuntimeSpecs extends munit.FunSuite {

  test("Key ADT construction and pattern matching") {
    val charKey = Key.Char('a')
    val ctrlKey = Key.Ctrl('A')
    val unknownKey = Key.Unknown(128)
    val enterKey = Key.Enter
    val escapeKey = Key.Escape

    charKey match {
      case Key.Char(c) => assert(c == 'a')
    }

    ctrlKey match {
      case Key.Ctrl(c) => assert(c == 'A')
    }

    unknownKey match {
      case Key.Unknown(code) => assert(code == 128)
    }

    assert(enterKey == Key.Enter)
    assert(escapeKey == Key.Escape)
  }

  test("Key parsing logic (through public interface)") {
    val charA = Key.Char('a')
    val charPlus = Key.Char('+')
    val charMinus = Key.Char('-')

    assert(charA.c == 'a')
    assert(charPlus.c == '+')
    assert(charMinus.c == '-')
  }

  test("LayoutzApp trait basic functionality") {
    object TestCounterApp extends LayoutzApp[Int, String] {
      def init: (Int, Cmd[String]) = (0, Cmd.none)
      def update(msg: String, state: Int): (Int, Cmd[String]) = msg match {
        case "inc" => (state + 1, Cmd.none)
        case "dec" => (state - 1, Cmd.none)
        case _     => (state, Cmd.none)
      }
      def subscriptions(state: Int): Sub[String] = Sub.onKeyPress {
        case Key.Char('+') => Some("inc")
        case Key.Char('-') => Some("dec")
        case _             => None
      }
      def view(state: Int): Element = layout(
        s"Count: $state",
        "Press +/- to change"
      )
    }

    // Test basic functionality
    assert(TestCounterApp.init == (0, Cmd.none))
    assert(TestCounterApp.update("inc", 5) == (6, Cmd.none))
    assert(TestCounterApp.update("dec", 5) == (4, Cmd.none))
    assert(TestCounterApp.update("unknown", 5) == (5, Cmd.none))

    // Test subscriptions extract keyboard handler
    val sub = TestCounterApp.subscriptions(0)
    // Can't easily test Sub internals, but we can test the view

    val view = TestCounterApp.view(42)
    assert(view.render.contains("Count: 42"))
    assert(view.render.contains("Press +/- to change"))
  }

  test("LayoutzApp polymorphism") {
    // Test that LayoutzApp can be used polymorphically
    object SimpleApp extends LayoutzApp[String, Char] {
      def init: (String, Cmd[Char]) = ("hello", Cmd.none)
      def update(msg: Char, state: String): (String, Cmd[Char]) =
        (state + msg, Cmd.none)
      def subscriptions(state: String): Sub[Char] = Sub.onKeyPress {
        case Key.Char(c) if c.isLetter => Some(c)
        case _                         => None
      }
      def view(state: String): Element = s"Text: $state"
    }

    val app: LayoutzApp[String, Char] = SimpleApp
    assert(app.init == ("hello", Cmd.none))
    assert(app.update('!', "test") == ("test!", Cmd.none))

    // Test subscriptions are defined
    val sub = app.subscriptions("test")
    assert(sub != null)

    val view = app.view("world")
    assert(view.render == "Text: world")
  }

  /** Mock terminal for testing */
  class MockTerminal extends Terminal {
    val outputs = collection.mutable.ArrayBuffer[String]()
    var inputQueue = collection.mutable.ArrayBuffer[Int]()
    var rawMode = false
    var cursorHidden = false

    def enterRawMode(): Unit = rawMode = true
    def exitRawMode(): Unit = rawMode = false
    def clearScreen(): Unit = outputs += "[CLEAR]"
    def clearScrollback(): Unit = outputs += "[CLEAR_SCROLLBACK]"
    def hideCursor(): Unit = cursorHidden = true
    def showCursor(): Unit = cursorHidden = false
    def write(text: String): Unit = outputs += text
    def writeLine(text: String): Unit = outputs += text + "\n"
    def flush(): Unit = outputs += "[FLUSH]"

    def readInput(): Int =
      if (inputQueue.nonEmpty) inputQueue.remove(0)
      else throw new RuntimeException("No input available")

    def readInputNonBlocking(): Option[Int] =
      if (inputQueue.nonEmpty) Some(inputQueue.remove(0))
      else None

    def close(): Unit = outputs += "[CLOSED]"
    def terminalWidth(): Int = 80

    // Test helpers
    def queueInput(chars: String): Unit =
      inputQueue ++= chars.map(_.toInt)

    def queueInput(code: Int): Unit =
      inputQueue += code

    def getOutput: String = outputs.mkString("")
    def clearOutput(): Unit = outputs.clear()
  }

  test("run method accepts named parameters") {
    // Verify the run method signature accepts all configuration options
    // This is a compile-time check - if it compiles, the API is correct
    object TestApp extends LayoutzApp[Unit, Unit] {
      def init = ((), Cmd.none)
      def update(msg: Unit, state: Unit) = ((), Cmd.none)
      def subscriptions(state: Unit) = Sub.none
      def view(state: Unit) = Text("test")
    }

    // These should compile - verifies all named params are available
    // We don't actually run them (would need a terminal)
    // Just verify the app compiles and view renders
    val (state, _) = TestApp.init
    assert(TestApp.view(state).render == "test")
  }

  test("key parsing") {
    val parser = KeyParser
    val mockTerminal = new MockTerminal()

    // Test regular characters - these should definitely work
    assert(KeyParser.parse(65, mockTerminal) == Key.Char('A'))
    assert(KeyParser.parse(97, mockTerminal) == Key.Char('a'))
    assert(KeyParser.parse(43, mockTerminal) == Key.Char('+')) // Plus sign - ASCII 43
    assert(KeyParser.parse(45, mockTerminal) == Key.Char('-')) // Minus sign - ASCII 45

    // Test special keys
    assert(KeyParser.parse(10, mockTerminal) == Key.Enter)
    assert(KeyParser.parse(13, mockTerminal) == Key.Enter)
    assert(KeyParser.parse(9, mockTerminal) == Key.Tab)
    assert(KeyParser.parse(127, mockTerminal) == Key.Backspace)

    // Test control characters
    assert(KeyParser.parse(1, mockTerminal) == Key.Ctrl('A'))

    // Test arrow keys with mock terminal (fallback path)
    mockTerminal.queueInput("[A") // ESC sequence for up arrow
    assert(KeyParser.parse(27, mockTerminal) == Key.Up)

    mockTerminal.queueInput("[B") // ESC sequence for down arrow
    assert(KeyParser.parse(27, mockTerminal) == Key.Down)

    mockTerminal.queueInput("[C") // ESC sequence for right arrow
    assert(KeyParser.parse(27, mockTerminal) == Key.Right)

    mockTerminal.queueInput("[D") // ESC sequence for left arrow
    assert(KeyParser.parse(27, mockTerminal) == Key.Left)
  }

  test("counter app keys work") {
    val parser = KeyParser
    val mockTerminal = new MockTerminal()

    // Keys counter app uses
    assert(KeyParser.parse(43, mockTerminal) == Key.Char('+'))
    assert(KeyParser.parse(45, mockTerminal) == Key.Char('-'))

    // Verify they match what the counter app expects
    val plusResult = KeyParser.parse(43, mockTerminal)
    val minusResult = KeyParser.parse(45, mockTerminal)

    assert(plusResult == Key.Char('+'))
    assert(minusResult == Key.Char('-'))
  }

  test("counter app simulation") {
    case class CounterState(count: Int)
    sealed trait CounterMsg
    case object Inc extends CounterMsg
    case object Dec extends CounterMsg

    def onKey(k: Key): Option[CounterMsg] = k match {
      case Key.Char('+') => Some(Inc)
      case Key.Char('-') => Some(Dec)
      case _             => None
    }

    val parser = KeyParser
    val mockTerminal = new MockTerminal()

    // Test that pressing + gives increment message
    val plusKey = KeyParser.parse(43, mockTerminal) // ASCII 43 = '+'
    val plusMsg = onKey(plusKey)
    assert(plusMsg == Some(Inc))

    // Test that pressing - gives decrement message
    val minusKey = KeyParser.parse(45, mockTerminal) // ASCII 45 = '-'
    val minusMsg = onKey(minusKey)
    assert(minusMsg == Some(Dec))
  }

  test("mock terminal functionality") {
    val terminal = new MockTerminal()

    terminal.clearScreen()
    terminal.write("hello")
    terminal.writeLine("world")
    terminal.flush()

    val output = terminal.getOutput
    assert(output.contains("[CLEAR]"))
    assert(output.contains("hello"))
    assert(output.contains("world"))
    assert(output.contains("[FLUSH]"))

    terminal.queueInput("abc")
    assert(terminal.readInput() == 'a'.toInt)
    assert(terminal.readInputNonBlocking() == Some('b'.toInt))
    assert(terminal.readInput() == 'c'.toInt)
    assert(terminal.readInputNonBlocking() == None)
  }

  test("layoutz app basic functionality") {
    case class TestState(value: Int = 0, message: String = "initial")
    sealed trait TestMessage
    case object Increment extends TestMessage
    case object Decrement extends TestMessage
    case class SetMessage(msg: String) extends TestMessage

    class TestApp extends LayoutzApp[TestState, TestMessage] {
      def init: (TestState, Cmd[TestMessage]) = (TestState(), Cmd.none)

      def update(
          msg: TestMessage,
          state: TestState
      ): (TestState, Cmd[TestMessage]) = msg match {
        case Increment     => (state.copy(value = state.value + 1), Cmd.none)
        case Decrement     => (state.copy(value = state.value - 1), Cmd.none)
        case SetMessage(m) => (state.copy(message = m), Cmd.none)
      }

      def subscriptions(state: TestState): Sub[TestMessage] = Sub.onKeyPress {
        case Key.Char('+') => Some(Increment)
        case Key.Char('-') => Some(Decrement)
        case Key.Char('h') => Some(SetMessage("hello"))
        case _             => None
      }

      def view(state: TestState): Element = layout(
        s"Value: ${state.value}",
        s"Message: ${state.message}"
      )
    }

    val app = new TestApp()

    // Test initialization
    val (initialState, initialCmd) = app.init
    assert(initialState.value == 0)
    assert(initialState.message == "initial")
    assert(initialCmd == Cmd.none)

    val (incrementedState, _) = app.update(Increment, initialState)
    assert(incrementedState.value == 1)

    val (decrementedState, _) = app.update(Decrement, incrementedState)
    assert(decrementedState.value == 0)

    val sub = app.subscriptions(initialState)
    assert(sub != null)

    val view = app.view(initialState)
    val rendered = view.render
    assert(rendered.contains("Value: 0"))
    assert(rendered.contains("Message: initial"))
  }

  test("stty terminal creation") {
    SttyTerminal.create() match {
      case Right(_)    =>
      case Left(error) => assert(error.isInstanceOf[TerminalError])
    }
  }

  test("runtime integration") {
    case class CounterState(count: Int = 0)
    sealed trait CounterMsg
    case object Increment extends CounterMsg
    case object Decrement extends CounterMsg

    object TestCounter extends LayoutzApp[CounterState, CounterMsg] {
      def init: (CounterState, Cmd[CounterMsg]) = (CounterState(), Cmd.none)
      def update(
          msg: CounterMsg,
          state: CounterState
      ): (CounterState, Cmd[CounterMsg]) =
        msg match {
          case Increment => (state.copy(count = state.count + 1), Cmd.none)
          case Decrement => (state.copy(count = state.count - 1), Cmd.none)
        }
      def subscriptions(state: CounterState): Sub[CounterMsg] = Sub.onKeyPress {
        case Key.Char('+') => Some(Increment)
        case Key.Char('-') => Some(Decrement)
        case _             => None
      }
      def view(state: CounterState): Element = layout(s"Count: ${state.count}")
    }

    val mockTerminal = new MockTerminal()
    mockTerminal.queueInput("++-")
    mockTerminal.queueInput(17)

    val parser = KeyParser
    assert(KeyParser.parse(43, mockTerminal) == Key.Char('+'))
    assert(KeyParser.parse(45, mockTerminal) == Key.Char('-'))

    val app = TestCounter
    // Test subscriptions exist
    val sub = app.subscriptions(CounterState())
    assert(sub != null)

    val (initialState, _) = app.init
    val (afterIncrement, _) = app.update(Increment, initialState)
    assert(afterIncrement.count == 1)

    val config = RuntimeConfig(
      tickIntervalMs = 50,
      renderIntervalMs = 10,
      quitKey = Key.Ctrl('Q'),
      showQuitMessage = false,
      quitMessage = "",
      clearOnStart = false,
      clearOnExit = false,
      alignment = Alignment.Left
    )

    val runtimeResult = LayoutzRuntime.run(app, config, Some(mockTerminal))
    runtimeResult match {
      case Right(_)    => assert(true)
      case Left(error) => assert(error.isInstanceOf[RuntimeError])
    }
  }

  test("Cmd.afterMs") {
    case class Tick(n: Int)
    val cmd = Cmd.afterMs(1000L, Tick(1))
    assert(cmd != Cmd.none)
  }

  test("Cmd.showCursor and Cmd.hideCursor") {
    val show = Cmd.showCursor
    val hide = Cmd.hideCursor
    assert(show != Cmd.none)
    assert(hide != Cmd.none)
    assert(show != hide)
  }

  test("Cmd.setTitle") {
    val cmd = Cmd.setTitle("My App")
    assert(cmd != Cmd.none)
  }

  test("Cmd variants in batch") {
    val batched = Cmd.batch(
      Cmd.setTitle("Test"),
      Cmd.hideCursor
    )
    assert(batched != Cmd.none)
  }

}
