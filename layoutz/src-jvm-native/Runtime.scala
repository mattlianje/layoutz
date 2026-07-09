package layoutz

import scala.concurrent.{Future, ExecutionContext}
import scala.io.Source
import scala.util.Try
import java.io.{PrintWriter, File}

sealed trait Key
object Key {
  final case class Char(c: scala.Char) extends Key
  final case class Ctrl(c: scala.Char) extends Key
  final case class Unknown(code: Int) extends Key
  case object Enter extends Key
  case object Escape extends Key
  case object Tab extends Key
  case object Backspace extends Key
  case object Delete extends Key
  case object Up extends Key
  case object Down extends Key
  case object Left extends Key
  case object Right extends Key
  case object Home extends Key
  case object End extends Key
  case object PageUp extends Key
  case object PageDown extends Key
}

/** Commands represent side effects to execute. Commands don't execute immediately, but are returned
  * from init/update to be run by the runtime.
  */
sealed trait Cmd[+Msg]

object Cmd {
  def none[Msg]: Cmd[Msg] = CmdNone
  def exit[Msg]: Cmd[Msg] = CmdExit
  def halt(code: Int = 0): Cmd[Nothing] = CmdHalt(code)
  def batch[Msg](cmds: Cmd[Msg]*): Cmd[Msg] = CmdBatch(cmds.toList)

  implicit def stateWithNoCmd[State, Msg](state: State): (State, Cmd[Msg]) =
    (state, none[Msg])

  object file {
    def read[Msg](path: String, onResult: Either[String, String] => Msg): Cmd[Msg] =
      CmdFileRead(path, onResult)

    def write[Msg](
        path: String,
        content: String,
        onResult: Either[String, Unit] => Msg
    ): Cmd[Msg] =
      CmdFileWrite(path, content, onResult)

    def ls[Msg](path: String, onResult: Either[String, List[String]] => Msg): Cmd[Msg] =
      CmdFileLs(path, onResult)

    def cwd[Msg](onResult: Either[String, String] => Msg): Cmd[Msg] =
      CmdFileCwd(onResult)
  }

  object http {
    def get[Msg](
        url: String,
        onResult: Either[String, String] => Msg,
        headers: Map[String, String] = Map.empty
    ): Cmd[Msg] = CmdHttpGet(url, headers, onResult)

    def post[Msg](
        url: String,
        body: String,
        onResult: Either[String, String] => Msg,
        headers: Map[String, String] = Map.empty
    ): Cmd[Msg] = CmdHttpPost(url, body, headers, onResult)
  }

  object clipboard {
    def read[Msg](onResult: Either[String, String] => Msg): Cmd[Msg] =
      CmdClipboardRead(onResult)

    def write[Msg](content: String, onResult: Either[String, Unit] => Msg): Cmd[Msg] =
      CmdClipboardWrite(content, onResult)
  }

  def task[A, Msg](run: => A)(toMsg: Either[String, A] => Msg): Cmd[Msg] =
    CmdTask(() => Try(run).toEither.left.map(_.getMessage), toMsg)

  def fire(effect: => Unit): Cmd[Nothing] = CmdFire(() => effect)

  def afterMs[Msg](delayMs: Long, msg: Msg): Cmd[Msg] = CmdAfterMs(delayMs, msg)
  def showCursor: Cmd[Nothing] = CmdShowCursor
  def hideCursor: Cmd[Nothing] = CmdHideCursor
  def setTitle(title: String): Cmd[Nothing] = CmdSetTitle(title)
}

private case object CmdNone extends Cmd[Nothing]
private case object CmdExit extends Cmd[Nothing]
private case class CmdHalt(code: Int) extends Cmd[Nothing]
private case class CmdBatch[Msg](cmds: List[Cmd[Msg]]) extends Cmd[Msg]

private case class CmdFileRead[Msg](
    path: String,
    onResult: Either[String, String] => Msg
) extends Cmd[Msg]

private case class CmdFileWrite[Msg](
    path: String,
    content: String,
    onResult: Either[String, Unit] => Msg
) extends Cmd[Msg]

private case class CmdFileLs[Msg](
    path: String,
    onResult: Either[String, List[String]] => Msg
) extends Cmd[Msg]

private case class CmdFileCwd[Msg](onResult: Either[String, String] => Msg)
    extends Cmd[Msg]

private case class CmdHttpGet[Msg](
    url: String,
    headers: Map[String, String],
    onResult: Either[String, String] => Msg
) extends Cmd[Msg]

private case class CmdHttpPost[Msg](
    url: String,
    body: String,
    headers: Map[String, String],
    onResult: Either[String, String] => Msg
) extends Cmd[Msg]

private case class CmdClipboardRead[Msg](
    onResult: Either[String, String] => Msg
) extends Cmd[Msg]

private case class CmdClipboardWrite[Msg](
    content: String,
    onResult: Either[String, Unit] => Msg
) extends Cmd[Msg]

private case class CmdTask[A, Msg](
    task: () => Either[String, A],
    toMsg: Either[String, A] => Msg
) extends Cmd[Msg]

private case class CmdFire(effect: () => Unit) extends Cmd[Nothing]

private case class CmdAfterMs[Msg](delayMs: Long, msg: Msg) extends Cmd[Msg]
private case object CmdShowCursor extends Cmd[Nothing]
private case object CmdHideCursor extends Cmd[Nothing]
private case class CmdSetTitle(title: String) extends Cmd[Nothing]

/** Subscriptions represent ongoing event sources. They declare what events your app is interested
  * in based on the current state.
  */
sealed trait Sub[+Msg]

object Sub {
  def none[Msg]: Sub[Msg] = SubNone
  def onKeyPress[Msg](handler: Key => Option[Msg]): Sub[Msg] = OnKeyPress(handler)
  def batch[Msg](subs: Sub[Msg]*): Sub[Msg] = SubBatch(subs.toList)

  object time {
    def everyMs[Msg](intervalMs: Long, msg: Msg): Sub[Msg] = OnTimeEveryMs(intervalMs, () => msg)
    def everyDynamicMs[Msg](intervalMs: Long, msgGenerator: () => Msg): Sub[Msg] =
      OnTimeEveryMs(intervalMs, msgGenerator)
  }

  object file {
    def watch[Msg](path: String, onChange: Either[String, String] => Msg): Sub[Msg] =
      OnFileWatch(path, onChange)
  }

  object http {
    def pollMs[Msg](
        url: String,
        intervalMs: Long,
        onResponse: Either[String, String] => Msg,
        headers: Map[String, String] = Map.empty
    ): Sub[Msg] = OnHttpPollMs(url, intervalMs, headers, onResponse)
  }
}

private case object SubNone extends Sub[Nothing]

private case class OnKeyPress[Msg](handler: Key => Option[Msg])
    extends Sub[Msg]

private case class OnTimeEveryMs[Msg](intervalMs: Long, msgGenerator: () => Msg)
    extends Sub[Msg]

private case class OnFileWatch[Msg](
    path: String,
    onChange: Either[String, String] => Msg
) extends Sub[Msg]

private case class OnHttpPollMs[Msg](
    url: String,
    intervalMs: Long,
    headers: Map[String, String],
    onResponse: Either[String, String] => Msg
) extends Sub[Msg]

private case class SubBatch[Msg](subs: List[Sub[Msg]]) extends Sub[Msg]

sealed trait Alignment
object Alignment {
  case object Left extends Alignment
  case object Center extends Alignment
  case object Right extends Alignment
}

private[layoutz] case class RuntimeConfig(
    tickIntervalMs: Long,
    renderIntervalMs: Long,
    quitKey: Key,
    showQuitMessage: Boolean,
    quitMessage: String,
    clearOnStart: Boolean,
    clearOnExit: Boolean,
    alignment: Alignment
)

trait Terminal {
  def enterRawMode(): Unit
  def exitRawMode(): Unit
  def clearScreen(): Unit
  def clearScrollback(): Unit
  def hideCursor(): Unit
  def showCursor(): Unit
  def write(text: String): Unit
  def writeLine(text: String): Unit
  def flush(): Unit
  def readInput(): Int
  def readInputNonBlocking(): Option[Int]
  def close(): Unit
  def terminalWidth(): Int
}

sealed trait RuntimeError

case class TerminalError(message: String, cause: Option[Throwable] = None)
    extends RuntimeError

case class RenderError(message: String, cause: Option[Throwable] = None)
    extends RuntimeError

case class InputError(message: String, cause: Option[Throwable] = None)
    extends RuntimeError

/** Pure Scala terminal using stty for raw mode (zero dependencies) */
class SttyTerminal private () extends Terminal {
  private var originalStty = ""

  def enterRawMode(): Unit = {
    originalStty = stty("-g").trim
    stty("-echo -icanon -ixon min 1")
  }

  def exitRawMode(): Unit = if (originalStty.nonEmpty) stty(originalStty)
  def clearScreen(): Unit = esc("2J", "H")
  def clearScrollback(): Unit = esc("3J", "2J", "H")
  def hideCursor(): Unit = esc("?25l")
  def showCursor(): Unit = esc("?25h")
  def write(text: String): Unit = System.out.print(text)
  def writeLine(text: String): Unit = System.out.println(text)
  def flush(): Unit = System.out.flush()
  def readInput(): Int = System.in.read()

  def readInputNonBlocking(): Option[Int] =
    try if (System.in.available() > 0) Some(System.in.read()) else None
    catch { case _: Exception => None }

  def close(): Unit = exitRawMode()

  /** Terminal width via an ANSI cursor-position report — no subprocess.
    * Park the cursor far past the right edge, ask where it actually landed,
    * and read back `ESC [ rows ; cols R`. Falls back to 80 on timeout/parse
    * failure. Must be called while raw mode is active and nothing else is
    * reading stdin (otherwise the reply gets consumed as keystrokes).
    */
  def terminalWidth(): Int = {
    write(Ansi.ESC + "7" + Ansi.CSI + "999;999H" + Ansi.CSI + "6n" + Ansi.ESC + "8")
    flush()
    val deadline = System.currentTimeMillis() + 100
    val sb = new StringBuilder
    var done = false
    while (!done && System.currentTimeMillis() < deadline)
      readInputNonBlocking() match {
        case Some('R') => done = true
        case Some(c)   => sb.append(c.toChar)
        case None      => Thread.sleep(2)
      }
    val resp = sb.toString
    val semi = resp.lastIndexOf(';')
    if (semi >= 0) Try(resp.substring(semi + 1).trim.toInt).getOrElse(80) else 80
  }

  private def esc(codes: String*): Unit = {
    codes.foreach(c => System.out.print(Ansi.CSI + c))
    System.out.flush()
  }

  /* Raw-mode toggling still needs the tty's canonical/echo flags, which the
   * JVM can't reach without JNI — so stty stays here (stderr silenced so a
   * teardown race can't spill "tcsetattr: Input/output error" to the shell).
   */
  private def stty(args: String): String = {
    import scala.sys.process._
    try Seq("sh", "-c", s"stty $args < /dev/tty 2>/dev/null").!!
    catch { case _: Exception => "" }
  }

}

object SttyTerminal {

  def create(): Either[RuntimeError, SttyTerminal] =
    try Right(new SttyTerminal())
    catch {
      case ex: Exception =>
        Left(
          TerminalError(
            s"Failed to create terminal: ${ex.getMessage}",
            Some(ex)
          )
        )
    }

}

object KeyParser {

  def parse(input: Int, terminal: Terminal): Key = input match {
    case '\n' | '\r'              => Key.Enter
    case '\t'                     => Key.Tab
    case 27                       => parseEscape(terminal)
    case 8 | 127                  => Key.Backspace
    case c if c >= 32 && c <= 126 => Key.Char(c.toChar)
    case c if c >= 1 && c <= 26   => Key.Ctrl((c + 64).toChar)
    case c                        => Key.Unknown(c)
  }

  private def parseEscape(terminal: Terminal): Key =
    try {
      Thread.sleep(5)
      terminal.readInputNonBlocking() match {
        case Some('[') =>
          Thread.sleep(5)
          terminal.readInputNonBlocking() match {
            case Some('A') => Key.Up
            case Some('B') => Key.Down
            case Some('C') => Key.Right
            case Some('D') => Key.Left
            case Some('H') => Key.Home
            case Some('F') => Key.End
            case Some('5') => consumeTilde(terminal); Key.PageUp
            case Some('6') => consumeTilde(terminal); Key.PageDown
            case Some('3') => consumeTilde(terminal); Key.Delete
            case _         => Key.Escape
          }
        case _ => Key.Escape
      }
    } catch { case _: Exception => Key.Escape }

  private def consumeTilde(terminal: Terminal): Unit = {
    Thread.sleep(5)
    terminal.readInputNonBlocking() // consume the '~'
  }

}

object input {

  /** Handle a key for a text field
    *
    * @param key
    *   The key that was pressed
    * @param fieldId
    *   Which field this is (0, 1, 2, etc.)
    * @param activeField
    *   Which field is currently active
    * @param currentValue
    *   The current text in the field
    * @return
    *   Some(newValue) if the key was handled, None if not
    *
    * Example:
    * {{{
    * case Key.Char(c) => input.handle(Key.Char(c), 0, state.activeField, state.name) match {
    *   case Some(newValue) => Some(UpdateName(newValue))
    *   case None => None
    * }
    * }}}
    */
  def handle(
      key: Key,
      fieldId: Int,
      activeField: Int,
      currentValue: String
  ): Option[String] = {
    if (activeField != fieldId) return None

    key match {
      case Key.Char(c)
          if c.isLetterOrDigit || c.isWhitespace ||
            "!@#$%^&*()_+-=[]{}|;':,.<>?/\\\"".contains(c) =>
        Some(currentValue + c)
      case Key.Backspace if currentValue.nonEmpty =>
        Some(currentValue.dropRight(1))
      case _ => None
    }
  }

}

/** File-loading wrapper around [[kittyImage]] */
object kitty {

  /** Load an image file and size it to a `cols` x `rows` cell footprint. PNG bytes are sent to the
    * terminal as-is..
    */
  def image(path: String, cols: Int, rows: Int, alt: String = ""): KittyImage = {
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path))
    kittyImage(bytes, cols, rows, alt)
  }
}

/** Elm Architecture app: init, update, subscriptions, view */
trait LayoutzApp[State, Message] {
  def init: (State, Cmd[Message])
  def update(msg: Message, state: State): (State, Cmd[Message])
  def subscriptions(state: State): Sub[Message]
  def view(state: State): Element

  def run: Unit = run()

  def run(
      tickIntervalMs: Long = 100,
      renderIntervalMs: Long = 50,
      quitKey: Key = Key.Ctrl('Q'),
      showQuitMessage: Boolean = false,
      quitMessage: String = "Press Ctrl+Q to quit",
      clearOnStart: Boolean = true,
      clearOnExit: Boolean = true,
      alignment: Alignment = Alignment.Left,
      terminal: Option[Terminal] = None,
      executionContext: Option[ExecutionContext] = None
  ): Unit = {
    val config = RuntimeConfig(
      tickIntervalMs,
      renderIntervalMs,
      quitKey,
      showQuitMessage,
      quitMessage,
      clearOnStart,
      clearOnExit,
      alignment
    )
    LayoutzRuntime.run(this, config, terminal, executionContext)
  }
}

private[layoutz] object LayoutzRuntime {

  def run[S, M](
      app: LayoutzApp[S, M],
      config: RuntimeConfig,
      terminal: Option[Terminal] = None,
      executionContext: Option[ExecutionContext] = None
  ): Either[RuntimeError, Unit] =
    terminal.orElse(SttyTerminal.create().toOption) match {
      case Some(t) =>
        Right(
          try new RuntimeInstance(
              app,
              config,
              t,
              executionContext.getOrElse(ExecutionContext.global)
            ).run()
          finally t.close()
        )
      case None => Left(TerminalError("Failed to initialize terminal"))
    }

  private class RuntimeInstance[State, Message](
      app: LayoutzApp[State, Message],
      config: RuntimeConfig,
      terminal: Terminal,
      executionContext: ExecutionContext
  ) {
    @volatile private var currentState: State = _
    @volatile private var shouldContinue = true
    private val stateLock = new Object()

    /* Width is sampled once at startup (before the input thread runs, so the
     * cursor-position reply isn't swallowed as keystrokes) and reused for the
     * lifetime of the app, mirroring the Haskell runtime.
     */
    @volatile private var cachedWidth = 80

    /* Restore the terminal if the JVM dies before normal cleanup runs:
     * Ctrl+C / SIGINT (raw mode leaves signals enabled), kill, or an
     * uncaught error. Without this the cursor stays hidden and the tty
     * stays in raw mode after the process exits.
     */
    private val shutdownHook = new Thread(
      () =>
        try {
          /* Signal the loops to stop first so they don't spew read errors
           * while we tear the terminal down. */
          shouldContinue = false
          if (config.clearOnExit) terminal.clearScrollback()
          terminal.showCursor()
          terminal.exitRawMode()
          terminal.flush()
        } catch { case _: Throwable => () },
      "layoutz-cleanup"
    )

    def run(): Unit = {
      initialize()
      java.lang.Runtime.getRuntime.addShutdownHook(shutdownHook)
      val renderThread = new Thread(() => runRenderLoop(), "LayoutzRender")
      val tickThread = new Thread(() => runTickLoop(), "LayoutzTick")
      val inputThread = new Thread(() => runInputLoop(), "LayoutzInput")
      renderThread.setDaemon(true)
      tickThread.setDaemon(true)
      inputThread.setDaemon(true)
      renderThread.start(); tickThread.start(); inputThread.start()

      try while (shouldContinue) Thread.sleep(50)
      catch { case _: InterruptedException => () }
      finally {
        cleanup()
        /* Hook no longer needed; ignore if a shutdown is already underway. */
        try java.lang.Runtime.getRuntime.removeShutdownHook(shutdownHook)
        catch { case _: Throwable => () }
      }
    }

    private def initialize(): Unit = {
      terminal.enterRawMode()
      if (config.clearOnStart) terminal.clearScreen()
      terminal.hideCursor()
      /* Sample width now: raw mode is on and no other thread reads stdin yet. */
      cachedWidth = terminal.terminalWidth()
      val (initialState, initialCmd) = app.init
      currentState = initialState
      processCommand(initialCmd)
    }

    private def cleanup(): Unit = {
      if (config.clearOnExit) terminal.clearScrollback()
      terminal.showCursor()
      terminal.flush()
    }

    /* Command executors */
    private def execFileRead(path: String): Either[String, String] =
      Try(Source.fromFile(path).mkString).toEither.left.map(e =>
        s"Read failed: ${e.getMessage}"
      )

    private def execFileWrite(
        path: String,
        content: String
    ): Either[String, Unit] =
      Try {
        val w = new java.io.PrintWriter(path);
        try w.write(content)
        finally w.close()
      }.toEither.left.map(e => s"Write failed: ${e.getMessage}")

    private def execFileLs(path: String): Either[String, List[String]] = {
      val f = new java.io.File(path)
      if (!f.exists()) Left(s"Path does not exist: $path")
      else if (!f.isDirectory()) Left(s"Not a directory: $path")
      else
        Option(f.listFiles())
          .toRight(s"Cannot read: $path")
          .map(_.map(_.getName()).sorted.toList)
    }

    private def execFileCwd(): Either[String, String] =
      Try(System.getProperty("user.dir")).toEither.left.map(e =>
        s"Failed: ${e.getMessage}"
      )

    private def execHttpGet(
        url: String,
        headers: Map[String, String]
    ): Either[String, String] = {
      import scala.sys.process._
      Try {
        val h = headers.flatMap { case (k, v) => Seq("-H", s"$k: $v") }
        (Seq("curl", "-s", "-f") ++ h ++ Seq(url)).!!
      }.toEither.left.map(e => s"HTTP GET failed: ${e.getMessage}")
    }

    private def execHttpPost(
        url: String,
        body: String,
        headers: Map[String, String]
    ): Either[String, String] = {
      import scala.sys.process._
      Try {
        val h = headers.flatMap { case (k, v) => Seq("-H", s"$k: $v") }
        (Seq("curl", "-s", "-f", "-X", "POST", "-d", body) ++ h ++ Seq(
          url
        )).!!
      }.toEither.left.map(e => s"HTTP POST failed: ${e.getMessage}")
    }

    private def execClipboardRead(): Either[String, String] = {
      import scala.sys.process._
      Try {
        val os = System.getProperty("os.name").toLowerCase
        if (os.contains("mac")) "pbpaste".!!
        else Seq("xclip", "-selection", "clipboard", "-o").!!
      }.toEither.left.map(e => s"Clipboard read failed: ${e.getMessage}")
    }

    private def execClipboardWrite(content: String): Either[String, Unit] = {
      import scala.sys.process._
      Try {
        val os = System.getProperty("os.name").toLowerCase
        val cmd = if (os.contains("mac")) Seq("pbcopy") else Seq("xclip", "-selection", "clipboard")
        val io = new ProcessIO(
          in => { in.write(content.getBytes); in.close() },
          _ => (),
          _ => ()
        )
        val proc = cmd.run(io)
        proc.exitValue(): Unit
      }.toEither.left.map(e => s"Clipboard write failed: ${e.getMessage}")
    }

    private def processCommand(cmd: Cmd[Message]): Unit = {
      implicit val ec: ExecutionContext = executionContext

      cmd match {
        case CmdNone        =>
        case CmdExit        => shouldContinue = false
        case CmdHalt(code)  => cleanup(); System.exit(code)
        case CmdBatch(cmds) => cmds.foreach(processCommand)
        case CmdFileRead(p, f) =>
          Future(execFileRead(p)).foreach(r => updateState(f(r)))
        case CmdFileWrite(p, c, f) =>
          Future(execFileWrite(p, c)).foreach(r => updateState(f(r)))
        case CmdFileLs(p, f) =>
          Future(execFileLs(p)).foreach(r => updateState(f(r)))
        case CmdFileCwd(f) =>
          Future(execFileCwd()).foreach(r => updateState(f(r)))
        case CmdHttpGet(u, h, f) =>
          Future(execHttpGet(u, h)).foreach(r => updateState(f(r)))
        case CmdHttpPost(u, b, h, f) =>
          Future(execHttpPost(u, b, h)).foreach(r => updateState(f(r)))
        case CmdClipboardRead(f) =>
          Future(execClipboardRead()).foreach(r => updateState(f(r)))
        case CmdClipboardWrite(c, f) =>
          Future(execClipboardWrite(c)).foreach(r => updateState(f(r)))
        case CmdTask(t, f)   => Future(t()).foreach(r => updateState(f(r)))
        case CmdFire(effect) => Future(effect())
        case CmdAfterMs(delayMs, msg) =>
          Future { Thread.sleep(delayMs); updateState(msg) }
        case CmdShowCursor  => terminal.showCursor(); terminal.flush()
        case CmdHideCursor  => terminal.hideCursor(); terminal.flush()
        case CmdSetTitle(t) => terminal.write(Ansi.setTitle(t)); terminal.flush()
      }
    }

    private def updateState(msg: Message): Unit = stateLock.synchronized {
      val (newState, cmd) = app.update(msg, currentState)
      currentState = newState
      processCommand(cmd)
    }

    private def readState(): State = stateLock.synchronized(currentState)

    private def flattenSubs(sub: Sub[Message]): List[Sub[Message]] =
      sub match {
        case SubNone        => Nil
        case SubBatch(subs) => subs.flatMap(flattenSubs)
        case other          => List(other)
      }

    private def getSubs: List[Sub[Message]] = flattenSubs(
      app.subscriptions(readState())
    )

    private def getKeyPressHandler(): Option[Key => Option[Message]] =
      getSubs.collectFirst { case OnKeyPress(h) => h }

    private def getTimeSubscriptions(): List[(Long, () => Message)] =
      getSubs.collect { case OnTimeEveryMs(ms, gen) => (ms, gen) }

    private def getFileWatchSubscriptions(): List[(String, Either[String, String] => Message)] =
      getSubs.collect { case OnFileWatch(p, f) => (p, f) }

    private def getHttpPollSubscriptions(): List[
      (String, Long, Map[String, String], Either[String, String] => Message)
    ] =
      getSubs.collect { case OnHttpPollMs(u, ms, h, f) => (u, ms, h, f) }

    private val lastTickTimes = scala.collection.mutable.Map[Long, Long]()

    private val lastModifiedTimes =
      scala.collection.mutable.Map[String, Long]()

    private val lastPollTimes = scala.collection.mutable.Map[String, Long]()

    private val sentKittyIds = scala.collection.mutable.Set.empty[Int]

    private def runRenderLoop(): Unit = {
      var lastRenderedState: Option[String] = None
      var lastLineCount: Int = 0

      while (shouldContinue)
        try {
          val currentRender = app.view(readState()).render
          if (lastRenderedState.forall(_ != currentRender)) {
            val deduped = KittyProtocol.dedupeTransmits(currentRender, sentKittyIds)
            val fullRender = if (config.showQuitMessage) {
              deduped + "\n\n" + config.quitMessage
            } else {
              deduped
            }
            val renderedLines = fullRender.split("\n", -1)
            val currentLineCount = renderedLines.length

            /* Move cursor to start of our render area */
            if (config.clearOnStart) {
              terminal.write(Ansi.CURSOR_HOME)
            } else if (lastLineCount > 0) {
              terminal.write(Ansi.cursorUp(lastLineCount))
            }

            /* Apply alignment to layout as a block (uniform margin for all lines) */
            val termWidth = cachedWidth
            val lineWidths = renderedLines.map(realLength)
            val maxLineWidth = if (lineWidths.isEmpty) 0 else lineWidths.max
            val blockPad = config.alignment match {
              case Alignment.Left   => 0
              case Alignment.Center => math.max(0, (termWidth - maxLineWidth) / 2)
              case Alignment.Right  => math.max(0, termWidth - maxLineWidth)
            }
            val padding = " " * blockPad
            val alignedLines =
              if (blockPad > 0) renderedLines.map(padding + _) else renderedLines

            /* Write each line with clear-to-end-of-line */
            alignedLines.foreach { line =>
              terminal.write(line + Ansi.CLEAR_TO_EOL + "\n")
            }
            /* Clear any extra lines from the previous render */
            val extraLines = lastLineCount - currentLineCount
            if (extraLines > 0) {
              (0 until extraLines).foreach { _ =>
                terminal.write(Ansi.CLEAR_TO_EOL + "\n")
              }
            }
            terminal.flush()

            lastRenderedState = Some(currentRender)
            lastLineCount = currentLineCount
          }
          Thread.sleep(config.renderIntervalMs)
        } catch {
          case ex: Exception => handleRenderError(ex)
        }
    }

    private def runTickLoop(): Unit =
      while (shouldContinue)
        try {
          val currentTime = System.currentTimeMillis()
          val timeSubs = getTimeSubscriptions()
          timeSubs.foreach { case (intervalMs, generator) =>
            val lastTime = lastTickTimes.getOrElse(intervalMs, 0L)
            if (currentTime - lastTime >= intervalMs) {
              lastTickTimes(intervalMs) = currentTime
              val msg = generator()
              updateState(msg)
            }
          }

          /* Simple polling for now */
          val fileWatchSubs = getFileWatchSubscriptions()
          fileWatchSubs.foreach { case (path, onChange) =>
            try {
              val file = new java.io.File(path)
              if (file.exists()) {
                val currentModified = file.lastModified()
                val lastModified = lastModifiedTimes.getOrElse(path, 0L)
                if (currentModified > lastModified) {
                  lastModifiedTimes(path) = currentModified
                  /* Not firing on first check */
                  if (lastModified > 0) {
                    val content = Source.fromFile(path).mkString
                    updateState(onChange(Right(content)))
                  }
                }
              } else {
                lastModifiedTimes(path) = 0L
              }
            } catch {
              case ex: Exception =>
                updateState(
                  onChange(Left(s"File watch error: ${ex.getMessage}"))
                )
            }
          }

          val httpPollSubs = getHttpPollSubscriptions()
          httpPollSubs.foreach {
            case (url, intervalMs, headers, onResponse) =>
              val lastPoll = lastPollTimes.getOrElse(url, 0L)
              if (currentTime - lastPoll >= intervalMs) {
                lastPollTimes(url) = currentTime
                scala.concurrent.Future {
                  import scala.sys.process._
                  try {
                    val headerArgs = headers.flatMap { case (k, v) =>
                      Seq("-H", s"$k: $v")
                    }
                    val cmd =
                      Seq("curl", "-s", "-f") ++ headerArgs ++ Seq(url)
                    val response = cmd.!!
                    updateState(onResponse(Right(response)))
                  } catch {
                    case ex: Exception =>
                      updateState(
                        onResponse(
                          Left(s"HTTP poll failed: ${ex.getMessage}")
                        )
                      )
                  }
                }(executionContext)
              }
          }

          Thread.sleep(10) /* Short sleep to avoid busy wait */
        } catch {
          case ex: Exception => handleTickError(ex)
        }

    private def runInputLoop(): Unit =
      while (shouldContinue)
        try {
          val input = terminal.readInput()
          if (input == -1) {
            /* stdin closed (EOF) — exit quietly rather than busy-looping. */
            shouldContinue = false
          } else {
            val key = KeyParser.parse(input, terminal)
            if (key == config.quitKey) {
              shouldContinue = false
            } else {
              /* Uses the current subscription's key handler */
              getKeyPressHandler().foreach { handler =>
                handler(key).foreach(updateState)
              }
            }
          }
        } catch {
          /* During teardown reads fail by design — stay silent. */
          case _: Exception if !shouldContinue => ()
          case ex: Exception =>
            handleInputError(ex)
            Thread.sleep(10)
        }

    private def handleRenderError(ex: Throwable): Unit = {
      terminal.writeLine(s"Render error: ${ex.getMessage}")
      terminal.flush()
    }

    private def handleTickError(ex: Throwable): Unit = {}

    private def handleInputError(ex: Throwable): Unit =
      try {
        terminal.writeLine(s"\nInput error: ${ex.getMessage}")
        terminal.flush()
      } catch {
        case _: Exception =>
      }

  }

}

/** Visual preset for [[loader]]. Controls the bar glyphs and color for bounded loaders, and the
  * spinner frames + color for streaming loaders.
  */
private[layoutz] final case class LoaderStyle(
    fill: Char = '█',
    empty: Char = '░',
    open: String = "",
    close: String = "",
    head: String = "",
    smooth: Boolean = false,
    spinner: SpinnerStyle = SpinnerStyle.Dots,
    color: Color = Color.Cyan
)

private[layoutz] object LoaderStyle {
  /** Smooth gradient blocks `████▊░░░` (default). */
  val Blocks = LoaderStyle(smooth = true, spinner = SpinnerStyle.Grow)

  /** Classic bracketed bar `[██████░░░░]`. */
  val Bar = LoaderStyle(open = "[", close = "]", color = Color.Green)

  /** Retro ASCII with an arrow head `[====>    ]`. */
  val Ascii = LoaderStyle('=', ' ', "[", "]", ">", spinner = SpinnerStyle.Line, color = Color.Yellow)

  /** Dotted `●●●●∙∙∙∙`. */
  val Dots = LoaderStyle('●', '∙', spinner = SpinnerStyle.Bounce, color = Color.Magenta)

  /** Heavy/light rule `━━━━────`. */
  val Line = LoaderStyle('━', '─', spinner = SpinnerStyle.Line, color = Color.Blue)

  /** Segmented pipes `▰▰▰▱▱▱`. */
  val Pipes = LoaderStyle('▰', '▱', spinner = SpinnerStyle.Arrow, color = Color.BrightMagenta)

  /** All built-in styles, in display order. */
  val all: Seq[LoaderStyle] = Seq(Blocks, Bar, Ascii, Dots, Line, Pipes)
}

/** Wrap any collection or iterator and stream a live progress bar to the terminal as you iterate.
  * Spins up a tiny [[LayoutzApp]] on a background daemon thread; the caller only sees an
  * `Iterator`.
  *
  * {{{
  * for (n <- loader("Processing", 1 to 100)) Thread.sleep(20)
  * for (n <- loader("Rendering", 1 to 100).ascii) Thread.sleep(20)
  * for (line <- loader.stream("Reading", io.Source.fromFile("big.log").getLines())) handle(line)
  * }}}
  */
object loader {
  import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}

  /** Wrap a sized collection ...renders an inline progress bar with N/total + %. */
  def apply[A](items: Iterable[A]): Loader[A] = apply("", items)
  def apply[A](label: String, items: Iterable[A]): Loader[A] =
    new Loader(label, LoaderStyle.Blocks, (l, s) => new BoundedIter(l, items.size, items.iterator, s))

  /** Wrap an iterator whose size isn't known ..renders a spinner + running count. */
  def stream[A](items: Iterator[A]): Loader[A] = stream("", items)
  def stream[A](label: String, items: Iterator[A]): Loader[A] =
    new Loader(label, LoaderStyle.Blocks, (l, s) => new StreamingIter(l, items, s))

  /** A lazy, restyleable progress wrapper. Iterate it (usually via `for`) to start the display;
    * chain a style accessor first to change its look:
    *
    * {{{
    * for (f <- loader("Resizing", files).ascii) resize(f)
    * for (u <- loader("Crawling", urls).styled(fill = '#', color = Color.Green)) fetch(u)
    * }}}
    */
  final class Loader[A] private[loader] (
      label: String,
      style: LoaderStyle,
      mk: (String, LoaderStyle) => Iterator[A]
  ) extends Iterable[A] {
    def iterator: Iterator[A] = mk(label, style)

    private def withStyle(s: LoaderStyle): Loader[A] = new Loader(label, s, mk)

    /** Smooth gradient blocks `████▊░░░` (default). */
    def blocks: Loader[A] = withStyle(LoaderStyle.Blocks)

    /** Classic bracketed bar `[██████░░░░]`. */
    def bar: Loader[A] = withStyle(LoaderStyle.Bar)

    /** Retro ASCII with an arrow head `[====>    ]`. */
    def ascii: Loader[A] = withStyle(LoaderStyle.Ascii)

    /** Dotted `●●●●∙∙∙∙`. */
    def dots: Loader[A] = withStyle(LoaderStyle.Dots)

    /** Heavy/light rule `━━━━────`. */
    def line: Loader[A] = withStyle(LoaderStyle.Line)

    /** Segmented pipes `▰▰▰▱▱▱`. */
    def pipes: Loader[A] = withStyle(LoaderStyle.Pipes)

    /** Custom style; unspecified fields keep the current style's values. */
    def styled(
        fill: Char = style.fill,
        empty: Char = style.empty,
        open: String = style.open,
        close: String = style.close,
        head: String = style.head,
        smooth: Boolean = style.smooth,
        spinner: SpinnerStyle = style.spinner,
        color: Color = style.color
    ): Loader[A] =
      withStyle(LoaderStyle(fill, empty, open, close, head, smooth, spinner, color))
  }

  /* Fractional block glyphs for the smooth-fill boundary cell (1/8 .. 8/8). */
  private val BlockEighths = Array("", "▏", "▎", "▍", "▌", "▋", "▊", "▉")
  private def paint(c: Color, s: String): String = if (c == Color.NoColor) s else c(s).render
  private def dimStr(s: String): String = Text(s).style(Style.Dim).render

  private[layoutz] def renderBar(style: LoaderStyle, progress: Double, width: Int): String = {
    val p = math.max(0.0, math.min(1.0, progress))
    val exact = p * width
    val filled = exact.toInt
    if (style.smooth) {
      val eighths = ((exact - filled) * 8).toInt
      val hasPartial = filled < width && eighths > 0
      val partial = if (hasPartial) BlockEighths(eighths) else ""
      val used = filled + (if (hasPartial) 1 else 0)
      val body = paint(style.color, style.fill.toString * filled + partial)
      style.open + body + dimStr(style.empty.toString * (width - used)) + style.close
    } else {
      val showHead = style.head.nonEmpty && filled > 0 && filled < width
      val head = if (showHead) style.head else ""
      val emptyCount = math.max(0, width - filled - head.length)
      val body = paint(style.color, style.fill.toString * filled + head)
      style.open + body + dimStr(style.empty.toString * emptyCount) + style.close
    }
  }

  /* Restore the cursor if the JVM dies mid-iteration before the
   * LayoutzApp had a chance to run its own cleanup.
   */
  java.lang.Runtime
    .getRuntime
    .addShutdownHook(new Thread(
      () => {
        System.out.print(Ansi.CSI + "?25h")
        System.out.flush()
      },
      "layoutz-loader-cleanup"
    ))

  sealed private trait LoaderMsg
  private case object LoaderTick extends LoaderMsg

  final private class BoundedLoaderApp(
      label: String,
      total: Int,
      style: LoaderStyle,
      doneRef: AtomicInteger,
      finishedRef: AtomicBoolean
  ) extends LayoutzApp[Unit, LoaderMsg] {
    def init: (Unit, Cmd[LoaderMsg]) = ((), Cmd.none)

    def update(msg: LoaderMsg, s: Unit): (Unit, Cmd[LoaderMsg]) =
      if (finishedRef.get()) ((), Cmd.exit) else ((), Cmd.none)

    def subscriptions(s: Unit): Sub[LoaderMsg] =
      Sub.time.everyMs(60, LoaderTick)

    def view(s: Unit): Element = Text(renderLine())

    def renderLine(): String = {
      val done = doneRef.get()
      val progress = if (total > 0) math.min(1.0, done.toDouble / total) else 1.0
      val bar = renderBar(style, progress, 20)
      val pct = (progress * 100).toInt
      val prefix = if (label.isEmpty) "" else s"$label "
      s"$prefix$bar $done/$total ($pct%)"
    }

    def finalLine(): String = renderLine()
  }

  final private class StreamingLoaderApp(
      label: String,
      style: LoaderStyle,
      doneRef: AtomicInteger,
      finishedRef: AtomicBoolean
  ) extends LayoutzApp[Int, LoaderMsg] {
    private val frames = style.spinner.frames
    private val frameRef = new AtomicInteger(0)

    def init: (Int, Cmd[LoaderMsg]) = (0, Cmd.none)

    def update(msg: LoaderMsg, frame: Int): (Int, Cmd[LoaderMsg]) = {
      val next = frame + 1
      frameRef.set(next)
      if (finishedRef.get()) (frame, Cmd.exit) else (next, Cmd.none)
    }

    def subscriptions(frame: Int): Sub[LoaderMsg] =
      Sub.time.everyMs(80, LoaderTick)

    def view(frame: Int): Element = Text(renderLine())

    def renderLine(): String = {
      val spin = paint(style.color, frames(java.lang.Math.floorMod(frameRef.get(), frames.length)))
      val prefix = if (label.isEmpty) "" else s"$label "
      s"$spin $prefix(${doneRef.get()} processed)"
    }

    def finalLine(): String = prefixLabel + s"${doneRef.get()} processed"
    private def prefixLabel: String = if (label.isEmpty) "" else s"$label "
  }

  private def spawn[S, M](app: LayoutzApp[S, M]): Thread = {
    val t = new Thread(
      () =>
        app.run(
          tickIntervalMs = 60,
          renderIntervalMs = 60,
          showQuitMessage = false,
          clearOnStart = false,
          clearOnExit = false
        ),
      "layoutz-loader"
    )
    t.setDaemon(true)
    t.start()
    t
  }

  abstract private class BaseLoaderIter[A](under: Iterator[A]) extends Iterator[A] {
    protected val doneRef = new AtomicInteger(0)
    protected val finishedRef = new AtomicBoolean(false)
    protected def appThread: Thread
    protected def renderFinalLine(): String
    private var closed = false

    private def shutdown(): Unit = if (!closed) {
      closed = true
      finishedRef.set(true)
      try appThread.join(500)
      catch { case _: InterruptedException => () }
      /* The runtime's render loop may have exited before drawing the final
       * post-completion frame, so we draw it ourselves.
       */
      val out = System.out
      out.print(Ansi.cursorUp(1) + renderFinalLine() + Ansi.CLEAR_TO_EOL + "\n")
      out.flush()
    }

    def hasNext: Boolean = {
      val h = under.hasNext
      if (!h) shutdown()
      h
    }

    def next(): A = {
      val v = under.next()
      doneRef.incrementAndGet()
      v
    }
  }

  final private class BoundedIter[A](label: String, total: Int, under: Iterator[A], style: LoaderStyle)
      extends BaseLoaderIter[A](under) {
    private val app = new BoundedLoaderApp(label, total, style, doneRef, finishedRef)
    protected val appThread: Thread = spawn(app)
    protected def renderFinalLine(): String = app.finalLine()
  }

  final private class StreamingIter[A](label: String, under: Iterator[A], style: LoaderStyle)
      extends BaseLoaderIter[A](under) {
    private val app = new StreamingLoaderApp(label, style, doneRef, finishedRef)
    protected val appThread: Thread = spawn(app)
    protected def renderFinalLine(): String = app.finalLine()
  }
}

/** One-shot interactive prompts. Each takes over the tty briefly, returns a value, and leaves a
  * single committed line behind..
  */
object Ask {

  private val CLEAR_BELOW = Ansi.CSI + "J"

  private def withTty[A](body: SttyTerminal => A): Option[A] =
    SttyTerminal.create().toOption.map { t =>
      t.enterRawMode()
      t.hideCursor()
      try body(t)
      finally {
        t.showCursor()
        t.exitRawMode()
        t.flush()
      }
    }

  private def repaint(t: SttyTerminal, frame: String, prevLines: Int): Int = {
    val lines = frame.split("\n", -1)
    val sb = new StringBuilder
    if (prevLines > 0) sb.append(Ansi.cursorUp(prevLines))
    lines.foreach(l => sb.append(l).append(Ansi.CLEAR_TO_EOL).append("\n"))
    if (prevLines > lines.length) sb.append(CLEAR_BELOW)
    t.write(sb.toString); t.flush()
    lines.length
  }

  private def commit(t: SttyTerminal, summary: String, prevLines: Int): Unit = {
    val sb = new StringBuilder
    if (prevLines > 0) sb.append(Ansi.cursorUp(prevLines))
    sb.append(summary).append(Ansi.CLEAR_TO_EOL).append("\n")
    if (prevLines > 1) sb.append(CLEAR_BELOW)
    t.write(sb.toString); t.flush()
  }

  private def erase(t: SttyTerminal, prevLines: Int): Unit = if (prevLines > 0) {
    t.write(Ansi.cursorUp(prevLines) + CLEAR_BELOW)
    t.flush()
  }

  private def colored(c: Color, s: String): String = c(s).render
  private def dim(s: String): String = Text(s).style(Style.Dim).render

  private[layoutz] def renderInputFrame(
      prompt: String,
      value: String,
      placeholder: String
  ): String = {
    val body =
      if (value.isEmpty && placeholder.nonEmpty) dim(placeholder)
      else value
    prompt + body + colored(Color.Cyan, "▌")
  }

  private[layoutz] def renderConfirmFrame(
      question: String,
      yes: Boolean,
      affirmative: String,
      negative: String
  ): String = {
    def btn(text: String, active: Boolean, bg: Color): String = {
      val padded = s"  $text  "
      if (active) padded.color(Color.White).colorBg(bg).style(Style.Bold).render
      else padded
    }
    question + "\n  " +
      btn(affirmative, yes, Color.Green) + "  " +
      btn(negative, !yes, Color.Red)
  }

  private[layoutz] def renderChooseFrame[A](
      prompt: String,
      items: Seq[A],
      idx: Int,
      render: A => String
  ): String = {
    val rows = items.zipWithIndex.map { case (item, i) =>
      val label = render(item)
      if (i == idx) colored(Color.Cyan, "› " + label) else "  " + label
    }
    (if (prompt.nonEmpty) prompt + "\n" else "") + rows.mkString("\n")
  }

  /** Prompt for a single line of text. */
  def input(
      prompt: String = "› ",
      placeholder: String = "",
      initial: String = ""
  ): Option[String] = withTty { t =>
    @scala.annotation.tailrec
    def loop(value: String, prevLines: Int): Option[String] =
      KeyParser.parse(t.readInput(), t) match {
        case Key.Enter =>
          commit(t, prompt + value, prevLines); Some(value)
        case Key.Escape | Key.Ctrl('C') | Key.Ctrl('D') =>
          erase(t, prevLines); None
        case Key.Backspace if value.nonEmpty =>
          val nv = value.dropRight(1)
          loop(nv, repaint(t, renderInputFrame(prompt, nv, placeholder), prevLines))
        case Key.Char(c) =>
          val nv = value + c
          loop(nv, repaint(t, renderInputFrame(prompt, nv, placeholder), prevLines))
        case _ =>
          loop(value, prevLines)
      }
    val initLines = repaint(t, renderInputFrame(prompt, initial, placeholder), 0)
    loop(initial, initLines)
  }.flatten

  def confirm(
      question: String,
      default: Boolean = true,
      affirmative: String = "Yes",
      negative: String = "No"
  ): Boolean = withTty { t =>
    def picked(yes: Boolean, prevLines: Int): Boolean = {
      val label = if (yes) affirmative else negative
      commit(t, question + " " + colored(Color.Cyan, label), prevLines)
      yes
    }
    @scala.annotation.tailrec
    def loop(yes: Boolean, prevLines: Int): Boolean =
      KeyParser.parse(t.readInput(), t) match {
        case Key.Enter                     => picked(yes, prevLines)
        case Key.Char('y') | Key.Char('Y') => picked(true, prevLines)
        case Key.Char('n') | Key.Char('N') => picked(false, prevLines)
        case Key.Left | Key.Right | Key.Tab =>
          val ny = !yes
          loop(ny, repaint(t, renderConfirmFrame(question, ny, affirmative, negative), prevLines))
        case Key.Escape | Key.Ctrl('C') | Key.Ctrl('D') =>
          erase(t, prevLines); !default
        case _ => loop(yes, prevLines)
      }
    val initLines = repaint(t, renderConfirmFrame(question, default, affirmative, negative), 0)
    loop(default, initLines)
  }.getOrElse(default)

  def choose[A](prompt: String, items: Seq[A]): Option[A] =
    choose(prompt, items, (a: A) => a.toString)

  def choose[A](prompt: String, items: Seq[A], render: A => String): Option[A] =
    if (items.isEmpty) None
    else
      withTty { t =>
        @scala.annotation.tailrec
        def loop(idx: Int, prevLines: Int): Option[A] =
          KeyParser.parse(t.readInput(), t) match {
            case Key.Enter =>
              val pick = items(idx)
              commit(t, prompt + " " + colored(Color.Cyan, render(pick)), prevLines)
              Some(pick)
            case Key.Up =>
              val ni = (idx - 1 + items.length) % items.length
              loop(ni, repaint(t, renderChooseFrame(prompt, items, ni, render), prevLines))
            case Key.Down | Key.Tab =>
              val ni = (idx + 1) % items.length
              loop(ni, repaint(t, renderChooseFrame(prompt, items, ni, render), prevLines))
            case Key.Escape | Key.Ctrl('C') | Key.Ctrl('D') =>
              erase(t, prevLines); None
            case _ => loop(idx, prevLines)
          }
        val initLines = repaint(t, renderChooseFrame(prompt, items, 0, render), 0)
        loop(0, initLines)
      }.flatten

  def chooseMany(prompt: String, items: Seq[String], limit: Int = 0): Option[Seq[String]] =
    chooseMany[String](prompt, items, limit, (s: String) => s)

  def chooseMany[A](
      prompt: String,
      items: Seq[A],
      limit: Int,
      render: A => String
  ): Option[Seq[A]] =
    if (items.isEmpty) None
    else
      withTty { t =>
        @scala.annotation.tailrec
        def loop(idx: Int, selected: Set[Int], prevLines: Int): Option[Seq[A]] = {
          val n = repaint(
            t,
            renderChooseManyFrame(prompt, items, idx, selected, limit, render),
            prevLines
          )
          KeyParser.parse(t.readInput(), t) match {
            case Key.Enter =>
              val picks =
                items.zipWithIndex.collect { case (item, i) if selected.contains(i) => item }
              val summary =
                if (picks.isEmpty) prompt + " " + dim("(nothing)")
                else prompt + " " + colored(Color.Cyan, picks.map(render).mkString(", "))
              commit(t, summary, n); Some(picks)
            case Key.Up =>
              loop((idx - 1 + items.length) % items.length, selected, n)
            case Key.Down =>
              loop((idx + 1) % items.length, selected, n)
            case Key.Char(' ') | Key.Tab =>
              val toggled =
                if (selected.contains(idx)) selected - idx
                else if (limit > 0 && selected.size >= limit) selected
                else selected + idx
              loop(idx, toggled, n)
            case Key.Escape | Key.Ctrl('C') | Key.Ctrl('D') =>
              erase(t, n); None
            case _ => loop(idx, selected, n)
          }
        }
        loop(0, Set.empty, 0)
      }.flatten

  private[layoutz] def renderChooseManyFrame[A](
      prompt: String,
      items: Seq[A],
      idx: Int,
      selected: Set[Int],
      limit: Int,
      render: A => String
  ): String = {
    val rows = items.zipWithIndex.map { case (item, i) =>
      val mark = if (selected.contains(i)) colored(Color.Green, "[x]") else dim("[ ]")
      val label = render(item)
      val row = mark + " " + label
      if (i == idx) colored(Color.Cyan, "› ") + row else "  " + row
    }
    val header =
      if (prompt.isEmpty) ""
      else {
        val tag =
          if (limit > 0) s" (${selected.size}/$limit)"
          else if (selected.nonEmpty) s" (${selected.size})"
          else ""
        prompt + dim(tag) + "\n"
      }
    header + rows.mkString("\n")
  }

  def spin[A](label: String, style: SpinnerStyle = SpinnerStyle.Dots)(task: => A): A = {
    import java.util.concurrent.atomic.AtomicBoolean
    val done = new AtomicBoolean(false)
    val out = System.out
    val frames = style.frames
    out.print(Ansi.CSI + "?25l"); out.flush()
    val ticker = new Thread(
      () => {
        var i = 0
        while (!done.get()) {
          val frame = colored(Color.Cyan, frames(i % frames.length)) + " " + label
          out.print("\r" + frame + Ansi.CLEAR_TO_EOL); out.flush()
          i += 1
          try Thread.sleep(80)
          catch { case _: InterruptedException => () }
        }
      },
      "ask-spin"
    )
    ticker.setDaemon(true)
    ticker.start()

    val (result, ok, err) =
      try { (task, true, null: Throwable) }
      catch { case t: Throwable => (null.asInstanceOf[A], false, t) }

    done.set(true)
    try ticker.join(500)
    catch { case _: InterruptedException => () }
    val glyph = if (ok) "✓" else "✗"
    val finished = colored(if (ok) Color.Green else Color.Red, glyph + " " + label)
    out.print("\r" + finished + Ansi.CLEAR_TO_EOL + "\n")
    out.print(Ansi.CSI + "?25h"); out.flush()
    if (err != null) throw err else result
  }

  def write(
      prompt: String = "",
      placeholder: String = "Write something…",
      initial: String = "",
      hint: String = "(Enter for newline, Ctrl+D to submit, Esc to cancel)"
  ): Option[String] = withTty { t =>
    @scala.annotation.tailrec
    def loop(value: String, prevLines: Int): Option[String] = {
      val n = repaint(t, renderWriteFrame(prompt, value, placeholder, hint), prevLines)
      KeyParser.parse(t.readInput(), t) match {
        case Key.Ctrl('D') =>
          val firstLine = value.linesIterator.toSeq.headOption.getOrElse("")
          val summary = if (value.contains('\n')) firstLine + " …" else firstLine
          commit(t, (if (prompt.nonEmpty) prompt + " " else "") + colored(Color.Cyan, summary), n)
          Some(value)
        case Key.Escape | Key.Ctrl('C') =>
          erase(t, n); None
        case Key.Enter                       => loop(value + "\n", n)
        case Key.Char(c)                     => loop(value + c, n)
        case Key.Backspace if value.nonEmpty => loop(value.dropRight(1), n)
        case _                               => loop(value, n)
      }
    }
    loop(initial, 0)
  }.flatten

  private[layoutz] def renderWriteFrame(
      prompt: String,
      value: String,
      placeholder: String,
      hint: String
  ): String = {
    val header = if (prompt.nonEmpty) prompt + "\n" else ""
    val body =
      if (value.isEmpty && placeholder.nonEmpty) dim(placeholder) + colored(Color.Cyan, "▌")
      else value + colored(Color.Cyan, "▌")
    val footer = if (hint.nonEmpty) "\n" + dim(hint) else ""
    header + body + footer
  }

  def filter(prompt: String, items: Seq[String]): Option[String] =
    filter(prompt, items, 10, (s: String) => s)

  def filter[A](
      prompt: String,
      items: Seq[A],
      height: Int,
      render: A => String
  ): Option[A] =
    if (items.isEmpty) None
    else
      withTty { t =>
        @scala.annotation.tailrec
        def loop(query: String, idx: Int, prevLines: Int): Option[A] = {
          val matches = fuzzyMatches(query, items, render)
          val safeIdx =
            if (matches.isEmpty) 0
            else math.max(0, math.min(idx, matches.length - 1))
          val n = repaint(
            t,
            renderFilterFrame(prompt, query, matches, safeIdx, height, render),
            prevLines
          )
          KeyParser.parse(t.readInput(), t) match {
            case Key.Enter if matches.nonEmpty =>
              val pick = matches(safeIdx)
              commit(t, prompt + query + "  " + colored(Color.Cyan, render(pick)), n)
              Some(pick)
            case Key.Up if matches.nonEmpty =>
              loop(query, (safeIdx - 1 + matches.length) % matches.length, n)
            case Key.Down | Key.Tab if matches.nonEmpty =>
              loop(query, (safeIdx + 1) % matches.length, n)
            case Key.Backspace if query.nonEmpty =>
              loop(query.dropRight(1), 0, n)
            case Key.Char(c) =>
              loop(query + c, 0, n)
            case Key.Escape | Key.Ctrl('C') | Key.Ctrl('D') =>
              erase(t, n); None
            case _ => loop(query, safeIdx, n)
          }
        }
        loop("", 0, 0)
      }.flatten

  private[layoutz] def fuzzyScore(query: String, target: String): Option[Int] = {
    if (query.isEmpty) return Some(0)
    val q = query.toLowerCase
    val s = target.toLowerCase
    var qi, ti, lastMatch, gap = 0
    lastMatch = -1
    while (qi < q.length && ti < s.length) {
      if (q.charAt(qi) == s.charAt(ti)) {
        if (lastMatch >= 0) gap += ti - lastMatch - 1
        lastMatch = ti
        qi += 1
      }
      ti += 1
    }
    if (qi == q.length) Some(gap) else None
  }

  private[layoutz] def fuzzyMatches[A](query: String, items: Seq[A], render: A => String): Seq[A] =
    if (query.isEmpty) items
    else
      items.iterator
        .flatMap(item => fuzzyScore(query, render(item)).map(score => (item, score)))
        .toSeq
        .sortBy(_._2)
        .map(_._1)

  private[layoutz] def renderFilterFrame[A](
      prompt: String,
      query: String,
      matches: Seq[A],
      idx: Int,
      height: Int,
      render: A => String
  ): String = {
    val total = matches.length
    val h = math.max(0, math.min(height, total))
    val top =
      if (total <= h) 0
      else math.max(0, math.min(idx - h / 2, total - h))
    val visible = matches.slice(top, top + h)
    val rows = visible.zipWithIndex.map { case (item, i) =>
      val absoluteIdx = top + i
      val label = render(item)
      if (absoluteIdx == idx) colored(Color.Cyan, "› " + label) else "  " + label
    }
    val header = prompt + query + colored(Color.Cyan, "▌")
    val list = if (rows.isEmpty) dim("  (no matches)") else rows.mkString("\n")
    header + "\n" + list
  }

  final private[layoutz] case class FileEntry(name: String, isDir: Boolean) {
    def display: String = if (isDir) name + "/" else name
  }

  private def listEntries(path: String): Either[String, Seq[FileEntry]] = {
    val f = new java.io.File(path)
    if (!f.exists()) Left(s"not found: $path")
    else if (!f.isDirectory) Left(s"not a directory: $path")
    else
      Option(f.listFiles())
        .toRight(s"cannot read: $path")
        .map { arr =>
          val sorted = arr.toSeq
            .map(c => FileEntry(c.getName, c.isDirectory))
            .sortBy(e => (!e.isDir, e.name.toLowerCase))
          FileEntry("..", isDir = true) +: sorted
        }
  }

  def file(start: String = ".", height: Int = 12): Option[String] = withTty { t =>
    @scala.annotation.tailrec
    def loop(dir: String, idx: Int, prevLines: Int): Option[String] = {
      val canonical = new java.io.File(dir).getCanonicalPath
      listEntries(canonical) match {
        case Left(err) =>
          commit(t, dim(err), prevLines); None
        case Right(entries) =>
          val safeIdx = math.max(0, math.min(idx, entries.length - 1))
          val n = repaint(t, renderFileFrame(canonical, entries, safeIdx, height), prevLines)
          KeyParser.parse(t.readInput(), t) match {
            case Key.Enter =>
              val target = entries(safeIdx)
              val full = new java.io.File(canonical, target.name).getCanonicalPath
              if (target.isDir) loop(full, 0, n)
              else { commit(t, colored(Color.Cyan, full), n); Some(full) }
            case Key.Right =>
              val target = entries(safeIdx)
              if (target.isDir)
                loop(new java.io.File(canonical, target.name).getCanonicalPath, 0, n)
              else loop(canonical, safeIdx, n)
            case Key.Up =>
              loop(canonical, (safeIdx - 1 + entries.length) % entries.length, n)
            case Key.Down | Key.Tab =>
              loop(canonical, (safeIdx + 1) % entries.length, n)
            case Key.Left | Key.Backspace =>
              val parent = new java.io.File(canonical).getParent
              if (parent != null) loop(parent, 0, n) else loop(canonical, safeIdx, n)
            case Key.Escape | Key.Ctrl('C') | Key.Ctrl('D') =>
              erase(t, n); None
            case _ => loop(canonical, safeIdx, n)
          }
      }
    }
    loop(start, 0, 0)
  }.flatten

  private[layoutz] def renderFileFrame(
      path: String,
      entries: Seq[FileEntry],
      idx: Int,
      height: Int
  ): String = {
    val total = entries.length
    val h = math.max(0, math.min(height, total))
    val top =
      if (total <= h) 0
      else math.max(0, math.min(idx - h / 2, total - h))
    val visible = entries.slice(top, top + h)
    val rows = visible.zipWithIndex.map { case (e, i) =>
      val absoluteIdx = top + i
      val name = if (e.isDir) colored(Color.Blue, e.display) else e.display
      if (absoluteIdx == idx) colored(Color.Cyan, "› ") + name else "  " + name
    }
    val header = dim(path)
    header + "\n" + rows.mkString("\n")
  }

  def pager(content: String, height: Int = 0, lineNumbers: Boolean = true): Unit = {
    val lines = if (content.isEmpty) Array("") else content.split('\n')
    withTty { t =>
      val termH = terminalHeight()
      val termW = t.terminalWidth()
      val h = if (height > 0) height else math.max(5, termH - 3)
      @scala.annotation.tailrec
      def loop(top: Int, prevLines: Int): Unit = {
        val maxTop = math.max(0, lines.length - h)
        val safeTop = math.max(0, math.min(top, maxTop))
        val n = repaint(t, renderPagerFrame(lines, safeTop, h, termW, lineNumbers), prevLines)
        KeyParser.parse(t.readInput(), t) match {
          case Key.Char('q') | Key.Escape | Key.Ctrl('C') => erase(t, n)
          case Key.Up                                     => loop(safeTop - 1, n)
          case Key.Down | Key.Enter                       => loop(safeTop + 1, n)
          case Key.PageUp                                 => loop(safeTop - h, n)
          case Key.PageDown | Key.Char(' ')               => loop(safeTop + h, n)
          case Key.Home                                   => loop(0, n)
          case Key.End                                    => loop(maxTop, n)
          case _                                          => loop(safeTop, n)
        }
      }
      loop(0, 0)
    }
    ()
  }

  private[layoutz] def renderPagerFrame(
      lines: Array[String],
      top: Int,
      h: Int,
      width: Int,
      lineNumbers: Boolean
  ): String = {
    val total = lines.length
    val visible = lines.slice(top, top + h)
    val numWidth = total.toString.length
    val gutter = if (lineNumbers) numWidth + 3 else 0
    val maxLineW = math.max(20, width - gutter)
    val body = visible.zipWithIndex.map { case (line, i) =>
      val truncated = if (line.length > maxLineW) line.take(maxLineW - 1) + "…" else line
      if (lineNumbers) {
        val prefix = dim(("%" + numWidth + "d │ ").format(top + i + 1))
        prefix + truncated
      } else truncated
    }.mkString("\n")
    val viewEnd = math.min(top + h, total)
    val percent = if (total == 0) 100 else (viewEnd * 100) / total
    val status =
      dim(s"  L${top + 1}–$viewEnd of $total  $percent%   ↑↓ PgUp/PgDn Home/End  q to quit")
    body + "\n" + status
  }

  private def terminalHeight(): Int = {
    import scala.sys.process._
    try {
      val out = Seq("sh", "-c", "stty size < /dev/tty").!!.trim
      out.split("\\s+") match {
        case Array(rows, _) => scala.util.Try(rows.toInt).getOrElse(24)
        case _              => 24
      }
    } catch { case _: Exception => 24 }
  }
}
