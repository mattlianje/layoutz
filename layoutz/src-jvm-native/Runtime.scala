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

  def terminalWidth(): Int = {
    val size = stty("size").trim
    size.split("\\s+") match {
      case Array(_, cols) => Try(cols.toInt).getOrElse(80)
      case _              => 80
    }
  }

  private def esc(codes: String*): Unit = {
    codes.foreach(c => System.out.print(s"\u001b[$c"))
    System.out.flush()
  }

  private def stty(args: String): String = {
    import scala.sys.process._
    try Seq("sh", "-c", s"stty $args < /dev/tty").!!
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
      terminal: Option[Terminal] = None
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
    LayoutzRuntime.run(this, config, terminal)
  }
}

private[layoutz] object LayoutzRuntime {

  def run[S, M](
      app: LayoutzApp[S, M],
      config: RuntimeConfig,
      terminal: Option[Terminal] = None
  ): Either[RuntimeError, Unit] =
    terminal.orElse(SttyTerminal.create().toOption) match {
      case Some(t) =>
        Right(
          try new RuntimeInstance(app, config, t).run()
          finally t.close()
        )
      case None => Left(TerminalError("Failed to initialize terminal"))
    }

  private class RuntimeInstance[State, Message](
      app: LayoutzApp[State, Message],
      config: RuntimeConfig,
      terminal: Terminal
  ) {
    @volatile private var currentState: State = _
    @volatile private var shouldContinue = true
    private val stateLock = new Object()

    def run(): Unit = {
      initialize()
      val renderThread = new Thread(() => runRenderLoop(), "LayoutzRender")
      val tickThread = new Thread(() => runTickLoop(), "LayoutzTick")
      val inputThread = new Thread(() => runInputLoop(), "LayoutzInput")
      renderThread.setDaemon(true)
      tickThread.setDaemon(true)
      inputThread.setDaemon(true)
      renderThread.start(); tickThread.start(); inputThread.start()

      try while (shouldContinue) Thread.sleep(50)
      catch { case _: InterruptedException => () }
      finally cleanup()
    }

    private def initialize(): Unit = {
      terminal.enterRawMode()
      if (config.clearOnStart) terminal.clearScreen()
      terminal.hideCursor()
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
      implicit val ec: ExecutionContext = ExecutionContext.global

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
        case CmdSetTitle(t) => terminal.write("\u001b]2;" + t + "\u0007"); terminal.flush()
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

    /* Runtime to handles last seens with mutable blocks */
    private val lastTickTimes = scala.collection.mutable.Map[Long, Long]()

    private val lastModifiedTimes =
      scala.collection.mutable.Map[String, Long]()

    private val lastPollTimes = scala.collection.mutable.Map[String, Long]()

    private def runRenderLoop(): Unit = {
      var lastRenderedState: Option[String] = None
      var lastLineCount: Int = 0

      while (shouldContinue)
        try {
          val currentRender = app.view(readState()).render
          if (lastRenderedState.forall(_ != currentRender)) {
            val fullRender = if (config.showQuitMessage) {
              currentRender + "\n\n" + config.quitMessage
            } else {
              currentRender
            }
            val renderedLines = fullRender.split("\n", -1)
            val currentLineCount = renderedLines.length

            /* Move cursor to start of our render area */
            if (config.clearOnStart) {
              /* Absolute positioning when screen was cleared */
              terminal.write("\u001b[H")
            } else if (lastLineCount > 0) {
              /* Relative positioning: move up by lines we previously rendered */
              terminal.write(s"\u001b[${lastLineCount}A\r")
            }

            /* Apply alignment to layout as a block (uniform margin for all lines) */
            val termWidth = terminal.terminalWidth()
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
              terminal.write(line + "\u001b[K\n")
            }
            /* Clear any extra lines from the previous render */
            val extraLines = lastLineCount - currentLineCount
            if (extraLines > 0) {
              (0 until extraLines).foreach { _ =>
                terminal.write("\u001b[K\n")
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
                }(scala.concurrent.ExecutionContext.global)
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
          val key = KeyParser.parse(input, terminal)
          if (key == config.quitKey) {
            shouldContinue = false
          } else {
            /* Uses the current subscription's key handler */
            getKeyPressHandler().foreach { handler =>
              handler(key).foreach(updateState)
            }
          }
        } catch {
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
