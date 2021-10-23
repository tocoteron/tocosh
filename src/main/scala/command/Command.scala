import java.io._
import scala.sys.process._

type CommandArg = String
type CommandArgs = Seq[CommandArg]
type CommandFunc = (args: CommandArgs, io: IO) => CommandResult

case class CommandResult(
  exit: Boolean,
  exitCode: Int,
)

case class Command(
  func: CommandFunc,
  args: CommandArgs,
)

object Command:
  def exec(command: Command): CommandResult =
    Command.exec(List(command))

  def exec(commands: List[Command]): CommandResult =
    // Build and exec commands
    val (plans, io) = CommandExecPlan.build(commands)
    val res = CommandExecPlan.exec(plans)

    // Handle last IO for shell
    val reader = io.stdinReader
    val writer = io.stdoutWriter
    var line = reader.readLine
    while line != null do
      writer.write(line)
      writer.newLine
      writer.flush
      line = reader.readLine

    res

  def registry = Map[String, CommandFunc](
    "exit" -> exitCommand,
    "hello" -> helloCommand,
  )

//////////////
// Commands //
//////////////

def externalCommand(args: CommandArgs, io: IO): CommandResult =
  val streamHandler = (is: InputStream, os: OutputStream) =>
    try
      val reader = BufferedReader(InputStreamReader(is))
      val writer = BufferedWriter(OutputStreamWriter(os))
      var line = reader.readLine

      while line != null do
        writer.write(line)
        writer.newLine
        writer.flush
        line = reader.readLine
    catch
      case e: java.lang.InterruptedException => ()

  val processIO = ProcessIO(
    streamHandler(io.stdin, _),
    streamHandler(_, io.stdout),
    streamHandler(_, io.stderr),
  )

  val process = Process(args).run(processIO)

  CommandResult(false, process.exitValue)

def exitCommand(args: CommandArgs, io: IO): CommandResult =
  CommandResult(true, 0)

def helloCommand(args: CommandArgs, io: IO): CommandResult =
  val reader = io.stdinReader
  val writer = io.stdoutWriter
  var line = reader.readLine

  writer.write(s"Hello ${line}")
  writer.newLine
  writer.flush

  CommandResult(false, 0)
