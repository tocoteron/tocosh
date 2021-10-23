import java.io._
import scala.collection.mutable.ListBuffer

case class CommandExecPlan(command: Command, io: IO)

object CommandExecPlan:
  def build(commands: List[Command]): (List[CommandExecPlan], IO) =
    // Build head plan
    val headStdoutOut = PipedOutputStream()
    val headStdoutIn = PipedInputStream(headStdoutOut)
    val headPlan = CommandExecPlan(
      commands.head, // Can be empty
      ShellIO(System.in, headStdoutOut, PipedOutputStream())
    )

    // Build tail IO
    val tailCommands = commands.tail
    val tailIO = ListBuffer[PipedShellIO]()
    tailCommands.foreach(_ =>
      val io = tailIO.lastOption match
        case None => PipedShellIO(
          headStdoutIn,
          PipedOutputStream(),
          PipedOutputStream(),
        )
        case Some(io) => PipedShellIO(
          PipedInputStream(io.stdout),
          PipedOutputStream(),
          PipedOutputStream()
        )

      tailIO += io
    )

    // Build last IO for shell
    val lastIO = ShellIO(
      tailIO.lastOption match
        case None => headStdoutIn
        case Some(io) => PipedInputStream(io.stdout),
      System.out,
      System.err,
    )

    // Build tail plans
    val tailPlans = (tailCommands zip tailIO)
      .map((command, io) =>
        CommandExecPlan(
          command,
          ShellIO(io.stdin, io.stdout, io.stderr)
        )
      )

    // Built
    (headPlan :: tailPlans, lastIO)

  def exec(plans: List[CommandExecPlan]): CommandResult =
    val results = plans.map(plan => {
      val res = plan.command.func(plan.command.args, plan.io)
      plan.io.stdout.close()
      res
    })

    results.lastOption match
      case None => CommandResult(false, 0)
      case Some(res) => res
