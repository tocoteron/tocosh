import scala.sys.process._

trait CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult

object ExitCommand extends CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult =
    return ExecutionResult(true, 0, None)

end ExitCommand

object ExternalCommand extends CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult =
    try
      val exitCode = (command :: arguments).!
      return ExecutionResult(false, exitCode, None)
    catch
      case e: Exception => ExecutionResult(false, 1, Some(e))

end ExternalCommand
