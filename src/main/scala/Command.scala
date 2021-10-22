import scala.sys.process._
import scala.collection.mutable.ListBuffer

trait CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult

object ExitCommand extends CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult =
    return ExecutionResult(
      true,
      0,
      ReadableData.MachineReadable(JSON.getEmptyArray())
    )

end ExitCommand

object ExternalCommand extends CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult =
    try
      val stdout = ListBuffer[String]()
      val stderr = ListBuffer[String]()
      val logger = ProcessLogger(
        stdout += _,
        stderr += _
      )
      val exitCode = (command :: arguments) ! logger

      return ExecutionResult(
        false,
        exitCode,
        ReadableData.MachineReadable(
          JSON.Object(Map(
            "stdout" -> JSONNode(JSON.Array(stdout.map(JSONNode(_)).toList)),
            "stderr" -> JSONNode(JSON.Array(stderr.map(JSONNode(_)).toList))
          ))
        )
      )
    catch
      case e: Exception => ExecutionResult(false, 1, e)

end ExternalCommand
