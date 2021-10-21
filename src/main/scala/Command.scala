import scala.sys.process._
import scala.collection.mutable.ListBuffer

trait CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult

object ExitCommand extends CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult =
    return ExecutionResult(
      true,
      0,
      ReadableData.MachineReadable(MachineReadableObject(None))
    )

end ExitCommand

object ExternalCommand extends CommandInterface:

  def execute(command: String, arguments: List[String]): ExecutionResult =
    try
      val stdout = ListBuffer[MachineReadableObject]()
      val stderr = ListBuffer[MachineReadableObject]()
      val logger = ProcessLogger(
        stdout += MachineReadableObject(_),
        stderr += MachineReadableObject(_)
      )
      val exitCode = (command :: arguments) ! logger

      return ExecutionResult(
        false,
        exitCode,
        ReadableData.MachineReadable(
          MachineReadableObject(
            Map(
              "stdout" -> MachineReadableObject(stdout.toList),
              "stderr" -> MachineReadableObject(stderr.toList)
            )
          )
        )
      )
    catch
      case e: Exception => ExecutionResult(false, 1, e)

end ExternalCommand
