case class ExecutionResult(
  exit: Boolean,
  exitCode: Int,
  error: Option[Exception]
)

case class Executor(
  nodeList: List[Node]
):

  def execute(): ExecutionResult =
    try
      val command = this.nodeList.head.value
      val arguments = this.nodeList.drop(1).map(_.value)

      val result = command match
        case "exit" => ExitCommand.execute(command, arguments)
        case _ => ExternalCommand.execute(command, arguments)

      return result
    catch
      case e: Exception => ExecutionResult(false, 1, Some(e))
