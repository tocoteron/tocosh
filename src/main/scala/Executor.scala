case class ExecutionResult(
  exit: Boolean,
  exitCode: Int
)

case class Executor(
  nodeList: List[Node]
):

  def execute(): ExecutionResult =
    val command = this.nodeList.head
    val arguments = this.nodeList.drop(1).map(_.value)

    val result = command.value match
      case "exit" => ExitCommand.execute(arguments)
      case _ => ExecutionResult(false, 0)

    return result
