trait CommandInterface:

  def execute(arguments: List[String]): ExecutionResult

object ExitCommand extends CommandInterface:

  def execute(arguments: List[String]): ExecutionResult =
    return ExecutionResult(true, 0)

end ExitCommand
