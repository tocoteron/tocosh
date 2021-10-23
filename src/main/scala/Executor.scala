case class Executor(
  nodeList: List[Node]
):

  def execute(): CommandResult = this.nodeList match
    case Nil => CommandResult(false, 0)
    case head :: tail =>
      val commandName = head.value
      val arguments = tail.map(_.value)

      val result = Command.registry.get(commandName) match
        case None => CommandResult(false, 0)
        case Some(commandFunc) =>
          Command.exec(Command(commandFunc, arguments))

      result
