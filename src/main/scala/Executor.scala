object Executor:

  def execute(nodes: Seq[Node]): CommandResult = nodes match
    case Seq() => CommandResult(false, 0)
    case head +: tail =>
      val commandName = head.value
      val arguments = tail.map(_.value)

      val result = Command.registry.get(commandName) match
        case None =>
          Command.exec(Command(externalCommand, commandName +: arguments))
        case Some(commandFunc) =>
          Command.exec(Command(commandFunc, arguments))

      result
