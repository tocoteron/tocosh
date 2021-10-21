case class Shell():

  def run(): Int =
    while true do
      print("$ ")

      val input = io.StdIn.readLine()
      val tokenizer = Tokenizer(input)
      val parser = Parser(tokenizer)
      val nodes = parser.parse()
      val executor = Executor(nodes)
      val res = executor.execute()

      val writer = res.data match
        case data: ReadableData => Writer(data)
        case e: Exception => Writer(
          ReadableData.HumanReadable(e.getMessage())
        )

      writer.write()

      if res.exit then
        return res.exitCode

    return 0
