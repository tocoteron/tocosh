object Shell:
  def run(): Int =
    while true do
      print("$ ")

      val input = io.StdIn.readLine()
      val tokens = Tokenizer.tokenize(input)
      val nodes = Parser.parse(tokens)
      val res = Executor.execute(nodes)

      /*
      val writer = res.data match
        case data: ReadableData => Writer(data)
        case e: Exception => Writer(
          ReadableData.HumanReadable(e.getMessage())
        )

      writer.write()
      */

      if res.exit then
        return res.exitCode

    return 0
