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

      if res.exit then
        return res.exitCode

    return 0
