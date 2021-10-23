case class Writer(
  data: ReadableData
):
  def write() = this.data match
    case ReadableData.HumanReadable(data) => println(data)
    case ReadableData.MachineReadable(data) => println(data.encode())
