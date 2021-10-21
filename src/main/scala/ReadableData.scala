case class MachineReadableObject(
  data:
    Null |
    Boolean |
    Long |
    Double |
    String |
    Option[MachineReadableObject] |
    Seq[MachineReadableObject] |
    Map[String, MachineReadableObject]
):
  def encode(): String =
    this.data match
      case null => "null"
      case b: Boolean => if b then "true" else "false"
      case num: Long => s"${num}"
      case num: Double => s"${num}"
      case str: String => s"\"${str}\""
      case Some(obj) => obj.encode()
      case None => ""
      case seq: Seq[MachineReadableObject] => s"[${seq.map(_.encode()).mkString(",")}]"
      case map: Map[String, MachineReadableObject] => s"{${map.map((k, v) => s"\"${k}\":${v.encode()}").mkString(",")}}"

enum ReadableData:
  case HumanReadable(data: String)
  case MachineReadable(data: MachineReadableObject)
