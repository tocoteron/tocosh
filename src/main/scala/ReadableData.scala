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
    MachineReadableObject.encode(this, 0)

object MachineReadableObject:
  def encode(obj: MachineReadableObject, depth: Int): String =
    val indent = "  "
    val firstIndent = indent * (depth + 1)
    val lastIndent = indent * depth
    val encodeChild = (obj: MachineReadableObject) =>
      MachineReadableObject.encode(obj, depth + 1)

    obj.data match
      case null => "null"
      case bool: Boolean => if bool then "true" else "false"
      case num: Long => s"${num}"
      case num: Double => s"${num}"
      case str: String => s"\"${str}\""
      case Some(obj) => encodeChild(obj)
      case None => ""
      case seq: Seq[MachineReadableObject] =>
        val content = seq
          .map(encodeChild(_))
          .mkString(s",\n${firstIndent}")
        if seq.isEmpty then
          "[]"
        else
          s"[\n${firstIndent}${content}\n${lastIndent}]"
      case map: Map[String, MachineReadableObject] =>
        val content = map
          .map((key, data) => s"\"${key}\": ${encodeChild(data)}")
          .mkString(s",\n${firstIndent}")
        if map.isEmpty then
          "{}"
        else
          s"{\n${firstIndent}${content}\n${lastIndent}}"

enum ReadableData:
  case HumanReadable(data: String)
  case MachineReadable(data: MachineReadableObject)
