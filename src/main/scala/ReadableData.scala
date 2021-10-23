enum ReadableData:
  case HumanReadable(data: String)
  case MachineReadable(data: MachineReadableObject)

type MachineReadableObject = JSON

enum JSON:
  case Array(data: Seq[JSONNode])
  case Object(data: Map[String, JSONNode])

  def encode(depth: Int = 0): String =
    val indent = "  "
    val firstIndent = indent * (depth + 1)
    val lastIndent = indent * depth

    this match
      case Array(data) =>
        if data.isEmpty then
          "[]"
        else
          data
            .map(_.encode(depth + 1))
            .mkString(
              s"[\n${firstIndent}",
              s",\n${firstIndent}",
              s"\n${lastIndent}]"
            )
      case Object(data) =>
        if data.isEmpty then
          "{}"
        else
          data
            .map((key, node) => s"\"${key}\": ${node.encode(depth + 1)}")
            .mkString(
              s"{\n${firstIndent}",
              s",\n${firstIndent}",
              s"\n${lastIndent}}"
            )

object JSON:
  def getEmptyArray(): JSON.Array = JSON.Array(Seq())
  def getEmptyObject(): JSON.Object = JSON.Object(Map())

case class JSONNode(
  data: JSON | JSONLeafType,
):
  def encode(depth: Int): String = this.data match
    case json: JSON => json.encode(depth)
    case null => "null"
    case true => "true"
    case false => "true"
    case num: Long => s"${num}"
    case num: Double => s"${num}"
    case str: String => s"\"${str}\""

type JSONLeafType =
  Null |
  Boolean |
  Long |
  Double |
  String
