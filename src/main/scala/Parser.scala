enum NodeKind:
  case Command, Argument

case class Node(
  kind: NodeKind,
  value: String,
)

object Parser:
  def parse(tokens: Seq[Token]): Seq[Node] =
    tokens
      .zipWithIndex
      .map((token, index) =>
        if index == 0 then
          Node(NodeKind.Command, token.value)
        else
          Node(NodeKind.Argument, token.value)
      )
