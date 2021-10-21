enum NodeKind:
  case Command, Argument

case class Node(
  kind: NodeKind,
  value: String
)

case class Parser(
  tokenizer: Tokenizer
):

  def parse(): List[Node] =
    this.tokenizer.tokenize()
      .zipWithIndex
      .map((token, index) =>
        if index == 0 then
          Node(NodeKind.Command, token.value)
        else
          Node(NodeKind.Argument, token.value)
      )
