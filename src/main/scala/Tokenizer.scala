enum TokenKind:
  case Plain

case class Token(
  kind: TokenKind,
  value: String
)

case class Tokenizer(
  input: String
):

  def tokenize(): List[Token] =
    this.input
      .split(" ")
      .filter(_ != "")
      .map(Token(TokenKind.Plain, _))
      .toList
