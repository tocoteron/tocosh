enum TokenKind:
  case Plain

case class Token(
  kind: TokenKind,
  value: String,
)

object Tokenizer:
  def tokenize(input: String): List[Token] =
    input
      .split(" ")
      .filter(_ != "")
      .map(Token(TokenKind.Plain, _))
      .toList
