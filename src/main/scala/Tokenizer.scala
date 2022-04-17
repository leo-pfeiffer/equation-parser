abstract class Token() {
    def precedence: Int
}

case class SumToken() extends Token {
    def precedence = 1
}

case class DifferenceToken() extends Token{
    def precedence = 1
}

case class ProductToken() extends Token {
    def precedence = 2
}

case class DivisionToken() extends Token {
    def precedence = 2
}

case class PowerToken() extends Token {
    def precedence = 3
}

case class NumberToken(n: Double) extends Token {
    def precedence = 4
}

case class LeftParensToken() extends Token {
    def precedence = 4
}

case class RightParensToken() extends Token {
    def precedence = 4
}

def tokenize(rawExpression: String): List[Token] = {

    val numPattern = "(\\-?\\d*\\.?\\d+)".r

    def tokenizeOne(x: String) = x match {
        case "+" => SumToken()
        case "-" => DifferenceToken()
        case "*" => ProductToken()
        case "/" => DivisionToken()
        case "^" => PowerToken()
        case "(" => LeftParensToken()
        case ")" => RightParensToken()
        case numPattern(c: String) => NumberToken(c.toDouble)
        case _ => throw RuntimeException("Illegal Token")
    }

    val trimmed = rawExpression
        .filterNot(_.isWhitespace)
        .split("(?=[)(+/*-^])|(?<=[)(+/*-^])")
        .map(_.trim)

    trimmed.map(tokenizeOne).toList
}
