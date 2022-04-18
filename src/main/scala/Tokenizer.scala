abstract class Token()

abstract class OperatorToken() extends Token {
    def precedence: Int
}

trait Associates
trait Left extends Associates
trait Right extends Associates

case class SumToken() extends OperatorToken, Left {
    def precedence = 2
}

case class DifferenceToken() extends OperatorToken, Left {
    def precedence = 2
}

case class ProductToken() extends OperatorToken, Left {
    def precedence = 3
}

case class DivisionToken() extends OperatorToken, Left {
    def precedence = 3
}

case class PowerToken() extends OperatorToken, Right {
    def precedence = 4
}

case class NumberToken(n: Double) extends Token

case class LeftParensToken() extends Token

case class RightParensToken() extends Token

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
        case _ => throw RuntimeException(s""""$x is not legal""")
    }

    val trimmed = rawExpression
        .filterNot(_.isWhitespace)
        //.split("(?=[)(+/*-^])|(?<=[)(+/*-^])")
        .split("(?=[)(+/*-])|(?<=[)(+/*-])|(?=[\\^])|(?<=[\\^])")
        .map(_.trim)

    trimmed.map(tokenizeOne).toList
}

extension(tokenList: List[Token])
    def parser: Expression = parse(tokenList)
