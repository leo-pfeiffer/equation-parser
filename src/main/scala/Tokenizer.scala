trait Token
case class SumToken() extends Token
case class DifferenceToken() extends Token
case class ProductToken() extends Token
case class DivisionToken() extends Token
case class NumberToken(n: Double) extends Token
case class LeftParensToken() extends Token
case class RightParensToken() extends Token
case class PowerToken() extends Token

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
