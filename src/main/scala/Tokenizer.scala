trait Token
case class SumToken() extends Token
case class DifferenceToken() extends Token
case class ProductToken() extends Token
case class DivisionToken() extends Token
case class NumberToken(n: Double) extends Token

def tokenize(rawExpression: String): List[Token] = {

    val numPattern = "(\\-?\\d*\\.?\\d+)".r

    def tokenizeOne(x: String) = x match {
        case "+" => SumToken()
        case "-" => DifferenceToken()
        case "*" => ProductToken()
        case "/" => DivisionToken()
        case numPattern(c: String) => NumberToken(c.toDouble)
        case _ => throw new RuntimeException("Illegal Token")
    }

    val trimmed = rawExpression.split("(?=[+/*-])|(?<=[+/*-])").map(_.trim)

    trimmed.map(tokenizeOne).toList
}
