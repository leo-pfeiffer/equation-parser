import scala.annotation.tailrec
/** Token of an expression */
abstract class Token()

/** Token representing an operator */
abstract class OperatorToken() extends Token {
    /** Precedence value of the operator */
    def precedence: Int
}

trait Associates
/** Associates left */
trait Left extends Associates
/** Associates right */
trait Right extends Associates

/** Token representing sum */
case class SumToken() extends OperatorToken, Left {
    def precedence = 2
}

/** Token representing difference */
case class DifferenceToken() extends OperatorToken, Left {
    def precedence = 2
}

/** Token representing product */
case class ProductToken() extends OperatorToken, Left {
    def precedence = 3
}

/** Token representing division */
case class DivisionToken() extends OperatorToken, Left {
    def precedence = 3
}

/** Token representing power */
case class PowerToken() extends OperatorToken, Right {
    def precedence = 4
}

/** Token representing number */
case class NumberToken(n: Double) extends Token

/** Token representing left parenthesis */
case class LeftParensToken() extends Token

/** Token representing right parenthesis */
case class RightParensToken() extends Token

/** 
 * Tokenize string expression.
 * 
 * Tokenize a string representation of an arithmentic expression. 
 * 
 * @param rawExpression: String representation of expression.
 * @return List of tokens of the expression.
 * */
def tokenize(rawExpression: String): List[Token] = {

    // Regex representing a double.
    val numPattern = "(\\-?\\d*\\.?\\d+)".r

    /**
     * Tokenize a single string.
     * 
     * @param x: String to tokenize
     * @returns Corresponding token
     * */
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

    /**
     * Handle negative numbers.
     * 
     * Negative numbers are prefixed with a zero
     * e.g. (-1) -> (0-1)
     * to maintian both a left and right expression of the Difference operator.
     * 
     * @param tokens: list of tokens to handle
     * @returns tokens with inserted zeros
     * */
    def handleNegative(tokens: List[Token]): List[Token] = 
        
        /**
         * Insert prefix zero.
         * 
         * If the token list starts with "(, -" insert a zero.
         * 
         * @param tokens: list of params to check
         * @return token list with inserted zero
         * */
        def insert(tokens: List[Token]): List[Token] = tokens match {
            case a :: b :: rest => {
                a match {
                    case _a: LeftParensToken => {
                        b match {
                            case _b: DifferenceToken => _a :: NumberToken(0) :: Nil
                            case _ => _a :: Nil
                        }
                    }
                    case _ => a :: Nil
                }
            }
            case a :: Nil => a :: Nil
            case _ => Nil
        }

        /** Recursively insert zeros where necessary */ 
        def recur(tokens: List[Token]): List[Token] =
            if tokens.isEmpty then Nil
            else if tokens.tail.isEmpty then tokens
            else insert(tokens) ++ recur(tokens.tail)

        recur(tokens)


    // String splitted into (un-tokenized) parts
    val splitted = rawExpression
        .filterNot(_.isWhitespace)
        .split("(?=[)(+/*-])|(?<=[)(+/*-])|(?=[\\^])|(?<=[\\^])")
        .map(_.trim)

    // tokenize each element
    val tokenized = splitted.map(tokenizeOne).toList

    // handle negative numbers
    handleNegative(tokenized)
}

extension(tokenList: List[Token])
    def parser: Expression = parse(tokenList)
