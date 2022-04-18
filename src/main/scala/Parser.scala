import scala.collection.mutable.Stack
import scala.annotation.tailrec

/**
 * Run the parser.
 * 
 * Converts the tokens to postfix notation and then reverses it to RPN.
 *
 * @param tokens: List of tokens in infix notation
 * @return Parsed expression
 * */
def parse(tokens: List[Token]): Expression =
    val postfix = shuntingYard(tokens)
    parsePostfix(postfix)

/**
 * Shunting Yard algorithm.
 * 
 * Converts a list of tokens from infix to postfix notation.
 * https://en.wikipedia.org/wiki/Shunting_yard_algorithm
 * 
 * @param tokens: List of tokens in infix notation
 * @return list of tokens in postfix notation
 * */
def shuntingYard(tokens: List[Token]): List[Token] = 

    /** Helper method to determine if token if left associative. */
    def isLeftAssoc(t: Token): Boolean = t match {
        case a: Left => true
        case _ => false
    }

    /** Helper method to determine if token if right associative. */
    def isRightAssoc(t: Token): Boolean = t match {
        case a: Right => true
        case _ => false
    }

    /** Helper method to determine if token if left parenthesis. */
    def isLeftParens(t: Token): Boolean = t match {
        case a: LeftParensToken => true
        case _ => false
    }

    /** Helper method to update postfix and stack during operator parsing. */
    @tailrec
    def operatorUpdate(postfix: List[Token], stack: Stack[Token], o: OperatorToken): (Stack[Token], List[Token]) =
        def matchCond(o: OperatorToken, stack: Stack[Token]): Boolean = stack.head match {
            case o2: OperatorToken => {
                (isLeftAssoc(o) && (o.precedence <= o2.precedence)) ||
                (isRightAssoc(o) && (o.precedence < o2.precedence))
            }
            case _ => false
        }
        if (stack.isEmpty || !matchCond(o, stack)) (stack.push(o), postfix)
        else operatorUpdate(postfix :+ stack.pop, stack, o)

    /** Helper method to update postfix and stack during right parens parsing. */
    @tailrec
    def rightParensUpdate(postfix: List[Token], stack: Stack[Token]): (Stack[Token], List[Token]) =
        if (isLeftParens(stack.head)) {stack.pop; (stack, postfix)}
        else rightParensUpdate(postfix :+ stack.pop, stack)

    /**
     * Recursive method of shunting yard.
     * 
     * @param stack: Stack of tokens left to place
     * @param postfix: Tokens converted to postfix notation
     * @param tokens: Tokens in infix notation
     * @return tokens in postfix notation
     * */
    @tailrec
    def recur(stack: Stack[Token], postfix: List[Token], tokens: List[Token]): List[Token] = 
        tokens match {
            case Nil => postfix ++ stack
            case t :: rest => {
                t match {
                    case n: NumberToken => recur(stack, postfix :+ n, rest)
                    case o: OperatorToken => {
                        if (stack.isEmpty) then recur(stack.push(o), postfix, rest)
                        else {
                            val updated = operatorUpdate(postfix, stack, o)
                            recur(updated._1, updated._2, rest)
                        }
                    }
                    case l: LeftParensToken => recur(stack.push(l), postfix, rest)
                    case r: RightParensToken => {
                        val updated = rightParensUpdate(postfix, stack)
                        recur(updated._1, updated._2, rest)
                    }
                }
            }
        }

    recur(new Stack[Token], List(), tokens)

/**
 * Parses RPN to expression.
 * 
 * Takes a list of tokens in RPN and parses the expression representation.
 * https://en.wikipedia.org/wiki/Reverse_Polish_notation
 * 
 * @param tokens: List of tokens in postfix notation
 * @return parsed expression
 * */
def parsePostfix(tokens: List[Token]): Expression = 

    /** Helper method to determine if token is operator. */
    def isOperator(t: Token): Boolean = t match {
        case a: OperatorToken => true
        case _ => false
    }

    /**
     * Recursive method of the algorithm.
     * 
     * @param stack: Stack of expressions to parse
     * @param tokens: Tokens to parse.
     * @return parsed expression
     * */
    @tailrec
    def recur(stack: Stack[Expression], tokens: List[Token]): Expression = tokens match {
        case Nil => stack.pop
        case t :: rest => {
            if (isOperator(t)) t match {
                case t: SumToken => stack.push(Sum(stack.pop, stack.pop))
                case t: DifferenceToken => val x = stack.pop; stack.push(Difference(stack.pop, x))
                case t: ProductToken => stack.push(Product(stack.pop, stack.pop))
                case t: DivisionToken => val x = stack.pop; stack.push(Division(stack.pop, x))
                case t: PowerToken => val x = stack.pop; stack.push(Power(stack.pop, x))
                case _ => throw new RuntimeException(s""""$t" is not an operator""")
            }
            else t match {
                case t: NumberToken => stack.push(Number(t.n))
                case _ => throw new RuntimeException(s""""$t" is not valid here""")   
            }
            recur(stack, rest)
        }
    }

    recur(new Stack[Expression], tokens)

