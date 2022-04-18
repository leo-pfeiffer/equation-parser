import scala.collection.mutable.Stack
import scala.annotation.tailrec

def parse(tokens: List[Token]): Expression =
    val postfix = shuntingYard(tokens)
    reversePostfix(postfix)

def reversePostfix(tokens: List[Token]): Expression = 

    def isOperator(t: Token): Boolean = t match {
        case a: OperatorToken => true
        case _ => false
    }

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


def shuntingYard(tokens: List[Token]): List[Token] = 

    def isLeftAssoc(t: Token): Boolean = t match {
        case a: Left => true
        case _ => false
    }

    def isRightAssoc(t: Token): Boolean = t match {
        case a: Right => true
        case _ => false
    }

    def isLeftParens(t: Token): Boolean = t match {
        case a: LeftParensToken => true
        case _ => false
    }

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

    @tailrec
    def rightParensUpdate(postfix: List[Token], stack: Stack[Token]): (Stack[Token], List[Token]) =
        if (isLeftParens(stack.head)) {stack.pop; (stack, postfix)}
        else rightParensUpdate(postfix :+ stack.pop, stack)

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
