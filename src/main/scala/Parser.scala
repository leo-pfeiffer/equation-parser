def parse(tokens: List[Token]): Expression =
    val postfix = shuntingYard(tokens)
    reversePostfix(postfix)

def reversePostfix(tokens: List[Token]): Expression = 
    import scala.collection.mutable.Stack
    val stack: Stack[Expression] = new Stack[Expression]
    for (token <- tokens) {
        if (isOperator(token)) token match {
            case t: SumToken => stack.push(Sum(stack.pop, stack.pop))
            case t: DifferenceToken => val x = stack.pop; stack.push(Difference(stack.pop, x))
            case t: ProductToken => stack.push(Product(stack.pop, stack.pop))
            case t: DivisionToken => val x = stack.pop; stack.push(Division(stack.pop, x))
            case t: PowerToken => val x = stack.pop; stack.push(Power(stack.pop, x))
            case _ => throw new RuntimeException(s""""$token" is not an operator""")
        }
        else token match {
            case t: NumberToken => stack.push(Number(t.n))
            case _ => throw new RuntimeException(s""""$token" is not valid here""")   
        }
    }
    stack.pop

def shuntingYard(tokens: List[Token]): List[Token] = 
    
    var i = 0
    var s: List[Token] = List()
    var postfix: List[Token] = List()

    def matchCond(o: OperatorToken): Boolean = s.head match {
        case o2: OperatorToken => {
            (isLeftAssoc(o) && (o.precedence <= o2.precedence)) ||
            (isRightAssoc(o) && (o.precedence < o2.precedence))
        }
        case _ => false
    }

    for(token <- tokens) {

        token match {
            case n: NumberToken => postfix = postfix :+ n
            case o: OperatorToken => {
                if (s.isEmpty) then s = o :: s
                else {
                    while(s != Nil && matchCond(o)) {
                        postfix = postfix :+ s.head
                        s = s.tail
                    }
                    s = o :: s
                }
            }
            case l: LeftParensToken => s = l :: s
            case r: RightParensToken => {
                while (!isLeftParens(s.head)) {
                    postfix = postfix :+ s.head
                    s = s.tail
                }
                s = s.tail
            }
        }
    }

    postfix ++ s

def isOperator(t: Token): Boolean = t match {
    case a: OperatorToken => true
    case _ => false
}

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