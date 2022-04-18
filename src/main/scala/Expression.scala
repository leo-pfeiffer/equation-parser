/** Represents arithmetic expression. */
trait Expression

/** Arithmetic operator */
trait Operator extends Expression {
    val left: Expression
    val right: Expression
}

/** Number (double) of an arithmetic expression */
case class Number(n: Double) extends Expression {
    def value = n
}

/** 
 * Represents operators whose left and right side are commutative,
 * i.e. the order of the LHS and RHS expression does not matter.
 * */
trait Commutative extends Operator {
    def canEqual(a: Any) = a.isInstanceOf[Commutative]

    override def equals(that: Any): Boolean =
        that match {
            case that: Commutative => {
                that.canEqual(this) &&
                ((this.left == that.left && this.right == that.right) ||
                    (this.left == that.right && this.right == that.left))
            }
            case _ => false
        }

    override def hashCode: Int = {
        val prime = 31
        prime + left.hashCode * right.hashCode
    }
}

case class Sum(left: Expression, right: Expression) extends Commutative
case class Difference(left: Expression, right: Expression) extends Operator
case class Product(left: Expression, right: Expression) extends Commutative
case class Division(left: Expression, right: Expression) extends Operator
case class Power(left: Expression, right: Expression) extends Operator

/**
 * Evaluate an expression.
 * @param expression: expression to evaluate
 * @return value of the expression
 * */
def evaluate(expression: Expression): Double = expression match
    case Number(n) => n
    case Sum(left, right) => evaluate(left) + evaluate(right)
    case Difference(left, right) => evaluate(left) - evaluate(right)
    case Product(left, right) => evaluate(left) * evaluate(right)
    case Division(left, right) => evaluate(left) / evaluate(right)
    case Power(left, right) => scala.math.pow(evaluate(left), evaluate(right))

extension(expr: Expression)
    def eval: Double = evaluate(expr)