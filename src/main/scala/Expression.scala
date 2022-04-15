trait Expression

trait Operator extends Expression

case class Number(n: Double) extends Expression {
    def value = n
}

case class Sum(left: Expression, right: Expression) extends Operator
case class Difference(left: Expression, right: Expression) extends Operator
case class Product(left: Expression, right: Expression) extends Operator
case class Division(left: Expression, right: Expression) extends Operator
case class Power(left: Expression, right: Expression) extends Operator

def evaluate(expression: Expression): Double = expression match
    case Number(n) => n
    case Sum(left, right) => evaluate(left) + evaluate(right)
    case Difference(left, right) => evaluate(left) - evaluate(right)
    case Product(left, right) => evaluate(left) * evaluate(right)
    case Division(left, right) => evaluate(left) / evaluate(right)
    case Power(left, right) => scala.math.pow(evaluate(left), evaluate(right))