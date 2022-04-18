object Main {
    def main(args: Array[String]) = args match
        case Array(x: String) => printResult(getResult(x))
        case Array() => throw new java.lang.IllegalArgumentException("Too few arguments!")
        case _ => throw new java.lang.IllegalArgumentException("Too many arguments!")

    def getResult(rawExpr: String): Expression = tokenize(rawExpr).parser
    def printResult(expr: Expression): Unit = println(expr.eval)
}