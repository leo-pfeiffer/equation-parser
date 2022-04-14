object Main {
    def main(args: Array[String]) = args match
        case Array(_) => tokenize(_)
        case Array() => throw new java.lang.IllegalArgumentException("Too few arguments!")
        case _ => throw new java.lang.IllegalArgumentException("Too many arguments!")
}