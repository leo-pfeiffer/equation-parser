def parse(tokens: List[Token]): Expression = 

    // operator precedence parser
    // https://ycpcs.github.io/cs340-fall2018/lectures/lecture06.html
    // https://en.wikipedia.org/wiki/Operator-precedence_parser
    def recur(lhs: List[Token], minPrec: Int): Expression = ???

    recur(tokens, 0)
