extension(s: SumToken)
    def precendence = 1;

extension(d: DifferenceToken)
    def precendence = 1;

extension(p: ProductToken)
    def precendence = 2;

extension(d: DivisionToken)
    def precendence = 2;

extension(p: PowerToken)
    def precendence = 3;

def parse(tokens: List[Token]): Expression = ???