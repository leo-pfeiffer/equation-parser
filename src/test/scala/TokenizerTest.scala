import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite {
    val expected = List(
            NumberToken(1), SumToken(), NumberToken(2), DivisionToken(),
            NumberToken(3), DifferenceToken(), NumberToken(4)
        )

    test("plain equation") {
        val eq = "1+2/3-4"    
        assert(tokenize(eq) == expected)
    }

    test("equation with whitespaces") {
        val eq = "1 + 2\n/\n3-\t4"    
        assert(tokenize(eq) == expected)
    }

    test("equation with negative number") {
        val eq = "-1+2/3-4"
        assert(tokenize(eq) == DifferenceToken() :: expected)
    }

    test("equation with character fails") {
        assertThrows[RuntimeException](
            tokenize("1 + 2 + b + 3")
        )
    }

    test ("equation with parens") {
        val expected = List(
            LeftParensToken(), NumberToken(1), SumToken(), 
            NumberToken(2), RightParensToken()
        )
        val eq = "(1+2)"
        assert(tokenize(eq) == expected)
    }

    test ("equation with power") {
        val expected = List(
            NumberToken(1), PowerToken(), NumberToken(2)
        )
        val eq = "1^2"
        assert(tokenize(eq) == expected)
    }
}