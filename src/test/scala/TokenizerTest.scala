import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite {
    val expected = List(
            NumberToken(1), SumToken(), NumberToken(2), DivisionToken(),
            NumberToken(3), DifferenceToken(), NumberToken(4)
        )

    test("single digit number") {
        assert(tokenize("1").head == NumberToken(1))
    }

    test("double digit number") {
        assert(tokenize("12").head == NumberToken(12))
    }

    test("floating point number single digit leading") {
        assert(tokenize("1.2").head == NumberToken(1.2))
    }

    test("floating point number double digit leading") {
        assert(tokenize("12.3").head == NumberToken(12.3))
    }

    test("floating point number double digit leading, double digit decimal") {
        assert(tokenize("12.34").head == NumberToken(12.34))
    }

    test("negative number") {
        assert(tokenize("(-1)") == List(
            LeftParensToken(), NumberToken(0), DifferenceToken(),
            NumberToken(1), RightParensToken()
        ))
    }

    test("plain equation") {
        val eq = "1+2/3-4"    
        assert(tokenize(eq) == expected)
    }

    test("equation with whitespaces") {
        val eq = "1 + 2\n/\n3-\t4"    
        assert(tokenize(eq) == expected)
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

    test ("equation with negative number") {
        val eq = "10 * (-10)"
        val expected = List(
            NumberToken(10), ProductToken(), 
            LeftParensToken(), NumberToken(0), 
            DifferenceToken(), NumberToken(10),
            RightParensToken()
        )
        assert(tokenize(eq) == expected)
    }

    test ("equation with power") {
        val expected = List(
            NumberToken(1), PowerToken(), NumberToken(2)
        )
        val eq = "1^2"
        assert(tokenize(eq) == expected)
    }

    test ("sum") {
        val eq = "1+2+3"
        val exp = List(NumberToken(1), SumToken(), NumberToken(2), 
            SumToken(), NumberToken(3))
        assert(tokenize(eq) == exp)   
    }

    test ("difference") {
        val eq = "4-1-2"
        val exp = List(NumberToken(4), DifferenceToken(), NumberToken(1), 
            DifferenceToken(), NumberToken(2))
        assert(tokenize(eq) == exp)   
    }

    test ("product") {
        val eq = "2*3*4"
        val exp = List(NumberToken(2), ProductToken(), NumberToken(3), 
            ProductToken(), NumberToken(4))
        assert(tokenize(eq) == exp)   
    }

    test ("division") {
        val eq = "12/3/2"
        val exp = List(NumberToken(12), DivisionToken(), NumberToken(3), 
            DivisionToken(), NumberToken(2))
        assert(tokenize(eq) == exp)   
    }

    test ("power") {
        val eq = "2^3^2"
        val exp = List(NumberToken(2), PowerToken(), NumberToken(3), 
            PowerToken(), NumberToken(2))
        assert(tokenize(eq) == exp)   
    }

    test ("parens") {
        val eq = "3*(4-2)"
        val exp = List(
            NumberToken(3), ProductToken(), LeftParensToken(), NumberToken(4),
            DifferenceToken(), NumberToken(2), RightParensToken()
        )
        assert(tokenize(eq) == exp)   
    }
}