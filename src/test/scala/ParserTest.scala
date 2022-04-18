import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {
    trait TestTokens:

        val n1 = Number(1)
        val n2 = Number(2)
        val n3 = Number(3)
        val n4 = Number(4)
        val n5 = Number(5)
        val n6 = Number(6)
        val n7 = Number(7)
        val n8 = Number(8)
        val n9 = Number(9)

        val sum1 = List(NumberToken(1), SumToken(), NumberToken(2))
        val sum1expr = Sum(n1, n2)

        val sum2 = List(NumberToken(1.0), SumToken(), NumberToken(2.0), SumToken(), NumberToken(3.0))
        val sum2expr = Sum(Sum(n1, n2), n3)

        val diff1 = List(NumberToken(1), DifferenceToken(), NumberToken(2))
        val diff1expr = Difference(n1, n2)
        val div1 = List(NumberToken(1), DivisionToken(), NumberToken(2))
        val div1expr = Division(n1, n2)
        val prod1 = List(NumberToken(1), ProductToken(), NumberToken(2))
        val prod1expr = Product(n1, n2)
        val pow1 = List(NumberToken(1), PowerToken(), NumberToken(2))
        val pow1expr = Power(n1, n2)

        val eq1 = List(
            NumberToken(3), SumToken(), NumberToken(4), ProductToken(),
            NumberToken(2), DivisionToken(), LeftParensToken(), NumberToken(1),
            DifferenceToken(), NumberToken(5), RightParensToken(), PowerToken(),
            NumberToken(2), PowerToken(), NumberToken(3)
        )

        val eq1postfix = List(
            NumberToken(3), NumberToken(4), NumberToken(2),
            ProductToken(), NumberToken(1), NumberToken(5),
            DifferenceToken(), NumberToken(2), NumberToken(3),
            PowerToken(), PowerToken(), DivisionToken(), SumToken()
        )

        val eq1expr = Sum(n3, Division(Product(n4, n2), Power(Power(Difference(n1, n5), n2), n3)))


    test("parse sum1") {
        new TestTokens {
            assert(parse(sum1) == sum1expr)
        }
    }

    test("parse sum2") {
        new TestTokens {
            assert(parse(sum2) == sum2expr)
        }
    }

    test("parse difference") {
        new TestTokens {
            assert(parse(diff1) == diff1expr)
        }
    }

    test("parse division") {
        new TestTokens {
            assert(parse(div1) == div1expr)
        }
    }

    test("parse product") {
        new TestTokens {
            assert(parse(prod1) == prod1expr)
        }
    }

    test("parse power") {
        new TestTokens {
            assert(parse(pow1) == pow1expr)
        }
    }

    test("shunting yard") {
        new TestTokens {
            assert(shuntingYard(eq1) == eq1postfix)
        }
    }

    test("reverse postfix") {
        new TestTokens {
            val x = evaluate(reversePostfix(eq1postfix))
            val y = evaluate(eq1expr)
            assert(math.abs(x - y) < 0.01)
        }
    }
}