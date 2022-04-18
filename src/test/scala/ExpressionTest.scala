import org.scalatest.funsuite.AnyFunSuite

class ExpresionTest extends AnyFunSuite {

    trait TestExpressions:
        val n1 = new Number(1);
        val n2 = new Number(2);
        val n3 = new Number(3);
        val n4 = new Number(4);
        val n5 = new Number(5);
        val s12 = new Sum(n1, n2);
        val d12 = new Difference(n1, n2);
        val prod12 = new Product(n1, n2);
        val div12 = new Division(n1, n2);
        val pow23 = new Power(n2, n3)

    test("number value") {
        new TestExpressions {
            assert(n1.value == 1)
        }
    }

    test("sum operators") {
        new TestExpressions {
            assert(s12.left == n1)
            assert(s12.right == n2)
        }
    }

    test("evaluate simple two-part sum") {
        new TestExpressions {
            assert(evaluate(s12) == 3)
        }
    }

    test("evaluate simple two-part difference") {
        new TestExpressions {
            assert(evaluate(d12) == -1)
        }
    }

    test("evaluate simple two-part product") {
        new TestExpressions {
            assert(evaluate(prod12) == 2)
        }
    }


    test("evaluate simple two-part division") {
        new TestExpressions {
            assert(evaluate(div12) == 0.5)
        }
    }

    test("evaluate simple power") {
        new TestExpressions {
            assert(evaluate(pow23) == 8)
        }
    }

    test("mult-part evaluation with order") {
        new TestExpressions {
            // 1 + 2 * 3 - 4 / 5 == 6.2
            val expr = Difference(Sum(n1, Product(n2, n3)), Division(n4, n5))
            assert(evaluate(expr) == 6.2)
        }
    }

    test("commutativity of sum") {
        assert (Sum(Number(1), Number(2)) == Sum(Number(2), Number(1)))
    }

    test("commutativity of product") {
        assert (Product(Number(1), Number(2)) == Product(Number(2), Number(1)))
    }
}