import org.scalatest.funsuite.AnyFunSuite
import Main.getResult

class MainTest extends AnyFunSuite {
    test("sum") {
        val s = "1+2+3"
        assert(getResult(s).eval == 6)
    }

    test("difference") {
        val s = "4-1-2"
        assert(getResult(s).eval == 1)
    }

    test("product") {
        val s = "2*3*4"
        assert(getResult(s).eval == 24)
    }

    test("division") {
        val s = "12/3/2"
        assert(getResult(s).eval == 2)
    }

    test("power") {
        val s = "2^3^2"
        assert(getResult(s).eval == 512)
    }

    test("parens") {
        val s = "3*(4-2)"
        assert(getResult(s).eval == 6)
    }
}