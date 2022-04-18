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

    test("negative number") {
        val s = "3 + (-10)"
        assert(getResult(s).eval == -7)
    }

    test("negative number leading") {
        val s = "(-10) + 3"
        assert(getResult(s).eval == -7)
    }

    test("long equation") {
        val s = "1+8-342*23^(1^(1^(1^2)))-22313*2+((-16)/(13*2))/(5/(5/5))-(-(-1))"
        val expected = -52484.1230
        val actual = getResult(s).eval
        assert(math.abs(expected - actual) < 0.1)
    }

    test("negative number with multiple parens") {
        val s = "((-1))"
        assert(getResult(s).eval == -1)
    }
}