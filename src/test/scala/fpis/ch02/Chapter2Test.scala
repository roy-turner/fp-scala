package fpis.ch02

import org.scalatest.FunSuite
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import Ch02._

class Chapter2Test extends FunSuite {

  test("fibonocci") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
  }

  test("isSorted") {
    assert(isSorted[Int](Array(1, 2, 3), (x,y) => x < y))
    assert(!isSorted[Int](Array(1, 4, 3), (x,y) => x < y))
    assert(isSorted[Char](Array('a', 'b', 'c'), (x,y) => x < y))
    assert(!isSorted[Char](Array('x', 'b', 'c'), (x,y) => x < y))
  }

}

object Chapter2Properties extends Properties("Chapter2") {
  property("curry") = forAll({
    (f: (String, Char) => Int, s: String, c: Char) =>
      curry(f)(s)(c) == f(s, c)
  })

  property("uncurry") = forAll({
    (f: String => Char => Int, s: String, c: Char) =>
      uncurry(f)(s, c) == f(s)(c)
  })

  property("compose") = forAll({
    (f: String => Int, g: Char => String, c: Char) =>
      compose(f, g)(c) == f(g(c))
  })
}
