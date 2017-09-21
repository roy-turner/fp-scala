package fpis.ch04

import org.scalatest.FunSuite
import fpis.ch04.Either
import fpis.ch04.Either._

class EitherTest extends FunSuite {
  test("map") {
    assert(Right(3).map(x => x + 1) == Right(4))
    assert((Left("oops"):Either[String, Int]).map(x => x + 1) == Left("oops"))
  }

  test("flatMap") {
    assert(Right(25).flatMap(x => Right(x+5)) == Right(30))
    assert((Left("again"):Either[String, Int]).flatMap(x => Right(x+5)) == Left("again"))
  }

  test("orElse") {
    assert(Right(5).orElse(Right(10)) == Right(5))
    assert((Left("error"):Either[String, Int]).orElse(Right(10)) == Right(10))
  }

  test("map2") {
    assert(Right(50).map2(Right(21))(_+_) == Right(71))
    assert((Left("oops"):Either[String, Int]).map2(Right(21))(_+_) == Left("oops"))
    assert(Right(50).map2(Left("oops"):Either[String, Int])(_+_) == Left("oops"))
    assert((Left("oops"):Either[String, Int]).map2(Left("again"))(_+_) == Left("oops"))
  }

  test("sequence") {
    assert(sequence(List(Right(1), Right(4), Right(5))) == Right(List(1,4,5)))
    assert(sequence(List(Left("oops"):Either[String, Int], Right(4), Right(5))) == Left("oops"))
    assert(sequence(List(Right(1), Left("oops"):Either[String, Int], Right(5))) == Left("oops"))
    assert(sequence(List(Right(1), Right(4), Left("oops"):Either[String, Int])) == Left("oops"))
    assert(sequence(List(Right(1), Left("oops"):Either[String, Int], Left("again"))) == Left("oops"))
  }

  test("traverse") {
    assert(traverse(List(1,2,3,4,5))(n => if (n < 10) Right(n) else Left("oops"):Either[String, Int]) == Right(List(1,2,3,4,5)))
    assert(traverse(List(1,2,3,4,5))(n => if (n < 5) Right(n) else Left("oops"):Either[String, Int]) == Left("oops"))
  }
}
