package fpis.ch04

import org.scalatest.FunSuite
import fpis.ch04.Option
import fpis.ch04.Option._

class OptionTest extends FunSuite {
  test("map") {
    assert(Some(3).map(x => x + 1) == Some(4))
    assert((None:Option[Int]).map(x => x + 1) == None)
  }

  test("flatMap") {
    assert(Some(25).flatMap(x => Some(x+5)) == Some(30))
    assert((None:Option[Int]).flatMap(x => Some(x+5)) == None)
  }

  test("getOrElse") {
    assert(Some(5).getOrElse(10) == 5)
    assert((None:Option[Int]).getOrElse(10) == 10)
  }

  test("orElse") {
    assert(Some(5).orElse(Some(10)) == Some(5))
    assert((None:Option[Int]).orElse(Some(10)) == Some(10))
  }

  test("filter") {
    assert(Some(50).filter(_ > 20) == Some(50))
    assert(Some(5).filter(_ > 20) == None)
    assert((None:Option[Int]).filter(_ > 20) == None)
  }

  test("map2") {
    assert(map2(Some(50), Some(21))(_+_) == Some(71))
    assert(map2(None:Option[Int], Some(21))(_+_) == None)
    assert(map2(Some(50), None:Option[Int])(_+_) == None)
    assert(map2(None:Option[Int], None:Option[Int])(_+_) == None)
  }

  test("sequence") {
    assert(sequence(List(Some(1), Some(4), Some(5))) == Some(List(1,4,5)))
    assert(sequence(List(None, Some(4), Some(5))) == None)
    assert(sequence(List(Some(1), None, Some(5))) == None)
    assert(sequence(List(Some(1), Some(4), None)) == None)
    assert(sequence(List(Some(1), None, None)) == None)
  }

  test("traverse") {
    assert(traverse(List(1,2,3,4,5))(n => if (n < 10) Some(n) else None) == Some(List(1,2,3,4,5)))
    assert(traverse(List(1,2,3,4,5))(n => if (n < 5) Some(n) else None) == None)
  }

  test("sequenceViaTraverse") {
    assert(sequenceViaTraverse(List(Some(1), Some(4), Some(5))) == Some(List(1,4,5)))
    assert(sequenceViaTraverse(List(None, Some(4), Some(5))) == None)
    assert(sequenceViaTraverse(List(Some(1), None, Some(5))) == None)
    assert(sequenceViaTraverse(List(Some(1), Some(4), None)) == None)
    assert(sequenceViaTraverse(List(Some(1), None, None)) == None)
  }

}
