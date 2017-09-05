package fpis.ch03

import org.scalatest.FunSuite
import fpis.ch03.List._

/**
  * @author rturner <roy.turner@amd.com>
  */
class ListTest extends FunSuite {

  test("sum positive values") {
    assert(List.sum(List(2,3,4)) == 9)
    assert(List.sum(List(10,40,25)) == 75)
  }

  test("sum empty list") {
    assert(List.sum(List()) == 0)
  }

  test("tail") {
    assert(List.tail(List(5,10,15)) == List(10,15))
    assert(List.tail(List(1,2,4,5)) == List(2,4,5))
  }

  test("tail on empty List throws") {
    assertThrows[IllegalArgumentException](List.tail(List()))
  }

  test("setHead") {
    assert(List.setHead(5, List(1,2,3)) == List(5,2,3))
  }

  test("setHead on empty List throws") {
    assertThrows[IllegalArgumentException](List.setHead(25,List()))
  }

  test("drop") {
    assert(List.drop(List(1,2,3), 0) == List(1,2,3))
    assert(List.drop(List(1,2,3,4), 2) == List(3,4))
    assert(List.drop(List(1,2,3), 5) == List())
    assert(List.drop(List(), 3) == List())
  }

  test("dropWhile") {
    assert(dropWhile(List(1,2,3,4), (x: Int) => x < 3) == List(3,4))
    assert(dropWhile(List(1,2,3), (x: Int) => x > 5) == List(1,2,3))
    assert(dropWhile(List(), (x: Int) => x < 3) == List())
  }

  test("init") {
    assert(init(List(1,2,3,4)) == List(1,2,3))
  }

  test("length") {
    assert(length(List()) == 0)
    assert(length(List(1,2,3)) == 3)
  }

  test("foldLeft") {
    assert(foldLeft(List(1.0,2.0,3.0,4.0), 1.0)(_*_) == 24.0)
    assert(foldLeft(List(1,2,3,4), 0)(_+_) == 10)
  }

  test("sumL") {
    assert(sumL(List()) == 0)
    assert(sumL(List(1)) == 1)
    assert(sumL(List(1,2,3,4)) == 10)
  }

  test("productL") {
    assert(productL(List()) == 1.0)
    assert(product(List(5)) == 5.0)
    assert(product(List(2,7)) == 14.0)
  }

  test("lengthL") {
    assert(lengthL(List()) == 0)
    assert(lengthL(List(25)) == 1)
    assert(lengthL(List(5,10,15,20)) == 4)
  }
}
