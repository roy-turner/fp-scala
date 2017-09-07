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

  test("reverse") {
    assert(reverse(List()) == List())
    assert(reverse(List(1,2,3)) == List(3,2,1))
    assert(reverse(List(25,20,32)) == List(32,20,25))
  }

  test("foldLViaR") {
    assert(foldLViaR(List(5,2), 0)(_-_) == -7)
    assert(foldLViaR(List(1,2,3,4), 0)(_-_) == -10)
    assert(foldLViaR(List(25, 35, 27), Nil:List[Int])((acc,a) => Cons(a,acc)) == List(27,35,25))
  }

  test("foldRViaL") {
    assert(foldRViaL(List(5,2), 0)(_-_) == 3)
    assert(foldRViaL(List(1,2,3,4), 0)(_-_) == -2)
    assert(foldRViaL(List(25, 35, 27), Nil:List[Int])(Cons(_,_)) == List(25,35,27))
  }

  test("append") {
    assert(append(List(), List()) == List())
    assert(append(List(), List(25)) == List(25))
    assert(append(List(25), List()) == List(25))
    assert(append(List(25), List(35)) == List(25,35))
    assert(append(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))
  }

  test("concat") {
    assert(concat(List(List(), List())) == List())
    assert(concat(List(List(1,2), List())) == List(1,2))
    assert(concat(List(List(), List(3,4))) == List(3,4))
    assert(concat(List(List(1,2), List(3,4))) == List(1,2,3,4))
    assert(concat(List(List(1,2), List(3,4), List(25,35,45))) == List(1,2,3,4,25,35,45))
  }

  test("add1") {
    assert(add1(List()) == List())
    assert(add1(List(1,2,3)) == List(2,3,4))
  }

  test("dblToStr") {
    assert(dblToStr(List()) == List())
    assert(dblToStr(List(2.5, 3.5, 4.5)) == List("2.5", "3.5", "4.5"))
  }

  test("map") {
    assert(map(List[Int]())(_+1) == List())
    assert(map(List[Int](1,2,3))(_+1) == List(2,3,4))
    assert(map(List[Double](1.0,2.0,3.0))(_.toString) == List("1.0","2.0","3.0"))
  }

  test("filter") {
    assert(filter(List[Int]())(i => i%2 == 0) == List())
    assert(filter(List[Int](1,2,3,4,5))(i => i%2 == 0) == List(2,4))
    assert(filter(List[String]("hi","there","how","are","you"))(_.length <4) == List("hi","how","are","you"))
  }

  test("evens") {
    assert(evens(List()) == List())
    assert(evens(List(1,2,3,4,5,6,7,8)) == List(2,4,6,8))
  }

  test("flatMap") {
    assert(flatMap(List[Int]())(n => List(n,n)) == List())
    assert(flatMap(List[Int](1,2,3))(n => List(n,n)) == List(1,1,2,2,3,3))
  }

  test("filterViaFlatMap") {
    assert(filterViaFlatMap(List[Int]())(i => i%2 == 0) == List())
    assert(filterViaFlatMap(List[Int](1,2,3,4,5))(i => i%2 == 0) == List(2,4))
    assert(filterViaFlatMap(List[String]("hi","there","how","are","you"))(_.length <4) == List("hi","how","are","you"))
  }

  test("addElems") {
    assert(addElems(List(), List(1,2,3)) == Nil)
    assert(addElems(List(1,2,3), List()) == Nil)
    assert(addElems(List(1,2,3), List(4,5,6)) == List(5,7,9))
  }

  test("zipWith") {
    assert(zipWith(List[Int](), List[Int]())(_+_) == List())
    assert(zipWith(List[Int](1,2,3), List[Int]())(_+_) == List())
    assert(zipWith(List[Int](), List[Int](1,2,3))(_+_) == List())
    assert(zipWith(List[Int](1,2,3), List[Int](4,5,6))(_+_) == List(5,7,9))
  }

  test("hasSubsequence") {
    val xs = List(1,2,3,4)
    assert(hasSubsequence(xs, List(1,2)))
    assert(hasSubsequence(xs, List(2,3)))
    assert(hasSubsequence(xs, List(2,3,4)))
    assert(hasSubsequence(xs, List(1,2,3,4)))
    assert(hasSubsequence(xs, List(1)))
    assert(hasSubsequence(xs, List(2)))
    assert(hasSubsequence(xs, List(3)))
    assert(hasSubsequence(xs, List(4)))
  }
}
