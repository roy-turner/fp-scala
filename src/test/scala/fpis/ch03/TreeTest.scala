package fpis.ch03

import org.scalatest.FunSuite
import fpis.ch03.Tree._

/**
  * @author rturner <roy.turner@amd.com>
  */
class TreeTest extends FunSuite  {
  test("size") {
    assert(size(Leaf(25)) == 1)
    assert(size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(size(Branch(
      Branch(
        Leaf(1), Leaf(2)),
      Leaf(3))
    ) == 5)
  }

  test("maximum") {
    assert(maximum(Leaf(25)) == 25)
    assert(maximum(Branch(Leaf(1), Leaf(2))) == 2)
    assert(maximum(Branch(
      Branch(
        Leaf(1), Leaf(20)),
      Leaf(3))
    ) == 20)
  }

  test("depth") {
    assert(depth(Leaf(25)) == 1)
    assert(depth(Branch(Leaf(1), Leaf(2))) == 2)
    assert(depth(Branch(
      Branch(
        Leaf(1), Leaf(20)),
      Leaf(3))
    ) == 3)
  }

  test("map") {
    assert(map(Leaf(25))(_+5) == Leaf(30))
    assert(map(Branch(Leaf(1), Leaf(2)))(_+1) == Branch(Leaf(2), Leaf(3)))
    assert(map(Branch(
      Branch(
        Leaf(1), Leaf(20)),
      Leaf(3))
    )(_*2) == Branch(Branch(Leaf(2), Leaf(40)), Leaf(6)))
  }

  test("sizeVfold") {
    assert(sizeVfold(Leaf(25)) == 1)
    assert(sizeVfold(Branch(Leaf(1), Leaf(2))) == 3)
    assert(sizeVfold(Branch(
      Branch(
        Leaf(1), Leaf(2)),
      Leaf(3))
    ) == 5)
  }

  test("maximumVfold") {
    assert(maximumVfold(Leaf(25)) == 25)
    assert(maximumVfold(Branch(Leaf(1), Leaf(2))) == 2)
    assert(maximumVfold(Branch(
      Branch(
        Leaf(1), Leaf(20)),
      Leaf(3))
    ) == 20)
  }

  test("depthVfold") {
    assert(depthVfold(Leaf(25)) == 1)
    assert(depthVfold(Branch(Leaf(1), Leaf(2))) == 2)
    assert(depthVfold(Branch(
      Branch(
        Leaf(1), Leaf(20)),
      Leaf(3))
    ) == 3)
  }

  test("mapVfold") {
    assert(mapVfold(Leaf(25))(_+5) == Leaf(30))
    assert(mapVfold(Branch(Leaf(1), Leaf(2)))(_+1) == Branch(Leaf(2), Leaf(3)))
    assert(mapVfold(Branch(
      Branch(
        Leaf(1), Leaf(20)),
      Leaf(3))
    )(_*2) == Branch(Branch(Leaf(2), Leaf(40)), Leaf(6)))
  }
}
