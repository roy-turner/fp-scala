package fpis.ch03

import org.scalacheck.{Arbitrary, Cogen, Gen, Properties}
import org.scalacheck.Prop.forAll
import fpis.ch03._
import fpis.ch03.List._

/**
  * @author rturner <roy.turner@amd.com>
  */
class ListProperties extends Properties("List") {
  def genFpList[A](ga: Gen[A]): Gen[fpis.ch03.List[A]] =
    Gen.listOf(ga).map(ls => ls.foldRight(Nil:List[A])(Cons(_,_)))

  implicit def arbFpList[A](implicit aa: Arbitrary[A]): Arbitrary[List[A]] =
    Arbitrary(genFpList(aa.arbitrary))

  property("foldLViaR") = forAll({
    (as: List[Int], z: String, f: (String, Int) => String) =>
      foldLViaR(as, z)(f) == foldLeft(as, z)(f)
  })
}
