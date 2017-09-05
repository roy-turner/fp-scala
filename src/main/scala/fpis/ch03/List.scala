package fpis.ch03

/**
  * @author rturner <roy.turner@amd.com>
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x*product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException("empty list")
    case Cons(_,t) => t
  }

  // 3.3
  def setHead[A](h: A, xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException("empty list")
    case Cons(_,t) => Cons(h,t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  // 3.5
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) if p(h) => dropWhile(t,p)
    case _ => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("init of empty List")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }
  // init can't be implemented in constant time because the returned List is
  // built as the List is traversed (nearly the entire List must be looked at
  // and copied into a new List)

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs,z)(f))
  }

  // 3.7
  // foldRight cannot short circuit if 0.0 is encountered. This is because
  // foldRight must traverse to the end of the List before collapsing back down.
  // To make short circuiting work you'd need to introduce laziness so that the
  // recursed `foldRight` isn't evaluated - thus short circuiting the fold.

  // 3.8
  // This evaluates to the original List. This says that the relationship
  // between foldRight and the data constructors of List are right associative (Cons),
  // and that the "zero" value is Nil

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_,acc) => 1+acc)

  // 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
  }

  // 3.11
  def sumL(xs: List[Int]): Int = foldLeft(xs, 0)(_+_)

  def productL(xs: List[Double]): Double = foldLeft(xs, 1.0)(_*_)

  def lengthL[A](as: List[A]): Int = foldLeft(as, 0)((acc,_) => 1+acc)
}
