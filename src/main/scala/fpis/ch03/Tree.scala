package fpis.ch03

/**
  * @author rturner <roy.turner@amd.com>
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l).max(maximum(r))
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(n) => 1
    case Branch(l,r) => 1 + depth(l).max(depth(r))
  }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(e) => Leaf(f(e))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  // We need a function for each data constructor (`Leaf` and `Branch`).
  // `Leaf` needs to be able to be converted to a `B`.
  // `Branch` needs to combine 2 `B`s
  // If you look at the folds for List, they also need one function per data
  // constructor:
  // `Nil` needs to be converted to a `B`. Since `Nil` takes no arguments a
  // value can simply be provided (can look at as () => B, but it's strict)
  // `Cons` needs to combine its `A` with a `B` to produce a `B`
  def fold[A,B](t: Tree[A])(lf: A => B)(bf: (B,B) => B): B = t match {
    case Leaf(e) => lf(e)
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def sizeVfold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1+_+_)

  def maximumVfold(t: Tree[Int]): Int = fold(t)(a => a)((x,y) => x.max(y))

  def depthVfold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l,r) => 1+l.max(r))

  def mapVfold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
