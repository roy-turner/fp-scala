package fpis.ch04

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3
  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C): Option[C] =
    oa.flatMap(a => ob.map(f(a,_)))

  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil):Option[List[A]])((oa, acc) => map2(oa, acc)(_ :: _))

  // 4.5
  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil):Option[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def traverseIneffecient[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence { as map f }

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)
}
