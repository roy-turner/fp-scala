package fpis.ch04

sealed trait Either[+E, +A] {
  // 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil):Either[E, List[A]])((ea, acc) => ea.map2(acc)(_ :: _))

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil):Either[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _))

  // 4.8
  // map2 would need to accumulate the errors, and return an Either[List[E], A].
  // The current implementation of map2 would need to be rewritten using pattern
  // matching instead of flatMap/map.
  // mkPerson could stay the same if the Strings are just concatenated; that would
  // avoid needing it being a List.
  //
  // A new data type could be used that would combine errors. As is, Either can't
  // accomplish this since it skips evaluating further computations once
  // a single Left is encountered.
  //
  // orElse could stay the same since it's purpose isn't to retain the errors,
  // but give a "default" instead.
  //
  // sequence would have to accumulate errors and return an
  // Either[List[E], List[A]]
  //
  // traverse would also have to accumulate errors and return an
  // Either[List[E], List[B]]
}
