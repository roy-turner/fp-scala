package fpis.ch02

import annotation.tailrec

object Ch02 {

  def fib(n: Int): Int = {
    @tailrec
    def go(nn: Int, prev: Int, curr: Int): Int = {
      if (nn <= 0) prev
      else go(nn - 1, curr, curr+prev)
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (n+1 >= as.length) true // reached end of array, must be true
      else if (ordered(as(n+1), as(n))) false // higher index > lower index, so not sorted
      else go(n+1)
    }

    go(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
