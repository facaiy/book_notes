package io.github.facaiy

object ChapterTwo {
  // 2.4.1
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, e: (Int, Int)): Int =
      if (n == 0) 0
      else if (n == 1) 1
      else if (n == 2) e._1 + e._2
      else go(n-1, (e._2, e._1 + e._2))

    go(n, (0, 1))
  }

  // 2.5.1
  def findFirst[A](as: Array[A])(p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if(p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (! ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  // 2.6
  def partiall[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
