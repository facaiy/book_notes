package io.github.facaiy.c3

/**
 * Created by facai on 4/10/17.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.3
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new IndexOutOfBoundsException()
    case Cons(x, xs) => xs
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }

  // 3.3.1
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (n <= 0) Cons(x, xs)
        else drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (!f(x)) Cons(x, xs)
        else dropWhile(xs)(f)
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new IndexOutOfBoundsException()
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, c) => c + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // ex 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((z, x) => Cons(x, z))

  // ex 3.13
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((x, y) => f(y, x))

  // ex 3.14
  def append[A](l: List[A], x: List[A]): List[A] =
    // TODO: foldLeft ?
    foldRight(l, x)((x, z) => Cons(x, z))

  // ex 3.15
  def concatenate[A](ls: List[List[A]]): List[A] =
    foldLeft(ls, Nil: List[A])((z, x) => append(x, z))

  // ex 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

  // ex 3.19
  /*
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) Cons(x, filter(xs)(f))
        else filter(xs)(f)
    }
 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as){ x =>
      if (f(x)) List(x)
      else Nil
    }

  // ex 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    val rs = map(as)(f)

    foldRight(rs, Nil: List[B])((x, z) => append(x, z))
  }

  // ex 3.22
  def zipWith[A, B, C](l0: List[A], l1: List[B])(f: (A, B) => C): List[C] =
    l0 match {
      case Nil =>
        l1 match {
          case Nil => Nil
          case _ => throw new AssertionError("Lists should have the same length.")
        }

      case Cons(x, xs) =>
        l1 match {
          case Nil => throw new AssertionError("Lists should have the same length.")
          case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
        }
    }

  // ex 3.24
  @annotation.tailrec
  def starsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => starsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if starsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
