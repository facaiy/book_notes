package io.github.facaiy.c10

import io.github.facaiy.c8.{Gen, Prop}

/**
 * Created by facai on 5/31/17.
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}


object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  // ex 10.1
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  // ex 10.2
  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  // ex 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    override def zero: (A) => A = x => x
  }

  // ex 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associative: Gen[Boolean] = for {
      a <- gen
      b <- gen
      c <- gen
    } yield m.op(m.op(a, b), c) == m.op(a, m.op(b, c))

    val identity: Gen[Boolean] = gen.map(x => m.op(x, m.zero) == m.op(m.zero, x))

    Prop.forAll(identity)(x => x) && Prop.forAll(associative)(x => x)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // ex 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
   as.map(f).foldLeft(m.zero)(m.op)

  // ex 10.6
  def foldLeft[A, B](init: B, as: List[A])(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => f(a, _))(init)

  def foldRight[A, B](as: List[A], init: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f(a, _))(init)

  // ex 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length <= 0) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (left, right) = as.splitAt(as.length / 2)

      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  // ex 10.8
  /*
  import io.github.facaiy.c7.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] =
      for {
        a <- a1
        b <- a2
      } yield m.op(a, b)

    override def zero: Par[A] = unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(f andThen unit)
    */

  // ex 10.9
  sealed trait Container[+A] {
    def map[B](f: A => B): Container[B] = flatMap(x => Fill(f(x)))

    def flatMap[B](f: A => Container[B]): Container[B] = this match {
      case Fill(x) => f(x)
      case Empty => Empty
      case Nan => Nan
    }

    def merge[A1 >: A](that: Container[A1])(f: (A, A1) => Container[A1]): Container[A1] =
      (this, that) match {
        case (Nan, _) => Nan
        case (_, Nan) => Nan
        case (Empty, x) => x
        case (x, Empty) => x
        case (Fill(a), Fill(b)) => f(a, b)
      }
  }
  case class Fill[+A](get: A) extends Container[A]
  case object Empty extends Container[Nothing]
  case object Nan extends Container[Nothing]

  def isOrdered(as: List[Int]): Boolean = {
    val m = new Monoid[Container[Int]] {
      def op(a1: Container[Int], a2: Container[Int]): Container[Int] =
        a1.merge(a2)((a, b) => if (a < b) Fill(b) else Nan)

      override def zero: Container[Int] = Empty
    }

    foldMap(as, m)(x => Fill(x)) match {
      case Fill(_) => true
      case _ => false
    }
  }

  def isOrdered2(as: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int)]] {
      override def op(a1: Option[(Int, Int)],
                      a2: Option[(Int, Int)]): Option[(Int, Int)] =
        for {
          a <- a1
          b <- a2 if a._2 <= b._1
        } yield (a._1, b._2)

      override def zero: Option[(Int, Int)] = throw new NotImplementedError
    }

    foldMapV(as, m)(x => Some((x, x)))
      .exists(x => x._1 < x._2)
  }
}
