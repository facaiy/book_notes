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

  // ex 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2
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
    foldMap(as, endoMonoid[B])(a => f(a, _))(init)
}
