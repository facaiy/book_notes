package io.github.facaiy.c13

import io.github.facaiy.c11.Monad
import io.github.facaiy.c7.Par
import io.github.facaiy.c7.Par.Par

/**
 * Created by facai on 6/16/17.
 */
sealed trait Async[A] { self =>
  import Async._

  def map[B](f: A => B): Async[B] =
    flatMap(x => Return(f(x)))

  def flatMap[B](f: A => Async[B]): Async[B] =
    FlatMap(this, f)
}

object Async extends Monad[Async] {
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  def apply[A](a: => A): Async[A] = unit(a)

  override def unit[A](a: => A): Async[A] = Return(a)

  override def flatMap[A, B](fa: Async[A])(f: (A) => Async[B]): Async[B] = fa flatMap f

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](io: Async[A]): Par[A] = step(io) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => Par.flatMap(r)(a => run(unit(a)))
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(unit(a))))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }
}
