package io.github.facaiy.c13

import io.github.facaiy.c11.Monad

/**
 * Created by facai on 6/16/17.
 */
sealed trait Free[F[_], A] {
  import Free._

  // ex 13.1
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] =
    flatMap(x => unit(f(x)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A],
                               f: A => Free[F, B]) extends Free[F, B]

object Free { self =>
  type TailRec[A] = Free[Function0, A]

  import io.github.facaiy.c7.Par.Par
  type Async[A] = Free[Par, A]

  // ex 13.1
  def unit[F[_], B](a: B): Free[F, B] = Return(a)

  def freeMonda[F[_]] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] =  self.unit(a)

    override def flatMap[A, B](fa: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = fa flatMap f
  }

  // ex 13.2
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(s, f) => s match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(g) => runTrampoline(f(g()))
      case FlatMap(x, g) => runTrampoline(x flatMap (a => g(a) flatMap f))
    }
  }

  // ex 13.3
  @annotation.tailrec
  def step[F[_], A](s: Free[F, A]): Free[F, A] = s match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => s
  }

  def run[F[_], A](s: Free[F, A])(implicit F: Monad[F]): F[A] = step(s) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => F.flatMap(r)(a => run(unit(a)))
    case FlatMap(x, f) => x match {
      case Suspend(r) => F.flatMap(r)(a => run(f(unit(a))))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }
}
