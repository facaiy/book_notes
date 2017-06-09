package io.github.facaiy.c12

import io.github.facaiy.c11.{Functor, Monad}

/**
 * Created by facai on 6/8/17.
 */
trait Applicative[F[_]] extends Functor[F] { self =>
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))
  def unit[A](a: => A): F[A]

  // ex 12.2
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit[A => B => C](f.curried))(fa))(fb)

  // ex 12.3
  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit[A => B => C => D](f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A],
                       fb: F[B],
                       fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit[A => B => C => D => E](f.curried))(fa))(fb))(fc))(fd)

  /*
  def map[A, B](fa: F[A])(f: A => B): F[B] =
     map2(fa, unit(()))((a, _) => f(a))
   */
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit[A => B](f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // ex 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // ex 12.4
  /**
   * sequence: (a1, b1, c1 ...), (a2, b2, c2, ...), ...
   */

  // ex 12.8
  def product[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

    override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
      (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
  }

  // ex 12.9
  def compose[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def apply[A, B](fab: F[G[(A) => B]])(fa: F[G[A]]): F[G[B]] =
      self.map2(fab, fa)((gab, ga) => G.apply(gab)(ga))
  }
}

object Applicative {
  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  // ex 12.6
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def apply[A, B](fab: Validation[E, (A) => B])(fa: Validation[E, A]): Validation[E, B] =
      (fab, fa) match {
        case (Success(f), Success(a)) => Success(f(a))
        case (Failure(h0, t0), Failure(h1, t1)) => Failure(h0, t0 ++: h1 +: t1)
        case (_, e @ Failure(_, _)) => e
        case (e @ Failure(_, _), _) => e
      }
  }
}