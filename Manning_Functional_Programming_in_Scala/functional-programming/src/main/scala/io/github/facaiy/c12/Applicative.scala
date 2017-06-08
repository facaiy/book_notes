package io.github.facaiy.c12

import io.github.facaiy.c11.Functor

/**
 * Created by facai on 6/8/17.
 */
trait Applicative[F[_]] extends Functor[F] {
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
}