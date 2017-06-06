package io.github.facaiy.c11

import io.github.facaiy.c6.State
import io.github.facaiy.c8.Gen

/**
 * Created by facai on 6/6/17.
 */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  
  // ex 11.8
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)()
  
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
  
  // ex 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((x, y) => map2(x, y)(_ :: _))
  
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la map f)
  
  // ex 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))
  // ex 11.5
  // replicate the element in the data structure.
  
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
  
  // ex 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val ls: List[F[Option[A]]] = ms.map(x => map(f(x))(b => if (b) Some(x) else None))
    
    map(sequence(ls))(_.flatten)
  }
  
  // ex 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
  
  // ex 11.9
  /**
   * Given: x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
   * Proof: compose(compose(h, f), g) == compose(h, compose(f, g))
   *
   * let h(x) = () => x
   * x.flatMap(f).flatMap(g) = flatMap(flatMap(x)(xx => f(xx)))(yy => g(yy))
   *                         = flatMap(compose(() => x)(f)())(yy => g(yy))
   *                         = compose(compose(() => x)(f))(g)()
   *                         = compose(compose(h)(f))(g)()
   * x.flatMap(a => f(a).flatMap(g)) = x.flatMap(a => compose(f)(g)(a))
   *                                 = compose(() => x)(compose(f)(g))()
   *                                 = compose(h)(compose(f)(g))()
   * hence: both sides are equivalent.
   */
}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    
    override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa flatMap f
  }
  
  // ex 11.1
  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
  
    override def flatMap[A, B](ls: List[A])(f: (A) => List[B]): List[B] = ls flatMap f
  }
  
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
  
    override def flatMap[A, B](op: Option[A])(f: (A) => Option[B]): Option[B] = op flatMap f
  }
  
  // ex 11.2
  def stateMonad[S] = {
    type State1[A] = State[S, A]
    
    new Monad[State1] {
      override def unit[A](a: => A): State1[A] = State.unit(a)
  
      override def flatMap[A, B](fa: State1[A])(f: (A) => State1[B]): State1[B] = fa flatMap f
    }
  }
}