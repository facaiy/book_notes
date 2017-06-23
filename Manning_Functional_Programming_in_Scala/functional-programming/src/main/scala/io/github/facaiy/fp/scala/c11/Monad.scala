package io.github.facaiy.fp.scala.c11

import io.github.facaiy.fp.scala.c12.Applicative
import io.github.facaiy.fp.scala.c6.State
import io.github.facaiy.fp.scala.c8.Gen

/**
 * Created by facai on 6/6/17.
 */
trait Monad[F[_]] extends Applicative[F] { self =>
  def unit[A](a: => A): F[A]

  // ex 11.8
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)()

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // ex 11.3
  override def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((x, y) => map2(x, y)(_ :: _))

  override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la map f)

  // ex 11.4
  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))
  // ex 11.5
  // replicate the element in the data structure.

  override def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // ex 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val ls: List[F[Option[A]]] = ms.map(x => map(f(x))(b => if (b) Some(x) else None))

    map(sequence(ls))(_.flatten)
  }

  // ex 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // ex 12.11
  def compose[G[_]](G: Monad[G]) = new Monad[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def flatMap[A, B](fa: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] =
    // F[G[]] cannot be flatten.
      throw new UnsupportedOperationException()
  }

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

  // ex 11.10
  /**
   * compose(f, unit) = compose(f: A => F[B], unit: B => F[B]): A => F[B]
   * compose(unit, f) = compose(unit: A => F[A], f: A => F[B]): A => F[B]
   */

  // ex 11.12
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(x => x)

  // ex 11.13
  def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))


  def forever[A, B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)

    flatMap(a)(_ => t)
  }
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

  def stateMonad2[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
  }

  // ex 11.17
  case class Id[A](value: A) {
    // ex 11.17
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val IdMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = fa flatMap f
  }

  // ex 11.20
  /**
   * sequence: List[Reader[R, A]] => Reader[R, List[A]], namely, r => List(a, b, ...)
   */

  // ex 12.5
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = fa match {
      case Left(e) => Left(e)
      case Right(x) => f(x)
    }
  }
}