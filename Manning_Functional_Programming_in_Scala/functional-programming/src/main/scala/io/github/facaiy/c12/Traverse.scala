package io.github.facaiy.c12

import io.github.facaiy.c10.{Foldable, Monoid}
import io.github.facaiy.c11.{Functor, Monad}
import io.github.facaiy.c6.State

/**
 * Created by facai on 6/12/17.
 */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  import Traverse._

  // ex 12.14
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
                           implicit G: Applicative[G]): G[F[B]] =
    sequence[({type f[x] = G[x]})#f, B](map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(
                        implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  import Applicative.optionApplitive
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Option, A, B](fa)(a => Some(f(a)))(optionApplitive).get

  override def foldMap[A, M](as: F[A])(f: (A) => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad2)

  import State.{get, set}

  def mapAccum[S, A, B](fa: F[A], s: S)(f:  (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, i) => ((a, i), i + 1))._1

  // ex 12.16
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  // ex 12.17
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Imcompatible shapes")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    map(zipL(fb, fa))(_.swap)

  // ex 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (G: Applicative[G], H: Applicative[H]):
                            (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  // ex 12.19
  def compose[G[_]](implicit G: Traverse[G]) = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[H[_], A, B](fa: F[G[A]])(f: (A) => H[B])(implicit H: Applicative[H]): H[F[G[B]]] =
      self.traverse(fa)(a => G.traverse(a)(f))
  }

  // ex 12.20
  def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]) =
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      override def flatMap[A, B](fga: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] = {
        F.flatMap(fga){ ga =>
          val gfgb: G[F[G[B]]] = G.map(ga)(a => f(a))
          val fggb: F[G[G[B]]] = T.sequence(gfgb)(F)
          val fgb = F.map(fggb)(ggb => G.join(ggb))
          fgb
        }
      }
    }
}

object Traverse {
  import Applicative._

  // ex 12.13
  val listTraverse = new Traverse[List] {
    /*
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B]))((a, ls) => G.map2(f(a), ls)(_ :: _))
      */

    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  val optionTraverse = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Tree(head, tail) => Tree(f(head), tail.map(t => map(t)(f)))
    }
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): Const[M, A] = M.zero
      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = M.op(fa, fb)
    }
}