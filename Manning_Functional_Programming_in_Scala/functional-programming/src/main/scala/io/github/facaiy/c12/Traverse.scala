package io.github.facaiy.c12

import io.github.facaiy.c11.Functor

/**
 * Created by facai on 6/12/17.
 */
trait Traverse[F[_]] extends Functor[F] {
  // ex 12.14
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))(G)

  def sequence[G[_]: Applicative, A](fga: F[G[A]])(G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)(G)

  import Applicative.optionApplitive

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Option, A, B](fa)(a => Some(f(a)))(optionApplitive).get
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
}