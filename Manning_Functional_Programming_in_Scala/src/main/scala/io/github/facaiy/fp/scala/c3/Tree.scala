package io.github.facaiy.fp.scala.c3

/**
 * Created by facai on 4/14/17.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /*
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(x) => 1
      case Branch(l, r) => size(l) + 1 + size(r)
    }
   */
  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + 1 + _)

  /*
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
    }
    */
  def maximum(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  /*
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
    }
    */
  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((x, y) => (x + 1) max (y + 1))

  /*
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(x => Leaf(f(x)))(Branch(_, _))

  def fold[A, B](t: Tree[A])(mapFunc: A => B)(comFunc: (B, B) => B): B =
    t match {
      case Leaf(x) => mapFunc(x)
      case Branch(l, r) => comFunc(fold(l)(mapFunc)(comFunc),
                                   fold(r)(mapFunc)(comFunc))
    }
}
