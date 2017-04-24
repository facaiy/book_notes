package io.github.facaiy.c4

import scala.{Option => _, Some => _, Either => _, _}

/**
 * Created by facai on 4/20/17.
 */
sealed trait Option[+A] {
  // ex 4.1
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(x) => x
      case None => default
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap{ x =>
    if (f(x)) Some(x)
    else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // ex 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap{ m =>
      val rs = xs.map(x => math.pow(x - m, 2))
      mean(rs)
    }

  // ex 4.3
  /*
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap{ x =>
      b.map{ y =>
        f(x, y)
      }
    }
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // ex 4.4
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}

  /*
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List.empty[A])){ (x, z) =>
      map2(x, z)(_ :: _)
    }
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil)) { (x, z) =>
      for {
        xx <- f(x)
        list <- z
      } yield xx :: list
    }
}
