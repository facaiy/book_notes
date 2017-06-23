package io.github.facaiy.fp.scala.c4

// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, Left => _, Right => _, _}

/**
 * Created by facai on 4/24/17.
 */
sealed trait Either[+E, +A] {
  // ex 4.6
  /*
  def map[B](f: A => B): Either[E, B] =
    this match {
      case x @ Left(_) => x
      case Right(x) => Right(f(x))
    }
    */
  def map[B](f: A => B): Either[E, B] = flatMap(x => Right(f(x)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case x @ Left(_) => x
      case Right(x) => f(x)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case x @ Right(_) => x
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap{ aa =>
      b.map(bb => f(aa, bb))
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends  Either[Nothing, A]

object Either {
  // ex 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])
                       (f: A => Either[E, B]): Either[E, List[B]] =
    // TODO: use shortcut ?
    as.foldRight[Either[E, List[B]]](Right(Nil)) { (x, z) =>
      for {
        xx <- f(x)
        zz <- z
      } yield xx :: zz
    }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) {
      Left("mean of empty list!")

    } else {
      Right(xs.sum / xs.length)
    }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  def Try[A](a: => A): Either[Exception, A] =
    try {
      Right(a)

    } catch {
      case e: Exception => Left(e)
    }
}


// ex 4.8
object EitherPlus {
  type EitherPlus[E, A] = Either[List[E], A]

  def LeftPlus[E](value: E): EitherPlus[E, Nothing] = Left(List(value))

  def RightPlus[A](value: A): EitherPlus[Nothing, A] = Right(value)

  def map2[A, B, C, E, EE >: E](a: EitherPlus[E, A], b: EitherPlus[EE, B])
                               (f: (A, B) => C): EitherPlus[EE, C] =
    (a, b) match {
      case (Left(x1), Left(x2)) => Left(x1 ::: x2)
      case (x @ Left(_), _) => x
      case (_, x @ Left(_)) => x
      case (Right(x1), Right(x2)) => Right(f(x1, x2))
  }
}
