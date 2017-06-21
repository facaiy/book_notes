package io.github.facaiy.c13

import io.github.facaiy.c11.Monad

/**
 * Created by facai on 6/15/17.
 */
sealed trait TailRec[A] { self =>
  import TailRec._

  def map[B](f: A => B): TailRec[B] =
    flatMap(x => Return(f(x)))

  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)
}

object TailRec extends Monad[TailRec] {
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  def apply[A](a: => A): TailRec[A] = unit(a)

  override def unit[A](a: => A): TailRec[A] = Suspend(() => a)

  override def flatMap[A, B](fa: TailRec[A])(f: (A) => TailRec[B]): TailRec[B] = fa flatMap f

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

object Example {
  def ReadLine: TailRec[String] = TailRec { scala.io.StdIn.readLine }

  def PrintLine(msg: String): TailRec[Unit] = TailRec { println(msg) }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def converter: TailRec[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
}

object Main extends App {
  import Example._

  // converter.run

  val echo = ReadLine.flatMap(PrintLine)
  // echo.run

  val readInt = ReadLine.map(_.toInt)
  val readInts = TailRec.product(readInt, readInt)
  // readInts.flatMap{ case (x, y) => PrintLine(s"$x, $y") }.run

  TailRec.run(TailRec.forever(PrintLine("Still going...")))
}
