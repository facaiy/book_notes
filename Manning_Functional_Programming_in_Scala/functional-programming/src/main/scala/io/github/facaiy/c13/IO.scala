package io.github.facaiy.c13

import io.github.facaiy.c11.Monad

/**
 * Created by facai on 6/15/17.
 */
sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] { def run: B = f(self.run) }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run: B = f(self.run).run }
}

object IO extends Monad[IO] {
  def apply[A](a: => A): IO[A] = unit(a)

  override def unit[A](a: => A): IO[A] = new IO[A] { def run: A = a }

  override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa flatMap f
}

object Example {
  def ReadLine: IO[String] = IO { scala.io.StdIn.readLine }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def converter: IO[Unit] = for {
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
  val readInts = IO.product(readInt, readInt)
  // readInts.flatMap{ case (x, y) => PrintLine(s"$x, $y") }.run

  IO.forever(PrintLine("Still going...")).run
}
