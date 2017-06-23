package io.github.facaiy.fp.scala.c9

import io.github.facaiy.fp.scala.c9.MyParser.Parser

import scala.util.matching.Regex

/**
 * Created by facai on 5/27/17.
 */
object MyParser {
  type Parser[+A] = Location => Result[A]
}

object MyParsers extends Parsers[Parser] {
  // ex 9.13
  override def string(s: String): Parser[String] =
    loc =>
      if (loc.input.startsWith(s)) Success(loc.input, s.length)
      else Failure(loc.toError(s"Expected: $s"))

  override def regex(r: Regex): Parser[String] =
    loc =>
      r.findFirstIn(loc.input) match {
        case Some(s) => Success(loc.input, s.length)
        case None => Failure(loc.toError(s"Regex failed: ${r.toString}"))
      }

  override def slice[A](p: Parser[A]): Parser[String] = p.map {
    case xs: Seq[_] => xs mkString ","
    case x => x.toString
  }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.push(loc, msg))


  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  // ex 9.14

  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit


  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    s => s1(s) match {
      case Failure(_, false) => s2(s)
      case r => r
    }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(a, n) => f(a)(s.advanceBy(n))
                            .addCommit(n != 0)
                            .advanceSuccess(n)
      case r @ Failure(_, _) => r
    }

  // ex 9.15
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input, 0)) match {
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)
    }
}
