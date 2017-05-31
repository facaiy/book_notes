package io.github.facaiy.c9

import io.github.facaiy.c8.{Gen, Prop}
import io.github.facaiy.c8.Prop._

import scala.util.matching.Regex

/**
 * Created by facai on 5/23/17.
 */
trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // ex 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = sequence(List.fill(n)(p))

  def sequence[A](ls: List[Parser[A]]): Parser[List[A]] =
    ls.foldRight(succeed(List.empty[A]))((x, acc) => x.map2(acc)(_ :: _))

  
  // ex 9.8
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(x => succeed(f(x)))

  // ex 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) or succeed(List())

  def slice[A](p: Parser[A]): Parser[String]

  // ex 9.7
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      x <- p
      y <- p2
    } yield (x, y)

  // ex 9.1
  /*
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2).map(f.tupled)
    */
  // ex 9.7
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      x <- p
      y <- p2
    } yield f(x, y)

  def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(p.many)(_ :: _)

  // ex 9.5

  // sec 9.3
  // product(fa, fb) is context-free, namely, input s is parsed by fa(s) and fb(s) independently.
  // while in context-sensitive case, fb(fa(s)) is required.

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def regex(r: Regex): Parser[String]

  // ex 9.9
  // def token[A](p: Parser[A]): Parser[A]

  def whitespace: Parser[String] = regex("\\s*".r)

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)
                                (implicit f: A => Parser[String]
                                ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p.map(x => x), p)(in)

    // ex 9.2
    def produceLaw[A, B](p: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      forAll(in)(s => run(p ** p2)(s) == (run(p)(s), run(p2)(s)))
  }

  object Example {
    // ex 9.6
    def digitFollowByA: Parser[String] =
      for {
        h <- regex("[0-9]".r)
        t <- char('a').many.slice
      } yield h + t
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == "\n") + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf("\n") match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))
  
  def advanceBy(n: Int): Location = copy(offset = offset + n)
}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    copy(stack.lastOption.map(l => (l._1, s)).toList)

  // ex 9.16
  override def toString: String = stack.sortBy(_._1.offset) mkString "\n"
}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, x) => Failure(f(e), x)
    case _ => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, false)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, m + n)
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError,
                   isCommitted: Boolean = true) extends Result[Nothing]
