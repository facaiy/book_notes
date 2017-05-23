package io.github.facaiy.c9

/**
 * Created by facai on 5/23/17.
 */
trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)
                                (implicit f: A => Parser[String]
                                ): ParserOps[String] = ParserOps(f(a))
  
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
  
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  
  def exist(c: Char): Parser[Boolean]
  def zeroOrMore(c: Char): Parser[Int] // = sequence(Stream(exist(c)).filter(_.value)).map(_.sum)
  def oneOrMore(c: Char): Parser[Int]
}
