package io.github.facaiy.c9

import io.github.facaiy.c8.{Gen, Prop}
import io.github.facaiy.c8.Prop._

/**
 * Created by facai on 5/23/17.
 */
trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  
  implicit def string(s: String): Parser[String]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  
  // ex 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = sequence(List.fill(n)(p))
  
  def sequence[A](ls: List[Parser[A]]): Parser[List[A]] =
    ls.foldRight(succeed(List.empty[A]))((x, acc) => x.map2(acc)(_ :: _))
  
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  
  // ex 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    p.map2(p.many)(_ :: _) or succeed(List())
  
  def slice[A](p: Parser[A]): Parser[String]
  
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
  
  // ex 9.1
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2).map(f.tupled)
  
  def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(p.many)(_ :: _)
  
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)
                                (implicit f: A => Parser[String]
                                ): ParserOps[String] = ParserOps(f(a))
  
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
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
  
  // ex 9.5
}
