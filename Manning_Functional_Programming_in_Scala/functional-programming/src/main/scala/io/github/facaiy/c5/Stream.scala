package io.github.facaiy.c5

/**
 * Created by facai on 4/26/17.
 */
sealed trait Stream[+A] {
  import Stream._

  /*
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  */

  // ex 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // ex 5.2
  /*
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }
    */
  def take(n: Int): Stream[A] = unfold((n, this)){
    case (n, _) if n <= 0 => None
    case (_, Empty) => None
    case (n, Cons(h, t)) => Some(h(), (n - 1, t()))
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(n: Int, xs: Stream[A]): Stream[A] =
      if (n <= 0) xs
      else xs match {
        case Empty => Empty
        case Cons(h, t) => loop(n - 1, t())
      }

    loop(n, this)
  }

  // ex 5.3
  /*
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t().takeWhile(p))
      else Empty
  }
  */

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((x, z) => p(x) || z)

  // ex 5.4
  def forAll(p: A => Boolean): Boolean = !exists(x => !p(x))

  // ex 5.5
  /*
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((x, z) =>
      if (p(x)) Cons(() => x, () => z) else Stream.empty)
      */
  def takeWhile(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  // ex 5.6
  def headOption: Option[A] = foldRight(Option.empty[A])((x, z) => Some(x))

  // ex 5.7
  /*
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((x, z) => Stream.cons(f(x), z))
    */
  def map[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((x, z) => if (f(x)) cons(x, z) else z)

  def append[B >: A](e: => B): Stream[B] = concat(cons(e, empty[B]))

  def concat[B >: A](as: Stream[B]): Stream[B] = foldRight(as)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((x, z) => f(x).concat(z))

  // ex 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)){
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  // ex 5.14
  /*
  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).foldRight(true){
    case ((_, p0), _) if p0.isEmpty => true
    case ((s0, p0), z) if s0 == p0 => z
    case _ => false
  }
  */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s)
      .takeWhile(!_._2.isEmpty)
      .forAll(x => x._1 == x._2)

  // ex 5.15
  def tails: Stream[Stream[A]] = unfold(this){
    case s @ Cons(h, t) => Some(s, t())
    case _ => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // ex 5.16
  /*
  def scanRight[B](z: => B)(f: (A, B) => B): Stream[B] = this match {
    case Cons(h, t) =>
      val as = t().scanRight(z)(f)
      cons(f(h(), as.headOption.getOrElse(throw new NoSuchElementException)), as)
    case _ => cons(z, empty)
  }
  */
  def scanRight[B](z: => B)(f: (A, B) => B): Stream[B] =
    foldRight(cons(z, empty)){ case (x, z) =>
      val e = f(x, z.headOption.getOrElse(throw new NoSuchElementException))
      cons(e, z)
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val header = h
    lazy val tail = t

    Cons(() => header, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // ex 5.8
  /*
  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = cons(a, c)

    c
  }
  */
  def constant[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  // ex 5.9
  // def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  // ex 5.10
  /*
  def fibs: Stream[Int] = {
    def next(x0: Int, x1: Int): Stream[Int] = cons(x0, next(x1, x0 + x1))

    next(0, 1)
  }
  */
  def fibs: Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  // ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  // ex 5.13
}
