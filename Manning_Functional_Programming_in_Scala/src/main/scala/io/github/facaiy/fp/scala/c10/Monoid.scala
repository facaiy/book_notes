package io.github.facaiy.fp.scala.c10

import io.github.facaiy.fp.scala.c8.{Gen, Prop}

/**
 * Created by facai on 5/31/17.
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}


object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  // ex 10.1
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  // ex 10.2
  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  // ex 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    override def zero = (x: A) => x
  }

  // ex 10.4
  /*
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associative: Gen[Boolean] = for {
      a <- gen
      b <- gen
      c <- gen
    } yield m.op(m.op(a, b), c) == m.op(a, m.op(b, c))

    val identity: Gen[Boolean] = gen.map(x =>
      m.op(x, m.zero) == x && x == m.op(m.zero, x))

    Prop.forAll(identity)(x => x) && Prop.forAll(associative)(x => x)
  }
  */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associative = Prop.forAll(
        for {
          a <- gen
          b <- gen
          c <- gen
        } yield (a, b, c)
      ){ case (a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c))}
    
    val identity = Prop.forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && a == m.op(m.zero, a))
    
    associative && identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // ex 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
   // as.map(f).foldLeft(m.zero)(m.op)
   as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // ex 10.6
  def foldLeft[A, B](init: B, as: List[A])(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => f(a, _))(init)

  def foldRight[A, B](as: List[A], init: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f(a, _))(init)

  // ex 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length <= 0) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (left, right) = as.splitAt(as.length / 2)

      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  // ex 10.8
  /*
  import io.github.facaiy.fp.scala.c7.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] =
      for {
        a <- a1
        b <- a2
      } yield m.op(a, b)

    override def zero: Par[A] = unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(f andThen unit)
    */

  // ex 10.9
  /*
  sealed trait Container[+A] {
    def map[B](f: A => B): Container[B] = flatMap(x => Fill(f(x)))

    def flatMap[B](f: A => Container[B]): Container[B] = this match {
      case Fill(x) => f(x)
      case Empty => Empty
      case Nan => Nan
    }

    def merge[A1 >: A](that: Container[A1])(f: (A, A1) => Container[A1]): Container[A1] =
      (this, that) match {
        case (Nan, _) => Nan
        case (_, Nan) => Nan
        case (Empty, x) => x
        case (x, Empty) => x
        case (Fill(a), Fill(b)) => f(a, b)
      }
  }
  case class Fill[+A](get: A) extends Container[A]
  case object Empty extends Container[Nothing]
  case object Nan extends Container[Nothing]

  def isOrdered(as: List[Int]): Boolean = {
    val m = new Monoid[Container[Int]] {
      def op(a1: Container[Int], a2: Container[Int]): Container[Int] =
        a1.merge(a2)((a, b) => if (a < b) Fill(b) else Nan)

      override def zero: Container[Int] = Empty
    }

    foldMap(as, m)(x => Fill(x)) match {
      case Fill(_) => true
      case _ => false
    }
  }

  def isOrdered2(as: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int)]] {
      override def op(a1: Option[(Int, Int)],
                      a2: Option[(Int, Int)]): Option[(Int, Int)] =
        for {
          a <- a1
          b <- a2 if a._2 <= b._1
        } yield (a._1, b._2)

      override def zero: Option[(Int, Int)] = throw new NotImplementedError
    }

    foldMapV(as, m)(x => Some((x, x)))
      .exists(x => x._1 < x._2)
  }
  */
  def isOrdered(as: List[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (a1, a2) match {
          case (Some((x1, y1, b1)), Some((x2, y2, b2))) =>
            Some((x1 min x2, y1 max y2, b1 && b2 && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
  
      override def zero: Option[(Int, Int, Boolean)] = None
    }
    
    foldMap(as, m)(x => Some(x, x, true)).map(_._3).getOrElse(true)
  }

  // ex 10.16
  def productMonoid[A, B](A: Monoid[A])(B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero){ (acc, k) =>
          acc.updated(k, V.op(a1.getOrElse(k, V.zero),
                              a2.getOrElse(k, V.zero)))
        }
      override def zero: Map[K, V] = Map[K, V]()
    }

  // ex 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(f1: (A) => B, f2: (A) => B): (A) => B = x => B.op(f1(x), f2(x))
    override def zero: (A) => B = _ => B.zero
  }

  // ex 10.18
  /*
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m: Monoid[A => Map[A, Int]] = functionMonoid(mapMergeMonoid(intAddition))

    as.foldLeft(m.zero) { (acc, x) =>
      m.op(acc, _ => Map(x -> 1))
    }(as.head)
  }
  */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

/*
object WC {
  // ex 10.10
  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(""), Stub("")) => Part("", 0, "")
      case (Stub(""), Stub(y)) => Part("", 0, y)
      case (Stub(x), Stub("")) => Part(x, 0, "")
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Stub(""), Part(_, w, r)) => Part("", w + 1, r)
      case (Stub(x), Part(l, w, r)) => Part(x + l, w, r)
      case (Part(l, w, _), Stub("")) => Part(l, w + 1, "")
      case (Part(l, w, r), Stub(y)) => Part(l, w, r + y)
      case (Part(l, w, ""), Part("", w2, r2)) => Part(l, w + w2, r2)
      case (Part(l, w, _), Part(_, w2, r2)) => Part(l, w + 1 + w2, r2)
    }

    override def zero: WC = Stub("")
  }

  // ex 10.11
  def str2wc(str: String): WC = Monoid.foldMapV(str, wcMonoid){ x =>
    if (x.isLetterOrDigit) Stub(x.toString)
    else Stub("")
  }

  def wordCount(str: String): Int = str2wc(str) match {
    case Stub("") => 0
    case Stub(_) => 1
    case Part("", w, "") => w
    case Part("", w, _) => w + 1
    case Part(_, w, "") => w + 1
    case Part(_, w, _) => w + 2
  }
}
*/
object WC {
  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Stub(x), Part(l, w, r)) => Part(x + l, w, r)
      case (Part(l, w, r), Stub(y)) => Part(l, w, r + y)
      case (Part(l, w, r), Part(l2, w2, r2)) =>
        Part(l, w + w2 + (if ((r + l2).isEmpty) 0 else 1), r2)
    }
  
    override def zero: WC = Stub("")
  }
  
  def str2wc(str: String): WC = Monoid.foldMapV(str, wcMonoid) { x =>
    if (x.isLetterOrDigit) Stub(x.toString)
    else Part("", 0, "")
  }
  
  def unstub(s: String) = s.length min 1
  
  def wordCount(str: String): Int = str2wc(str) match {
    case Stub(x) => unstub(x)
    case Part(l, w, r) => unstub(l) + w + unstub(r)
  }
}


trait Foldable[F[_]] {
  import Monoid.{dual, endoMonoid}

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(x => (y: B) => f(x, y))(endoMonoid)(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(x => (y: B) => f(y, x))(dual(endoMonoid))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((z, x) => mb.op(z, f(x)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // ex 10.15
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List.empty[A])(_ :: _)
}

// ex 10.12
object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
    concatenate(as.map(f))(mb)
}

// ex 10.13
// similar with ex 10.12

// ex 10.14
object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(z)((b, a) => f(a, b))

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.map(x => f(z, x)).getOrElse(z)

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B =
    as.map(f).getOrElse(mb.zero)
}
