package io.github.facaiy.c7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import language.implicitConversions

/**
 * Created by facai on 5/5/17.
 */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val a = pa(es)
      val b = pb(es)
      UnitFuture(f(a.get, b.get))
    }
  // ex 7.3
  def map2[A, B, C](timeout: Option[Long], units: TimeUnit)
                   (pa: Par[A], pb: Par[B])
                   (f: (A, B) => C): Par[C] =
    (timeout, units) match {
      case (None, _) => map2(pa, pb)(f)
      case (Some(t), u @ TimeUnit.NANOSECONDS) => es =>
        val st = System.nanoTime()
        val aa = pa(es).get(t, u)
        val leftTime = System.nanoTime() - st
        val bb = pb(es).get(leftTime, u)

        UnitFuture(f(aa, bb))
      case (_, u) => throw new UnsupportedOperationException(u.toString)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  // ex 7.4
  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit())((a, _) => f(a))

  // ex 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((x, acc) => map2(x, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val os: Par[List[Option[A]]] = parMap(as)(x => if (f(x)) Some(x) else None)

    map(os)(_.flatten)
  }

  // sec 7.4.3
  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // ex 7.11
  /*
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)
      */
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(x => if (x) t else f)

  /*
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(n(es).get)(es)
    */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices.apply)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(choices.apply)

  // ex 7.13
  /*
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => f(a(es).get)(es)
    */
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  // ex 7.14
  def join[A](a: Par[Par[A]]): Par[A] = es => a(es).get()(es)

  def joinUseFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

  def map2UseFlatmap[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    flatMap(pa)(a => flatMap(pb)(b => unit(f(a, b))))
  
  // sec 8.4.2
  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p1, p2)(_ == _)
}

object Examples {
  import Par._

  /*
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1) {
      unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)

      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }
    */

  // sec 7.3
  def sum(ints: IndexedSeq[Int]): Par[Int] = fold(ints)(0, x => x)(_ + _)

  def fold[A, B](as: IndexedSeq[A])(default: B, mapFunc: A => B)(comFun: (B, B) => B): Par[B] =
    if (as.length <= 1) {
      val res = as.headOption.map(mapFunc).getOrElse(default)
      unit(res)
    }
    else {
      val (l, r) = as.splitAt(as.length / 2)

      map2(fork(fold(l)(default, mapFunc)(comFun)),
           fork(fold(r)(default, mapFunc)(comFun)))(comFun)
    }

  def max(ints: IndexedSeq[Int]): Par[Int] = fold(ints)(Int.MinValue, x => x)(_ max _)

  def count(ps: List[String]): Par[Int] = fold(ps.toArray[String])(0, _.split("\\s+").length)(_ + _)

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val pab: Par[(A, B)] = map2(pa, pb)((_, _))

    map2(pab, pc){ case ((a, b), c) => f(a, b, c) }
  }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])
                         (f: (A, B, C, D) => E): Par[E] = {
    val pab: Par[(A, B)] = map2(pa, pb)((_, _))
    val pcd: Par[(C, D)] = map2(pc, pd)((_, _))

    map2(pab, pcd){ case ((a, b), (c, d)) => f(a, b, c, d) }
  }

  // ex 7.7
  /**
   * map(y)(id) == y
   * map(unit(x))(f) == unit(f(x))
   * map(map(y)(g))(f) == map(g(y))(f) == f(g(y)) == (f compose g)(y) == map(y)(f compose g)
   */
}