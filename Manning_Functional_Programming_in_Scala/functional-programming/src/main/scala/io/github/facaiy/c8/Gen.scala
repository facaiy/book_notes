package io.github.facaiy.c8

import java.util.concurrent.{ExecutorService, Executors}

import io.github.facaiy.c5.Stream
import io.github.facaiy.c6.RNG.Simple
import io.github.facaiy.c6.{RNG, RNG2, State}
import io.github.facaiy.c6.RNG2.Rand
import io.github.facaiy.c7.Par.Par
import io.github.facaiy.c8.Prop._

/**
 * Created by facai on 5/11/17.
 */

/**
 * ex 8.1
 * sum: List[Int] = Int
 * 1. sum(list) = sum(list.reverse)
 * 2. sum(List.fill(x)(n)) = x * n
 * 3. sum(x) = x
 * 4. sum(List.empty) = 0
 * 5. sum(x if all x < 0) < 0
 */

/**
 * ex 8.2
 * max: List[Int] = Int
 * 1. max(list) = max(list.reverse)
 * 2. max(List.fill(x)(n)) = x
 * 3. max(x) = x
 * 4. max(x1, x2) = x1 max x2
 * 5. max(xs) in xs
 * 6. max(List.empty): not defined.
 */

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // ex 8.9
  def &&(p: Prop): Prop = map2(p)((r1, r2) => if (r1().isFalsified) r1() else r2())
  
  def ||(p: Prop): Prop = map2(p)((r1, r2) => if (!r1().isFalsified) r1() else r2())
  
  def map2(p: Prop)(f: (() => Result, () => Result) => Result): Prop = Prop {
    (m, n, rng) => {
      lazy val r1 = run(m, n, rng)
      lazy val r2 = p.run(m, n, rng)
      
      f(() => r1, () => r2)
    }
  }
}

object Prop {
  type TestCases = Int
  
  type FailedCase = String
  type SuccessCount = Int
  
  type MaxSize = Int
  
  /*
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = {
    val g =
      (n: TestCases, r: RNG) => {
        val samples = a.listOfN(Gen.unit(n)).get(r)
        
        val res = samples.map(x => (f(x), x))
        
        val failedCases = res
          .filter(_._1 == false)
          .map(x => s"${x._1}: ${x._2}")
        
        val failedCount = failedCases.size
        val failure = failedCases.mkString("\n")
        
        if (failedCount > 0) {
          Falsified(failure, n - failedCount)
        } else {
          Passed
        }
      }
    
    Prop(g)
  }
  */
  
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)
  
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) =>
      val casesPerSize = (n + (m - 1)) / m
      val props: Stream[Prop] =
        Stream.from(0).take((n min m) + 1).map(i => Prop.forAll(g(i))(f))
      val prop: Prop =
        props.map(p =>
          Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
      
      prop.run(m, n, rng)
  }
  
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    Prop{
      (_, n, rng) =>
        randomStream(a)(rng).zipAll(Stream.from(0)).map{
          case (Some(x), Some(i)) =>
            try {
              if (f(x)) Passed else Falsified(x.toString, i)
            } catch {
              case e: Exception => Falsified(buildMsg(x, e), i)
            }
        }
        .take(n)
        .filter(_.isFalsified == true)
        .headOption
        .getOrElse(Passed)
    }
  
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.next(rng)))
  
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }
  
  // sec 8.4.2
  def check(p: => Boolean): Prop = Prop {
    (_, _, _) =>
      try {
        if (p) Proved else Falsified("()", 0)
      } catch {
        case e: Exception => Falsified(buildMsg("()", e), 0)
      }
  }
  
  // ex 8.15
  def check[A](candidates: Seq[A])(f: A => Boolean): Prop = {
    val sGen = candidates.zipWithIndex.map{ case (x, i) =>
      i -> Gen.unit(x)
    }.toMap
    
    Prop { (_, _, rng) =>
      forAll(sGen)(f).run(candidates.size, 1, rng) match {
        case Passed => Proved
        case x => x
      }
    }
  }
  
  def checkPar(p: => Par[Boolean]): Prop = {
    val threadPools: Seq[ExecutorService] =
      Executors.newCachedThreadPool :: Range(1, 4).map(Executors.newFixedThreadPool).toList
    check(threadPools)(es => p(es).get)
  }
  
  val threadPools: Gen[ExecutorService] = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )
  
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(g ** threadPools){ case x ** es => f(x)(es).get }
  
  def forAllPar[A](g: SGen[A])(f: A => Par[Boolean]): Prop =
    forAll(x => g.forSize(x) ** threadPools){ case x ** es => f(x)(es).get }
}

object ** {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}


case class Gen[+A](sample: Rand[A]) {
  import Gen._
  
  def next(r: RNG): (A, RNG) = sample.run(r)
  
  def get(r: RNG): A = next(r)._1
  
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  
  // ex 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val s: Rand[B] = sample.flatMap(x => f(x).sample)
    Gen(s)
  }
  
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => sequence(List.fill(s)(this)))
  
  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  
  // ex8.10
  def unsized: SGen[A] = SGen(_ => this)
  
  // sec 8.4.2
  def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))
}

object Gen {
  // ex 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val s = RNG2.nonNegativeLessThan(stopExclusive - start).map(_ + start)
    Gen(s)
  }
  
  // ex 8.5
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))
  
  def sequence[A](ls: List[Gen[A]]): Gen[List[A]] =
    ls.foldRight(unit(List.empty[A]))((x, acc) => x.map2(acc)(_ :: _))
  
  def boolean: Gen[Boolean] = choose(0, 2).map(x => if (x == 1) true else false)
  
  // def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = sequence(List.fill(n)(g))
  
  // sec 8.2.3
  /*
  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val g1 = choose(start, stopExclusive)
    val g2 = choose(start, stopExclusive)
    
    g1.map2(g2)((_, _))
  }
  
  Gen[Option[A]].map(_.getOrElse())
  Gen[A].map(Some(A))
  
  Gen[String]: use choose to generate ASCII code.
  */
  
  // ex 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)
  
  // ex 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    assert(g1._2 > 0 && g1._2 < 1)
    assert(g2._2 > 0 && g2._2 < 1)
    
    val (gMin, gMax) = if (g1._2 < g2._2) (g1, g2) else (g2, g1)
    
    val ratio = gMin._2 / (gMax._2 + gMin._2)
    
    val threshold = (ratio * Int.MaxValue).floor.toInt
    val total = (threshold / ratio).toInt
    assert(threshold > 0 && total > threshold)
    
    choose(0, total)
    .flatMap{ x =>
      if (x <= threshold) gMin._1
      else gMax._1
    }
  }
  
  // ex 8.19
  /* use hashcode to create a RNG */
  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    Gen.unit(s => g.get(Simple(s.hashCode)))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  // ex 8.11
  def map[B](f: A => B): SGen[B] = SGen(n => forSize(n).map(f))
}

object SGen {
  // ex 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(Gen.unit(n)))
  
  // ex 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(Gen.unit(n + 1)))
}
