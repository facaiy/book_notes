package io.github.facaiy.fp.scala.c8

import io.github.facaiy.fp.scala.c6.RNG.Simple
import io.github.facaiy.fp.scala.c7.{Examples, Par}
import org.scalatest.FunSuite

/**
 * Created by facai on 5/11/17.
 */
class GenSuite extends FunSuite {
  import Gen._
  import GenSuite._
  
  test("choose") {
    val gens = List.fill(20)(choose(5, 10))
  
    val s = Simple(1)
    val res = sequence(gens).get(s)

    assert(notAllAreSame(res))
    assert(res.forall(x => x >= 5 && x < 10))
  }
  
  test("unit") {
    val s = Simple(1)
    assert(unit(10).get(s) === 10)
  }
  
  test("boolean") {
    val gens = List.fill(20)(boolean)
    
    val s = Simple(1)
    val res = sequence(gens).get(s)
    
    assert(notAllAreSame(res))
  }
}

object GenSuite {
  def notAllAreSame[A](ss: Seq[A]): Boolean = ss.toSet.size > 1
}

class SGenSuite extends FunSuite {
  test("max") {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    
    Prop.run(maxProp)
  }
  
  // ex 8.14
  test("List.sorted") {
    val smallInt = Gen.choose(-10, 10)
    val sortProp =  Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      if (ns.size <= 1) {
        ns.sorted == ns
      } else {
        ns.sorted.sliding(2).forall(x => x(0) <= x(1))
      }
    }
    
    Prop.run(sortProp)
  }
  
  test("checkPar") {
    val prop = Prop.checkPar {
      Par.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }

    Prop.run(prop)
  }
  
  test("forAllPar") {
    val pint = Gen.choose(0, 10).map(Par.unit(_))
    val p4 = Prop.forAllPar(pint)(x => Par.equal(Par.map(x)(y => y), x))
    
    Prop.run(p4)
  }
  
  // ex 8.16
  test("richer generator for Par[Int]") {
    val smallInt = Gen.choose(0, 10)
    val ls = SGen.listOf1(smallInt).map(_.toArray)
    val prop = Prop.forAllPar(ls)(x => Par.equal(Examples.sum(x), Par.unit(x.sum)))
    
    // blockPar will get into trouble because of deadlock.
    Prop.run(prop, 2, 1)
  }
  
  // ex 8.17
  test("fork") {
    val prop = Prop.checkPar {
      Par.equal(
        Par.fork(Par.unit(1)),
        Par.unit(1)
      )
    }
    
    Prop.run(prop)
  }
  
  // ex 8.18
  /**
   * 1. s.takeWhile(f).forall(f) == true
   * 2. s.takeWhile(true) == s
   * 3. s.takeWhile(false) = empty
   * 4. s = s.takeWhile(f) :: s.dropWhile(f)
   */
}

class ExampleSuite extends FunSuite {
  // ex 8.20
  test("take") {
    val smallInt = Gen.choose(-10, 10)
    val prop = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val n = Gen.choose(0, ns.length)
      n.map(x => ns.take(x).length === x).get(Simple(System.currentTimeMillis))
    }
    
    Prop.run(prop)
  }
  
  test("Tree") {
  
  }
  
  test("sequence") {
  
  }
}
