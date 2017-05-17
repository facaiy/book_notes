package io.github.facaiy.c8

import java.util.concurrent.{ExecutorService, Executors}

import io.github.facaiy.c6.RNG.Simple
import io.github.facaiy.c7.Par
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
  
  test("check") {
    val es: ExecutorService = Executors.newCachedThreadPool
    val prop = Prop.check(
      Par.map(Par.unit(1))(_ + 1)(es).get === Par.unit(2)(es).get)
    
    Prop.run(prop)
  }
}