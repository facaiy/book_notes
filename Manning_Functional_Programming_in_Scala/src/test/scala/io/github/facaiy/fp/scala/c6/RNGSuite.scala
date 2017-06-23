package io.github.facaiy.fp.scala.c6

import org.scalatest.FunSuite

/**
 * Created by facai on 5/2/17.
 */
class RNGSuite extends FunSuite {
  import RNG._

  def unfoldRight[A, B](seed: B)(f: B => Option[(A, B)]): List[A] =
    f(seed) match {
      case Some((a, b)) => a :: unfoldRight(b)(f)
      case None => Nil
    }

  def fill[A](maxIter: Int, seed: Long = 0)(f: RNG => (A, RNG)) = {
    val s = Simple(seed)

    unfoldRight[A, (Int, RNG)]((0, s)){ case ((n, z)) =>
      if (n > maxIter) None
      else {
        val (d, rng1) = f(z)

        Some((d, (n + 1, rng1)))
      }
    }
  }

  def notAllSameElement[A](ls: Seq[A]): Boolean =
    ls.sliding(2).exists(x => x(0) != x(1))

  test("nonNegativeInt") {
    val res = fill(10)(nonNegativeInt)

    assert(notAllSameElement(res))
    assert(res.forall(_ >= 0))
  }

  test("double") {
    val res = fill(10)(double)

    assert(notAllSameElement(res))
    assert(res.forall(x => x >= 0 && x < 1))
  }

  test("ints") {
    val (res, _) = ints(10)(Simple(0))

    assert(notAllSameElement(res))
    assert(res.length === 10)
  }

  test("unit") {
    assert(unit(10.0)(Simple(0))._1 === 10.0)
  }

  test("map") {
    val s = Simple(1)

    val (ori, r0) = int(s)
    val (res, r1) = map(int)(_ + 1)(s)

    assert(r0 === r1)
    assert(res === ori + 1)
  }

  test("nonNegativeLessThan") {
    val res = fill(10)(nonNegativeLessThan(6))

    assert(notAllSameElement(res))
    assert(res.forall(x => x >= 0 && x < 6))
  }
}

class StateSuite extends FunSuite {
  import State._

  test("map") {
    val s = State(RNG.int)
    val r = RNG.Simple(1)

    val ori = s.run(r)
    assert(s.map(_ + 1).run(r) === (ori._1 + 1, ori._2))
  }

  test("map2") {
    val s1 = State(RNG.int)
    val s2 = State(RNG.int)
    val r = RNG.Simple(0)

    val v1 = s1.run(r)
    val v2 = s2.run(v1._2)

    assert(s1.map2(s2)(_ + _).run(r) === (v1._1 + v2._1, v2._2))
  }

  test("sequence") {
    val as = List.fill(10)(State(RNG.int))
    val r = RNG.Simple(0)

    val (res, _) = sequence(as).run(r)
    assert(res.length === 10)
  }

  test("simulateMachine") {
    {
      val inputs = List(Turn, Coin, Turn, Turn, Coin, Coin, Turn)
      val machine = Machine(true, 10, 0)

      val (_, m) = simulateMachine(inputs).run(machine)
      assert(m.locked === true)
      assert(m.candies === 8)
      assert(m.coins === 2)
    }

    {
      val inputs = List(Turn, Coin, Turn, Turn, Coin, Coin, Turn)
      val machine = Machine(true, 1, 0)

      val (_, m) = simulateMachine(inputs).run(machine)
      assert(m.locked === true)
      assert(m.candies === 0)
      assert(m.coins === 1)
    }
  }
}
