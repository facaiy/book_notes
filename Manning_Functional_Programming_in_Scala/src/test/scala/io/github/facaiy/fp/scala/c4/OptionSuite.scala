package io.github.facaiy.fp.scala.c4

import org.scalatest.FunSuite

import io.github.facaiy.fp.scala.c4.{Some => CSome, None => CNone, Option => COption}

/**
 * Created by facai on 4/20/17.
 */
class OptionSuite extends FunSuite {
  test("map") {
    assert(CSome(1).map(_ + 1) === CSome(2))
    assert(CNone.map(_ => 1) === CNone)

    assert(CSome(1).map(x => CSome(x + 1)) === CSome(CSome(2)))
    assert(CNone.map(_ => CSome(1)) === CNone)

    assert(CSome(1).map(x => CNone) === CSome(CNone))
    assert(CNone.map(_ => CNone) === CNone)
  }

  test("flatMap") {
    assert(CSome(1).flatMap(x => CSome(x + 1)) === CSome(2))
    assert(CNone.flatMap(_ => CSome(1)) === CNone)

    assert(CSome(1).flatMap(x => CNone) === CNone)
    assert(CNone.flatMap(_ => CNone) === CNone)
  }

  test("getOrElse") {
    assert(CSome(1).getOrElse(0) === 1)
    assert(CNone.getOrElse(0) === 0)
  }

  test("orElse") {
    assert(CSome(1).orElse(CSome(0)) === CSome(1))
    assert(CNone.orElse(CSome(0)) === CSome(0))
  }

  test("filter") {
    assert(CSome(1).filter(_ == 1) === CSome(1))
    assert(CSome(2).filter(_ == 1) === CNone)
  }

  test("variance") {
    assert(COption.variance(Seq.empty[Double]) === CNone)
    assert(COption.variance(Seq(1.0, -1.0, 1.0, -1.0)) === CSome(1.0))
  }

  test("sequence") {
    assert(COption.sequence(List(CSome(1), CSome(2))) === CSome(List(1, 2)))
    assert(COption.sequence(List(CSome(1), None)) === CNone)
  }

  test("traverse") {
    assert(COption.traverse(List(1, 2, 3))(CSome(_)) === CSome(List(1, 2, 3)))

    val isTwo = (x: Int) => if (x == 2) None else Some(x)
    assert(COption.traverse(List(1, 2, 3))(isTwo) === CNone)
  }
}
