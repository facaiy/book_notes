package io.github.facaiy.fp.scala.c10

import org.scalatest.FunSuite

/**
 * Created by facai on 6/1/17.
 */
class MonoidSuite extends FunSuite {
  test("foldLeft") {
    val as = Range(1, 10).toList

    assert(Monoid.foldLeft(1, as)(_ * _) == as.reduce(_ * _))
  }

  test("foldMapV") {
    val as = Range(0, 10)

    assert(Monoid.foldMapV(as, Monoid.intAddition)(_ + 1) === as.map(_ + 1).sum)
  }

  test("isOrdered") {
    assert(Monoid.isOrdered(Range(0, 10).toList) === true)
    assert(Monoid.isOrdered(Range(10, 0, -1).toList) === false)
  }

  test("wordCount") {
    assert(WC.wordCount("") === 0)
    assert(WC.wordCount("?") === 0)
    assert(WC.wordCount("abc") === 1)
    assert(WC.wordCount(" abc ") === 1)
    assert(WC.wordCount("Hello,world") === 2)
    assert(WC.wordCount("Hello, world!") === 2)

    val longText = "Represents a value of one of two possible types (a disjoint union.) " +
      "An instance of Either is an instance of either scala.util.Left or scala.util.Right."
    assert(WC.wordCount(longText) === 28)
  }

  test("bag") {
    val vs = Vector("a", "rose", "is", "a", "rose")

    assert(Monoid.bag(vs) === Map("a"-> 2, "rose" -> 2, "is" -> 1))
  }
}