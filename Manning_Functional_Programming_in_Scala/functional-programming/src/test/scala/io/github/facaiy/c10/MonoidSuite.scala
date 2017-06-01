package io.github.facaiy.c10

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
}