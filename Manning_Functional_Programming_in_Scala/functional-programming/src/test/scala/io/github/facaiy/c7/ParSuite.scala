package io.github.facaiy.c7

import org.scalatest.FunSuite

/**
 * Created by facai on 5/5/17.
 */
class ParSuite extends FunSuite {
  test("sum") {
    val ints = Range(0, 10).toArray

    assert(Examples.sum(ints) === Range(0, 10).sum)
  }
}
