package io.github.facaiy

import org.scalatest.FunSuite

/**
 * Created by facai on 4/10/17.
 */
class ChapterTwoSuite extends FunSuite {
  import ChapterTwo._

  test("testFib") {
    assert(1 === factorial(0))
    assert(1 === factorial(1))
    assert(24 === factorial(4))
  }

  test("testFactorial") {
    assert(0 === fib(0))
    assert(1 === fib(1))
    assert(1 === fib(2))
    assert(5 === fib(5))
  }

  test("testFindFirst") {
    assertResult(2)(findFirst(Array(1, 3, 2, 1, 2, 1))(_ == 2))
    assertResult(1)(
      findFirst(Array("hi", "world", "other"))(_.startsWith("w")))
  }

  test("testIsSorted") {
    assertResult(true)(isSorted(Array(1))(_ < _))
    assertResult(true)(isSorted(Array(1, 3, 5, 7))(_ < _))
    assertResult(false)(isSorted(Array(1, 2, 3, 4))(_ > _))
  }
}
