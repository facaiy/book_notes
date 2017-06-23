package io.github.facaiy.fp.scala.c15

import org.scalatest.FunSuite

/**
 * Created by facai on 6/22/17.
 */
class ProcessSuite extends FunSuite {
  import Process._

  test("lift") {
    val s = Stream(1, 2, 3)
    val p = lift((x: Int) => x * 2)
    assert(p(s).toList === List(2, 4, 6))
  }

  test("take") {
    val s = Range(1, 10).toStream
    assert(take(3)(s).toList === List(1, 2, 3))
  }

  test("drop") {
    val s = Range(1, 10).toStream
    assert(drop(8)(s).toList === List(9))
  }

  test("takeWhile") {
    val s = Range(1, 10).toStream
    assert(takeWhile((x: Int) => x <= 3)(s).toList === List(1, 2, 3))
  }

  test("dropWhile") {
    val s = Range(1, 10).toStream
    assert(dropWhile((x: Int) => x <= 8)(s).toList === List(9))
  }

  test("count") {
    val s = "hello".toList.toStream
    assert(count(s).toList === Range(1, 6))
  }

  test("mean") {
    val s = Stream(1.0, 3, 5, 7)
    assert(mean(s).toList === List(1, 2, 3, 4))
  }

  test("sum") {
    val s = Stream(1.0, 2, 3, 4)
    assert(sum(s) === List(1, 3, 6, 10))
  }

  test("|>") {
    val s = Range(0, 5).toStream
    val p = filter[Int](_ % 2 == 0) |> lift(_ + 1)

    assert(p(s).toList === List(1, 3, 5))
  }

  test("zipWithIndex") {
    val s = "hel".toList.toStream
    val p = lift((x: Char) => x)

    assert(p(s).zipWithIndex.toList === List(('h', 0), ('e', 1), ('l', 2)))
  }
}
