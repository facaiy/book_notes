package io.github.facaiy.c4

import scala.{Option => _, Either => _, Left => _, Right => _, _}

import org.scalatest.FunSuite


/**
 * Created by facai on 4/24/17.
 */
class EitherSuite extends FunSuite {
  test("map") {
    assert(Right(2).map(_ + 1) === Right(3))

    val l: Either[String, Int] = Left("hi")
    assert(l.map(_ + 1) === Left("hi"))
  }

  test("orElse") {
    assert(Left("hi").orElse(Right(0)) === Right(0))
    assert(Right(1).orElse(Right(0)) === Right(1))
  }

  test("map2") {
    assert(Right(1).map2(Right(2))(_ + _) === Right(3))
    assert(Right(1).map2(Left("hi"))(_ + _) === Left("hi"))
  }

  test("sequence") {
    {
      val ls = List(Right(1), Right(2), Right(3))

      val res = Either.sequence(ls)
      assert(res === Right(List(1, 2, 3)))
    }

    {
      val ls = List(Right(1), Left("first"), Left("second"))

      val res = Either.sequence(ls)
      assert(res === Left("first"))
    }
  }
}
