package io.github.facaiy.fp.scala.c5

import org.scalatest.FunSuite

/**
 * Created by facai on 4/26/17.
 */
class StreamTest extends FunSuite {
  test("headOption") {
    assert(Stream().headOption === None)
    assert(Stream(1, 2, 3).headOption === Some(1))
  }

  test("toList") {
    assert(Stream().toList === List.empty)
    assert(Stream(1, 2, 3).toList === List(1, 2, 3))
  }

  test("take") {
    {
      val s = Stream(1, 2, 3, 4, 5)

      assert(s.take(-1) === Stream.empty)
      assert(s.take(0) === Stream.empty)
      assert(s.take(2).toList === List(1, 2)) // function -> value
      assert(s.take(5).toList === List(1, 2, 3, 4, 5))
      assert(s.take(10).toList === List(1, 2, 3, 4, 5))
    }

    {
      assert(Stream.empty.take(-1) === Stream.empty)
      assert(Stream.empty.take(0) === Stream.empty)
      assert(Stream.empty.take(2) === Stream.empty)
    }
  }

  test("drop") {
    {
      val s = Stream(1, 2, 3, 4, 5)

      assert(s.drop(-1) === s)
      assert(s.drop(0) === s)
      assert(s.drop(2).toList === List(3, 4, 5)) // function -> value
      assert(s.drop(5) === Stream.empty)
    }

    {
      assert(Stream.empty.drop(-1) === Stream.empty)
      assert(Stream.empty.drop(0) === Stream.empty)
      assert(Stream.empty.drop(2) === Stream.empty)
    }
  }

  test("takeWhile") {
    {
      val s = Stream(1, 2, 3, 4, 5)

      assert(s.takeWhile(_ < 3).toList === List(1, 2)) // function -> value
      assert(s.takeWhile(_ > 3).toList === List.empty)
    }

    {
      assert(Stream.empty[Int].takeWhile(_ => true) === Stream.empty[Int])
      assert(Stream.empty[Int].takeWhile(_ => false) === Stream.empty[Int])
    }
  }

  test("exist") {
    val s = Stream(1, 2, 3, 4, 5)

    assert(s.exists(_ > 2) === true)
    assert(s.exists(_ > 6) === false)
  }

  test("forAll") {
    val s = Stream(1, 2, 3, 4, 5)

    assert(s.forAll(_ > 0) === true)
    assert(s.forAll(_ < 3) === false)
  }

  test("map") {
    assert(Stream().map(_ => 1).toList === List())
    assert(Stream(1, 2, 3).map(_ + 1).toList === List(2, 3, 4))
  }

  test("filter") {
    assert(Stream().filter(_ == 1).toList === List())
    assert(Stream(1, 2, 3, 4).filter(_ < 0).toList === List())
    assert(Stream(1, 2, 3, 4).filter(_ > 2).toList === List(3, 4))
    assert(Stream(1, 2, 3, 4).filter(_ > 0).toList === List(1, 2, 3, 4))
  }

  test("append") {
    assert(Stream().append(1).toList === List(1))
    assert(Stream(1, 2, 3).append(4).toList === List(1, 2, 3, 4))
  }

  test("flatMap") {
    assert(Stream().flatMap(x => Stream(x, x)).toList === List())
    assert(Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList === List(1, 1, 2, 2, 3, 3))
  }

  test("constant") {
    assert(Stream.constant(1).take(3).toList === List(1, 1, 1))
  }

  test("from") {
    assert(Stream.from(1).take(3).toList === List(1, 2, 3))
  }

  test("fibs") {
    assert(Stream.fibs.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }

  test("zipAll") {
    {
      val s1 = Stream.empty[Int]
      val s2 = Stream(1, 2)

      assert(s1.zipAll(s2).toList === List((None, Some(1)), (None, Some(2))))
    }

    {
      val s1 = Stream(1, 2, 3)
      val s2 = Stream(1, 2)

      assert(s1.zipAll(s2).toList === List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None)))
    }
  }

  test("startsWith") {
    {
      val s1 = Stream.empty[Int]
      val s2 = Stream(1, 2)

      assert(s1.startsWith(s2) === false)
      assert(s2.startsWith(s1) === true)
    }

    {
      val s1 = Stream(1, 2, 3, 4)
      val s2 = Stream(2, 3)

      assert(s1.startsWith(s2) === false)
      assert(s2.startsWith(s1) === false)
    }
    {
      val s1 = Stream(1, 2, 3, 4)
      val s2 = Stream(1, 2)

      assert(s1.startsWith(s2) === true)
      assert(s2.startsWith(s1) === false)
    }
  }

  test("tails") {
    val s = Stream(1, 2)

    assert(s.tails.toList.map(_.toList) == List(List(1, 2), List(2)))
  }

  test("hasSubsequence") {
    val sup = Stream(1, 2, 3, 4)

    assert(sup.hasSubsequence(Stream(1, 2)) === true)
    assert(sup.hasSubsequence(Stream(2, 3)) === true)
    assert(sup.hasSubsequence(Stream(4)) === true)
    assert(sup.hasSubsequence(Stream(1, 3, 4)) === false)
    assert(sup.hasSubsequence(Stream(1, 4, 4)) === false)
  }

  test("scanRight") {
    val s = Stream(1, 2, 3)

    assert(s.scanRight(0)(_ + _).toList === List(6, 5, 3, 0))
  }
}
