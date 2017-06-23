package io.github.facaiy.fp.scala.c3

import org.scalatest.FunSuite

import io.github.facaiy.fp.scala.c3.{List => CustomList}

/**
 * Created by facai on 4/10/17.
 */
class ListSuite extends FunSuite {

  test("sum") {
    val data = CustomList(1, 3, 5, 2)
    assert(CustomList.sum(data) === 11)
  }

  test("product") {
    val data = CustomList(1.0, 3.0, 5.0, 2.0)
    assert(CustomList.product(data) === 30.0)
  }

  test("tail") {
    val list = CustomList(1, 3, 5, 2)

    assert(CustomList.tail(list) === CustomList(3, 5, 2))
  }

  test("drop") {
    val list = CustomList(1, 3, 5, 2)

    assert(CustomList.drop(list, 2) === CustomList(5, 2))
  }

  test("dropWhile") {
    val list = CustomList(1, 3, 5, 2)

    assert(CustomList.dropWhile(list)(_ < 4) === CustomList(5, 2))
    assert(CustomList.dropWhile(list)(_ > 4) === CustomList(1, 3, 5, 2))
  }

  test("init") {
    val list = CustomList(1, 3, 5, 2)

    assert(CustomList.init(list) === CustomList(1, 3, 5))
  }

  test("foldRight") {
    {
      val list = CustomList(1, 3, 5, 2)
      assert(CustomList.foldRight(list, 0)(_ + _) === CustomList.sum(list))
    }

    {
      val list = CustomList(1.0, 3.0, 5.0, 2.0)
      assert(CustomList.foldRight(list, 1.0)(_ * _) === CustomList.product(list))
    }
  }

  test("ex 3.8") {
    val res = CustomList.foldRight(CustomList(1,2,3), Nil:List[Int])(Cons(_,_))
    print(res)
  }

  test("length") {
    val list = CustomList(1, 3, 5, 2)

    assert(CustomList.length(list) === 4)
  }

  test("foldLeft") {
    val list = CustomList(1, 3, 5, 2)

    val left = CustomList.foldLeft(list, 0)(_ + _)
    val right = CustomList.foldRight(list, 0)(_ + _)

    assert(left === right)
  }

  test("reverse") {
    val list = CustomList(1, 3, 5, 2)

    val exp = CustomList(2, 5, 3, 1)
    assert(CustomList.reverse(list) === exp)
  }

  test("append") {
    val list1 = CustomList(3, 4, 5)
    val list2 = CustomList(1, 2)

    val exp = CustomList(Range(1, 6): _*)

    assert(CustomList.append(list2, list1) === exp)
  }

  test("concatenate") {
    val list0 = CustomList(6, 7, 8, 9)
    val list1 = CustomList(3, 4, 5)
    val list2 = CustomList(1, 2)

    val exp = CustomList(Range(1, 10): _*)

    assert(CustomList.concatenate(CustomList(list0, list1, list2)) === exp)
  }

  test("map") {
    // ex 3.16
    {
      val list = CustomList(1, 3, 5, 2)
      val exp = CustomList(2, 4, 6, 3)

      assert(CustomList.map(list)(_ + 1) === exp)
    }

    // ex 3.17
    {
      val list = CustomList(1.0, 3.0, 5.0)
      val exp = CustomList("1.0", "3.0", "5.0")

      assert(CustomList.map(list)(_.toString) === exp)
    }
  }

  test("filter") {
    val list = CustomList(Range(1, 10): _*)

    val exp = CustomList(Range(2, 10, 2): _*)
    assert(CustomList.filter(list)(_ % 2 == 0) === exp)
  }

  test("flatMap") {
    val list = CustomList(1, 2, 3)

    val exp = CustomList(1, 1, 2, 2, 3, 3)
    assert(CustomList.flatMap(list)(i => CustomList(i, i)) === exp)
  }

  test("zipWith") {
    val list0 = CustomList(1, 2, 3)
    val list1 = CustomList(4, 5, 6)

    val exp = CustomList(5, 7, 9)
    assert(CustomList.zipWith(list0, list1)(_ + _) === exp)
  }

  test("hasSubsequence") {
    val sup = CustomList(1, 2, 3, 4)

    assert(CustomList.hasSubsequence(
      sup,
      CustomList(1, 2)) === true)

    assert(CustomList.hasSubsequence(
      sup,
      CustomList(2, 3)) === true)

    assert(CustomList.hasSubsequence(
      sup,
      CustomList(4)) === true)

    assert(CustomList.hasSubsequence(
      sup,
      CustomList(1, 3, 4)) === false)

    assert(CustomList.hasSubsequence(
      sup,
      CustomList(1, 4, 4)) === false)
  }
}
