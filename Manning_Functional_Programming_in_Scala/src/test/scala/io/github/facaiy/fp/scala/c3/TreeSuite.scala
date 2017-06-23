package io.github.facaiy.fp.scala.c3

import org.scalatest.FunSuite

/**
 * Created by facai on 4/14/17.
 */
class TreeSuite extends FunSuite {
  val strTree =
    Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"),
        Branch(Leaf("d"), Leaf("e"))))

  val intTree =
    Branch(
      Branch(Leaf(1), Leaf(3)),
      Branch(Leaf(8), Leaf(14)))

  test("size") {
    assert(Tree.size(strTree) === 9)
    assert(Tree.size(intTree) === 7)
  }

  test("max") {
    assert(Tree.maximum(intTree) === 14)
  }

  test("depth") {
    assert(Tree.depth(strTree) === 3)
    assert(Tree.depth(intTree) === 2)
  }

  test("map") {
    val exp =
      Branch(
        Branch(Leaf("1"), Leaf("3")),
        Branch(Leaf("8"), Leaf("14")))

    assert(Tree.map(intTree)(_.toString) === exp)
  }
}
