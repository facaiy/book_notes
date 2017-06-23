package io.github.facaiy.fp.scala.c4

import scala.{Option => _, Either => _, Left => _, Right => _, _}

import org.scalatest.FunSuite

/**
 * Created by facai on 4/25/17.
 */
class PersonSuite extends FunSuite {
  test("mkPerson") {
    print(Person.mkPerson("", -1))
  }
}
