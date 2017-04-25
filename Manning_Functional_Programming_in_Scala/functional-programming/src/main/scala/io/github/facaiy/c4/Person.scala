package io.github.facaiy.c4

import scala.{Option => _, Either => _, Left => _, Right => _, _}

/**
 * Created by facai on 4/25/17.
 */
sealed class Name(val value: String)
sealed class Age(val value: Int)

case class Person(name: Name, age: Age)

object Person {
  import EitherPlus._

  def mkName(name: String): EitherPlus[String, Name] =
    if (name == "" || name == null) LeftPlus("Name is empty.")
    else RightPlus(new Name(name))

  def mkAge(age: Int): EitherPlus[String, Age] =
    if (age < 0) LeftPlus(s"Age $age is out of range.")
    else RightPlus(new Age(age))

  def mkPerson(name: String, age: Int): EitherPlus[String, Person] =
    map2(mkName(name), mkAge(age))(Person(_, _))
}
