package io.github.facaiy.fp.scala.c12

import io.github.facaiy.fp.scala.c12.Applicative.Failure
import org.scalatest.FunSuite

/**
 * Created by facai on 6/9/17.
 */
class ApplicativeSuite extends FunSuite {
  test("Validation") {
    assert(WebForm.validWebForm("", "90.07.28", "119") ===
           Failure("Name cannot be empty.",
                   Vector("Birthdate must be in the form yyyy-MM-dd.",
                          "Phone number must be 10 digits.")))
  }
}
