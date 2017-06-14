package io.github.facaiy.c12

import java.util.Date

import io.github.facaiy.c12.Applicative.{Failure, Success, Validation}

/**
 * Created by facai on 6/9/17.
 */
case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object WebForm {
  def validName(name: String): Validation[String, String] =
    if (name.isEmpty) Failure("Name cannot be empty.")
    else Success(name)

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd.")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits.")

  def validWebForm(name: String,
                   birthdate: String,
                   phoneNumber: String): Validation[String, WebForm] =
    Applicative.validationApplicative[String].map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phoneNumber)
    )(WebForm(_, _, _))
}
