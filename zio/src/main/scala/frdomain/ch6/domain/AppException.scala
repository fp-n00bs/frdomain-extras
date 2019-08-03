package frdomain.ch6.domain

import cats.data.NonEmptyList
import cats.syntax.validated.catsSyntaxValidatedId

trait AppException {
  def msg: String
  def fullMsg= this.getClass.getSimpleName + ": " + msg.toString()
}
