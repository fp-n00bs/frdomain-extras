package frdomain.ch6.domain

import cats.data.NonEmptyList
import cats.syntax.validated.catsSyntaxValidatedId

trait AppException {
  def msg: NonEmptyList[String]
  def fullMsg= this.getClass.getSimpleName + ": " + msg.toString()
  def toInvalidNel= msg.toString().invalidNel
}
