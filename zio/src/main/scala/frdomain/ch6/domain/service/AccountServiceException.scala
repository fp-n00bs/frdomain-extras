package frdomain.ch6
package domain

import cats.data.NonEmptyList

trait AccountServiceException extends AppException

case class AlreadyExistingAccount(no: String) extends AccountServiceException {
  def msg = NonEmptyList.of(s"Already existing account with no $no")
}
case class NonExistingAccount(no: String) extends AccountServiceException {
  def msg = NonEmptyList.of(s"No existing account with no $no")
}
case object RateMissingForSavingsAccount extends AccountServiceException {
  def msg = NonEmptyList.of("Rate needs to be given for savings account")
}
case class MiscellaneousDomainExceptions(msgs: NonEmptyList[String]) extends AccountServiceException {
  def msg= msgs
}



