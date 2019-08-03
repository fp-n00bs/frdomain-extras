package frdomain.ch6
package domain

import cats.data.NonEmptyList

trait AccountServiceException extends AppException

case class AlreadyExistingAccount(no: String) extends AccountServiceException {
  def msg = s"Already existing account with no $no"
}
case class NonExistingAccount(no: String) extends AccountServiceException {
  def msg = s"No existing account with no $no"
}
case class RateMissingForSavingsAccount (no: String)extends AccountServiceException {
  def msg = s"Rate needs to be given for savings account no $no"
}

//DPDPDP formodligen flytta denna
case class MiscellaneousDomainExceptions(msgs: NonEmptyList[String]) extends AccountServiceException {
  def msg= msgs.toString()
}



