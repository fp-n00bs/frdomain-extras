package frdomain.ch6
package domain
package service

import java.util.Date

import frdomain.ch6.domain.model.Account
import cats.data._
import frdomain.ch6.domain.common.errors.AppError

trait AccountServiceException extends AppError {

case class AlreadyExistingAccount(no: String) extends AccountServiceException {
  override def msg: String = s"Already existing account with no $no"
}

case class NonExistingAccount(no: String) extends AccountServiceException {
  override def msg: String = s"No existing account with no $no"
}

case class ClosedAccount(no: String) extends AccountServiceException {
  override def msg: String = s"Account with no $no is closed"
}

case object RateMissingForSavingsAccount extends AccountServiceException {
  override def msg: String = "Rate needs to be given for savings account"
}

case class NotValidAccountNumber(no: String) extends AccountServiceException {
  override def msg: String = s"Account No has to be at least 5 characters long: found $no"
}

case class NotValidClosingDate(od: Date, c: Date) extends AccountServiceException {
  override def msg: String = s"Close date [$c] cannot be earlier than open date [$od]"
}

case class NotValidRate(rate: BigDecimal) extends AccountServiceException {
  override def msg: String = s"Interest rate $rate must be > 0"
}

case class AccountAlreadyClosed(no: String) extends AccountServiceException {
  override def msg: String = s"Account ${no} is already closed"
}

case class NotSufficientAmount(a: Account) extends AccountServiceException {
  val message = NonEmptyList.of(s"Insufficient amount in ${a.no} to debit")
}
}
