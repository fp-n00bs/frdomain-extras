package frdomain.ch6.domain.model

import java.util.Date

import cats.data.NonEmptyList
import frdomain.ch6.domain.AppException

trait AccountValidationException extends AppException

case class NotValidAccountNumber(no: String) extends AccountValidationException {
  def msg = NonEmptyList.of(s"Account No has to be at least 5 characters long: found $no")
}

case class NotValidClosingDate(od: Date, c: Date) extends AccountValidationException {
  def msg = NonEmptyList.of(s"Close date [$c] cannot be earlier than open date [$od]")
}

case class NotValidRate(rate: BigDecimal) extends AccountValidationException {
  def msg = NonEmptyList.of(s"Interest rate $rate must be > 0")
}

case class AccountAlreadyClosed(no: String) extends AccountValidationException {
  def msg = NonEmptyList.of(s"Account ${no} is already closed")
}

case class NotSufficientAmount(a: Account) extends AccountValidationException {
  def msg = NonEmptyList.of(s"Insufficient amount in ${a.no} to debit")
}
