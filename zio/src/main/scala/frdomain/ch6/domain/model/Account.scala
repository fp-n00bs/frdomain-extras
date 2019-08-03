package frdomain.ch6
package domain
package model

import java.util.Date

import cats.data.NonEmptyList
import cats.syntax.apply.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple3Semigroupal}
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.validated.catsSyntaxValidatedId
import frdomain.ch6.domain.common.{Validation, _}
import zio.{UIO, ZIO, IO}

case class Balance(amount: Amount = 0)

sealed trait Account {
  def no: String
  def name: String
  def dateOfOpen: Option[Date]
  def dateOfClose: Option[Date]
  def balance: Balance
}

final case class CheckingAccount (no: String, name: String,
                                  dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

final case class SavingsAccount (no: String, name: String, rateOfInterest: Amount,
                                 dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

object Account {

  def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date],
                      balance: Balance): ErrorOr[Account]  = {

    AccumulateErrors(Seq
    (
      validateAccountNo(no),
      validateOpenCloseDate(openDate.getOrElse(today), closeDate))
    ).accumulateParN()
  }

  def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date],
                     closeDate: Option[Date], balance: Balance): ErrorOr[Account] = {

    validateAccountNo(no)
    validateOpenCloseDate(openDate.getOrElse(today), closeDate)
    validateRate(rate)
    ZIO.succeed(SavingsAccount(no, name, rate, openDate, closeDate, balance))

  }

  def close(a: Account, closeDate: Date): ErrorOr[Account] = {


    validateAccountAlreadyClosed(a)
    validateCloseDate(a, closeDate)
    ???
  }

  private def checkBalance(a: Account, amount: Amount): ErrorOr[Account] = {
    if (amount < 0 && a.balance.amount < -amount) ZIO.fail(NotSufficientAmount(a))
    else ZIO.succeed(a)
  }

  def updateBalance(a: Account, amount: Amount): ErrorOr[Account] = {


    validateAccountAlreadyClosed(a)
    checkBalance(a, amount)
    ???
  }

  def rate(a: Account) = a match {
    case SavingsAccount(_, _, r, _, _, _) => r.some
    case _ => None
  }

  private def validateAccountNo(no: String): Validation =
    if (no.isEmpty || no.size < 5) ZIO.fail(NotValidAccountNumber(no).msg)
    else validationOk

  private def validateOpenCloseDate(od: Date, cd: Option[Date]): Validation =
    if (cd.get before od) ZIO.fail(NotValidClosingDate(od, cd.get).msg)
    else validationOk

  private def validateRate(rate: BigDecimal): Validation =
    if (rate <= BigDecimal(0)) ZIO.fail(NotValidRate(rate).msg)
    else validationOk

  private def validateAccountAlreadyClosed(a: Account): Validation = {
    if (a.dateOfClose isDefined) ZIO.fail(AccountAlreadyClosed(a.no).msg)
    else validationOk
  }

  private def validateCloseDate(a: Account, cd: Date): Validation = {
    if (cd before a.dateOfOpen.get) ZIO.fail(NotValidClosingDate(a.dateOfOpen.get, cd).msg)
    else validationOk
  }

}

