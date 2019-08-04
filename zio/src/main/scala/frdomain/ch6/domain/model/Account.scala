package frdomain.ch6
package domain
package model

import java.util.Date

import cats.data.NonEmptyList
import cats.syntax.apply.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple3Semigroupal}
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.validated.catsSyntaxValidatedId
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.model
import frdomain.ch6.domain.model.Account.{rate, validateAccountNo, validateOpenCloseDate, validateRate}
import zio.{IO, UIO, ZIO}

case class Balance(amount: Amount = 0)

sealed trait Account {
  def no: String
  def name: String
  def dateOfOpen: Option[Date]
  def dateOfClose: Option[Date]
  def balance: Balance
}

final case class CheckingAccount(no: String, name: String,
                                 dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

final case class SavingsAccount(no: String, name: String, rateOfInterest: Amount,
                                dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

object Account {

  def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date],
                      balance: Balance): ErrorOr[Account] =
    for {
      _   <- validateAccountNo(no)
      _   <- validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      acc = CheckingAccount(no, name, openDate, closeDate, balance)
    } yield acc

  def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date],
                     closeDate: Option[Date], balance: Balance): ErrorOr[Account] =
    for {
      _   <- validateAccountNo(no)
      _   <- validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      _   <- validateRate(rate)
      acc = SavingsAccount(no, name, rate, openDate, closeDate, balance)
    } yield acc

  def close(a: Account, closeDate: Date): ErrorOr[Account] =
    for {
      _   <- validateAccountAlreadyClosed(a)
      _   <- validateCloseDate(a, closeDate)
      acc = a match {
        case c: CheckingAccount => c.copy(dateOfClose = Some(closeDate))
        case s: SavingsAccount => s.copy(dateOfClose = Some(closeDate))
      }} yield acc

  private def checkBalance(a: Account, amount: Amount): ErrorOr[Account] =
    if (amount < 0 && a.balance.amount < -amount) ZIO.fail(NotSufficientAmount(a))
    else ZIO.succeed(a)

  def updateBalance(a: Account, amount: Amount): ErrorOr[Account] =
    for {
      _   <- validateAccountAlreadyClosed(a)
      _   <- checkBalance(a, amount)
      acc = a match {
        case c: CheckingAccount => c.copy(balance = Balance(c.balance.amount + amount))
        case s: SavingsAccount => s.copy(balance = Balance(s.balance.amount + amount))
      }} yield acc

  def rate(a: Account) = a match {
    case SavingsAccount(_, _, r, _, _, _) => r.some
    case _ => None
  }

  private def validateAccountNo(no: String): ErrorOr[String] =
    if (no.isEmpty || no.size < 5) ZIO.fail(NotValidAccountNumber(no))
    else valid

  private def validateCloseDate(a: Account, cd: Date): ErrorOr[Date] =
    if (cd before a.dateOfOpen.get) ZIO.fail(NotValidClosingDate(a.dateOfOpen.get, cd))
    else ZIO.succeed(cd)

  private def validateOpenCloseDate(od: Date, cd: Option[Date]): ErrorOr[(Option[Date], Option[Date])] =
    cd.map { c =>
    if (c before od) ZIO.fail(NotValidClosingDate(od, c))
    else ZIO.succeed((od.some, cd))
  }.getOrElse { ZIO.succeed((od.some, cd))}

  private def validateRate(rate: BigDecimal): ErrorOr[String] =
    if (rate <= BigDecimal(0)) ZIO.fail(NotValidRate(rate))
    else valid

  private def validateAccountAlreadyClosed(a: Account): ErrorOr[String] =
    if (a.dateOfClose isDefined) ZIO.fail(AccountAlreadyClosed(a.no))
    else valid
}

