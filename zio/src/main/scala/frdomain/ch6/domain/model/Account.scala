package frdomain.ch6
package domain
package model

import java.util.Date

import cats.syntax.apply.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple3Semigroupal}
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.validated.catsSyntaxValidatedId
import frdomain.ch6.domain.common._
import zio.ZIO

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

  private def validateAccountNo(no: String): ValidationResult[String] =
    if (no.isEmpty || no.size < 5) NotValidAccountNumber(no).toInvalidNel
    else no.validNel

  private def validateOpenCloseDate(od: Date, cd: Option[Date]): ValidationResult[(Option[Date], Option[Date])] =
    cd.map { c =>
    if (c before od) NotValidClosingDate(od, c).toInvalidNel
    else (od.some, cd).validNel
  }.getOrElse { (od.some, cd).validNel }

  private def validateRate(rate: BigDecimal): ValidationResult[BigDecimal] =
    if (rate <= BigDecimal(0)) NotValidRate(rate).toInvalidNel
    else rate.validNel

  def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date],
                      balance: Balance): ErrorOr[Account]  = {

    ZIO.fromEither(
    (
      validateAccountNo(no),
      validateOpenCloseDate(openDate.getOrElse(today), closeDate)

    ).mapN {(n, d) =>
      CheckingAccount(n, name, d._1, d._2, balance)
    }.toEither)
  }

  def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date],
                     closeDate: Option[Date], balance: Balance): ErrorOr[Account] = {
    ZIO.fromEither(
    (
      validateAccountNo(no),
      validateOpenCloseDate(openDate.getOrElse(today), closeDate),
      validateRate(rate)

    ).mapN{(n, d, r) =>
      SavingsAccount(n, name, r, d._1, d._2, balance)
    }.toEither)
  }

  private def validateAccountAlreadyClosed(a: Account): ValidationResult[Account] = {
    if (a.dateOfClose isDefined) AccountAlreadyClosed(a.no).toInvalidNel
    else a.validNel
  }

  private def validateCloseDate(a: Account, cd: Date): ValidationResult[Date] = {
    if (cd before a.dateOfOpen.get) NotValidClosingDate(a.dateOfOpen.get, cd).toInvalidNel
    else cd.validNel
  }

  def close(a: Account, closeDate: Date): ErrorOr[Account] = {
    ZIO.fromEither(
      (
        validateAccountAlreadyClosed(a),
        validateCloseDate(a, closeDate)

      ).mapN { (acc, _) =>
          acc match {
            case c: CheckingAccount => c.copy(dateOfClose = Some(closeDate))
            case s: SavingsAccount => s.copy(dateOfClose = Some(closeDate))
          }
        }.toEither)
  }

  private def checkBalance(a: Account, amount: Amount): ValidationResult[Account] = {
    if (amount < 0 && a.balance.amount < -amount) NotSufficientAmount(a).toInvalidNel
    else a.validNel
  }

  def updateBalance(a: Account, amount: Amount): ErrorOr[Account] = {
    ZIO.fromEither(
      (
        validateAccountAlreadyClosed(a),
        checkBalance(a, amount)

      ).mapN { (_, _) =>
        a match {
          case c: CheckingAccount => c.copy(balance = Balance(c.balance.amount + amount))
          case s: SavingsAccount => s.copy(balance = Balance(s.balance.amount + amount))
        }
      }.toEither)
  }

  def rate(a: Account) = a match {
    case SavingsAccount(_, _, r, _, _, _) => r.some
    case _ => None
  }
}

