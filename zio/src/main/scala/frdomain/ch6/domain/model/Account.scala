package frdomain.ch6
package domain
package model

import java.util.Date

import cats.implicits._
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.repository.accountRepository.query
import frdomain.ch6.domain.service._
import zio.ZIO

case class Balance(amount: Amount = 0)

sealed trait Account {

  def no: String

  def name: String

  def dateOfOpen: Option[Date]

  def dateOfClose: Option[Date]

  def balance: Balance
}

private final case class CheckingAccount(no: String, name: String,
                                         dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

private final case class SavingsAccount(no: String, name: String, rateOfInterest: Amount,
                                        dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

object Account {

  private def validateAccountNo(no: String): PureResult[String] =
    if (no.isEmpty || no.size < 5) Left(NotValidAccountNumber(no))
    else Right(no)


  private def validateOpenCloseDate(od: Date, cd: Option[Date]): PureResult[Option[Date]] =
    cd.map { c =>
      if (c before od) Left(NotValidClosingDate(od, c))
      else Right(cd)
    }.getOrElse(Right(cd))


  private def validateRate(rate: BigDecimal): AccountOperation[BigDecimal] =
    if (rate <= BigDecimal(0)) ZIO.fail(NotValidRate(rate))
    else ZIO.succeed(rate)

  def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date],
                      balance: Balance): AccountOperation[Account] =
    for {
      _     <- validateAccountNo(no)
      _     <- validateOpenCloseDate(openDate.getOrElse(today), closeDate)
    } yield CheckingAccount(no, name, openDate, closeDate, balance)

  def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date],
                     closeDate: Option[Date], balance: Balance): AccountOperation[Account] =
    for {
      _     <- validateAccountNo(no)
      _     <- validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      _     <- validateRate(rate)
    } yield SavingsAccount(no, name, rate, openDate, closeDate, balance)

  private def validateAccountAlreadyClosed(a: Account) : PureResult[Account] = {
    if (a.dateOfClose isDefined) Left(AccountAlreadyClosed(a.no))
    else Right(a)
  }

  private def validateCloseDate(a: Account, cd: Date) : AccountOperation[Date] = {
    if (cd before a.dateOfOpen.get) ZIO.fail(NotValidClosingDate(a.dateOfOpen.get, cd))
    else ZIO.succeed(cd)
  }

  private def updateClosingDate(a: Account, someDate: Some[Date]): Account = {
    a match {
      case c: CheckingAccount => c.copy(dateOfClose = someDate)
      case s: SavingsAccount => s.copy(dateOfClose = someDate)
    }
  }

  def close(no: String, closeDate: Date): AccountOperation[Account] = {
    for {
      acc   <- query(no)
      _     <- validateAccountAlreadyClosed(acc)
      _     <- validateCloseDate(acc, closeDate)
    } yield updateClosingDate(acc, Some(closeDate))
  }

  private def checkBalance(a: Account, amount: Amount):AccountOperation[Account] = {
    if (amount < 0 && a.balance.amount < -amount) ZIO.fail(NotSufficientAmount(a))
    else ZIO.succeed(a)
  }

  def updateAmount(a: Account, amount: Amount): AccountOperation[Account] = for {
    _     <- validateAccountAlreadyClosed(a)
    _     <- checkBalance(a, amount)
    _     <- updateBalance(a, amount)
  } yield (a)

  private def updateBalance(a: Account, amount: Amount): PureResult[Account] = {
    Right(a match {
      case c: CheckingAccount => c.copy(balance = Balance(c.balance.amount + amount))
      case s: SavingsAccount => s.copy(balance = Balance(s.balance.amount + amount))
    })
  }

  def rate(a: Account) = a match {
    case SavingsAccount(_, _, r, _, _, _) => Some(r)
    case _ => None
  }
}

