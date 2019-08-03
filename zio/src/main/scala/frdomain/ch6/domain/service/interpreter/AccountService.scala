package frdomain.ch6
package domain
package service
package interpreter

import java.util.Date

import cats.data._
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.model.{Account, Balance}
import frdomain.ch6.domain.repository.AccountRepository
import frdomain.ch6.domain.repository.accountRepository.{query, store}
import zio.ZIO

class AccountServiceInterpreter extends AccountService[Account, Amount, Balance] {

  def open(no: String,
           name: String,
           rate: Option[BigDecimal],
           openingDate: Option[Date],
           accountType: AccountType): ErrorOr[Account] = {

    for {
      maybeAccount   <- query(no)
      account        <- doOpenAccount(maybeAccount, no, name, rate, openingDate, accountType)
    } yield account
  }

  private def doOpenAccount(maybeAccount: Option[Account],
                            no: String,
                            name: String,
                            rate: Option[BigDecimal],
                            openingDate: Option[Date],
                            accountType: AccountType): ErrorOr[Account] =

    maybeAccount.map(_ => ZIO.fail(AlreadyExistingAccount(no)))
      .getOrElse(createOrUpdate(no, name, rate, openingDate, accountType))


  private def createOrUpdate(no: String,
                             name: String,
                             rate: Option[BigDecimal],
                             openingDate: Option[Date],
                             accountType: AccountType): ErrorOr[Account] =
    accountType match {

      case Checking => createOrUpdate(Account.checkingAccount(no, name, openingDate, None, Balance()))
      case Savings => rate.map(r => createOrUpdate(Account.savingsAccount(no, name, r, openingDate, None, Balance())))
        .getOrElse(ZIO.fail(RateMissingForSavingsAccount(no)))
    }

  private def createOrUpdate(errorOrAccount: ErrorOr[Account]): ErrorOr[Account] =
    errorOrAccount.foldM(
      err => ZIO.fail(MiscellaneousDomainExceptions(NonEmptyList.of(err.toString))),
      acc => store(acc))

  def close(no: String, closeDate: Option[Date]): ErrorOr[Account] = {
    for {
      maybeAccount <- query(no)
      account      <- maybeAccount.map(a => createOrUpdate(Account.close(a, closeDate.getOrElse(today))))
                   .getOrElse(ZIO.fail(NonExistingAccount(no)))
    } yield account
  }

  def balance(no: String): ErrorOr[Balance] = balance(no)

  private trait DC
  private object D extends DC
  private object C extends DC

  def debit(no: String, amount: Amount): ErrorOr[Account] = update(no, amount, D)
  def credit(no: String, amount: Amount): ErrorOr[Account] = update(no, amount, C)

  private def update(no: String, amount: Amount, debitCredit: DC): ErrorOr[Account] =
    for {
      maybeAccount <- query(no)
      multiplier = if (debitCredit == D) (-1) else 1
      account      <- maybeAccount.map(a => createOrUpdate(Account.updateBalance(a, multiplier * amount)))
        .getOrElse(ZIO.fail(NonExistingAccount(no)))
    } yield account

}
object AccountService extends AccountServiceInterpreter
