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
           accountType: AccountType): AccountOperation[Account] = {

    query(no).foldM(
      _ => openAccount(no, name, rate, openingDate, accountType),
      _ => ZIO.fail(AlreadyExistingAccount(no))
    )
  }

  private def openAccount(no: String,
                          name: String,
                          rate: Option[BigDecimal],
                          openingDate: Option[Date],
                          accountType: AccountType) : AccountOperation[Account] = {
    accountType match {

      case Checking => createOrUpdate(Account.checkingAccount(no, name, openingDate, None, Balance()))

      case Savings  => rate.map { r =>
        createOrUpdate(Account.savingsAccount(no, name, r, openingDate, None, Balance()))
      } getOrElse {
        ZIO.fail(RateMissingForSavingsAccount)
      }
    }
  }

  private def createOrUpdate(accountOp: AccountOperation[Account]) : AccountOperation[Account] =
    accountOp.flatMap(store(_))

  def close(no: String, closeDate: Option[Date]) : AccountOperation[Account]  = {
    val cd = closeDate.getOrElse(today)
    createOrUpdate(Account.close(no, cd))
  }

  def debit(no: String, amount: Amount) = up(no, amount, D)
  def credit(no: String, amount: Amount) = up(no, amount, C)

  private trait DC
  private case object D extends DC
  private case object C extends DC

  private def up(no: String, amount: Amount, debitCredit: DC) : AccountOperation[Account] =
    for {
      maybeAccount   <- query(no)
      multiplier = if (debitCredit == D) (-1) else 1
      account        <- createOrUpdate(Account.updateAmount(maybeAccount, multiplier * amount))
    } yield account

  def balance(no: String): AccountOperation[Balance] = AccountService.balance(no)
}

object AccountService extends AccountServiceInterpreter
