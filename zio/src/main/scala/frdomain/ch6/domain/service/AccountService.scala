package frdomain.ch6
package domain
package service

import java.util.Date

import frdomain.ch6.domain.common.ErrorOr

sealed trait AccountType
case object Checking extends AccountType
case object Savings extends AccountType

trait AccountService[Account, Amount, Balance] {

  def open(no: String, name: String, rate: Option[BigDecimal], openingDate: Option[Date],
           accountType: AccountType): ErrorOr[Account]

  def close(no: String, closeDate: Option[Date]): ErrorOr[Account]

  def debit(no: String, amount: Amount): ErrorOr[Account]

  def credit(no: String, amount: Amount): ErrorOr[Account]

  def balance(no: String): ErrorOr[Balance]

  def transfer(from: String, to: String, amount: Amount): ErrorOr[(Account, Account)] = for {
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield ((a, b))
}
