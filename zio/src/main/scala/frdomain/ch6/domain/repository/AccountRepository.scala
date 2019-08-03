package frdomain.ch6
package domain
package repository

import java.util.Date

import frdomain.ch6.domain.common._
import frdomain.ch6.domain.model.{Account, Balance}
import zio.ZIO

trait AccountRepository{
  def accountRepository: AccountRepository.Service
}

object AccountRepository {

  trait Service {
    def query(no: String): ErrorOr[Option[Account]]

    def store(a: Account): ErrorOr[Account]

    def query(openedOn: Date): ErrorOr[Seq[Account]]

    def all: ErrorOr[Seq[Account]]

    def balance(no: String): ErrorOr[Balance] = {
      query(no).foldM(
        _    => ZIO.fail(NonExistingAccount(no).msg),
        acc  => ZIO.succeed(acc.get.balance))
    }
  }
}

package object accountRepository {
  def query(no: String): ErrorOr[Option[Account]] =
    ZIO.accessM(_.accountRepository query no)

  def store(a: Account): ErrorOr[Account] =
    ZIO.accessM(_.accountRepository store a)

  def query(openedOn: Date): ErrorOr[Seq[Account]] =
    ZIO.accessM(_.accountRepository query openedOn)

  def all: ErrorOr[Seq[Account]] =
    ZIO.accessM(_.accountRepository all)

  def balance(no: String): ErrorOr[Balance] =
    ZIO.accessM(_.accountRepository balance no)

}
