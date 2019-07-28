package frdomain.ch6
package domain
package repository

import java.util.Date

import cats.data._
import cats.effect.IO
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.model.{Account, Balance}
import frdomain.ch6.domain.service.AccountOperation
import zio.ZIO

trait AccountRepository {
  def accountRepository: AccountRepository.Service
}

object AccountRepository {

  trait Service {
    def query(no: String): AccountOperation[Account]

    def store(a: Account): AccountOperation[Account]

    def query(openedOn: Date): AccountOperation[Seq[Account]]

    def all: AccountOperation[Seq[Account]]

    def balance(no: String): AccountOperation[Balance] = {
      query(no).map(_.balance)
    }
  }
}

package object accountRepository {
  def query(no: String): AccountOperation[Account] =
    ZIO.accessM(_.accountRepository query no)

  def store(a: Account): AccountOperation[Account] =
    ZIO.accessM(_.accountRepository store a)

  def query(openedOn: Date): AccountOperation[Seq[Account]] =
    ZIO.accessM(_.accountRepository query openedOn)

  def all: AccountOperation[Seq[Account]] =
    ZIO.accessM(_.accountRepository all)

  def balance(no: String): AccountOperation[Balance] = {
    query(no).map(_.balance)
  }
}
