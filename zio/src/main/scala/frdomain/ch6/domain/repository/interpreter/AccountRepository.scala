package frdomain.ch6
package domain
package repository
package interpreter

import java.util.Date

import cats.effect.IO
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.model.Account
import frdomain.ch6.domain.service.{AccountOperation, NonExistingAccount}
import zio.ZIO

import scala.collection.mutable.{Map => MMap}

class AccountRepositoryInMemory extends AccountRepository {
  lazy val repo = MMap.empty[String, Account]

  val accountRepository = new AccountRepository.Service {

    def query(no: String): AccountOperation[Account] =
      ZIO.fromOption(repo.get(no)).mapError(_ => NonExistingAccount(no))

    def store(a: Account): AccountOperation[Account] = {
      val _ = repo += ((a.no, a))
      ZIO.succeed(a)
    }

    def query(openedOn: Date): AccountOperation[Seq[Account]] = {
      ZIO.succeed(repo.values.filter(_.dateOfOpen.getOrElse(today) == openedOn).toSeq)
    }

    def all: AccountOperation[Seq[Account]] = ZIO.succeed(repo.values.toSeq)
  }
}

object AccountRepositoryInMemory extends AccountRepositoryInMemory
