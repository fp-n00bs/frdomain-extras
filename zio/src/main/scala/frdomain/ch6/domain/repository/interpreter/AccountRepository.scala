package frdomain.ch6
package domain
package repository
package interpreter

import java.util.Date

import cats.effect.IO
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.model.Account

import scala.collection.mutable.{Map => MMap}

class AccountRepositoryInMemory extends AccountRepository {
  lazy val repo = MMap.empty[String, Account]

  def query(no: String): IO[ErrorOr[Option[Account]]] = IO(Right(repo.get(no)))

  def store(a: Account): IO[ErrorOr[Account]] = IO {
    val _ = repo += ((a.no, a))
    Right(a)
  }

  def query(openedOn: Date): IO[ErrorOr[Seq[Account]]] = IO {
    Right(repo.values.filter(_.dateOfOpen.getOrElse(today) == openedOn).toSeq)
  }

  def all: IO[ErrorOr[Seq[Account]]] = IO(Right(repo.values.toSeq))
}

object AccountRepositoryInMemory extends AccountRepositoryInMemory
