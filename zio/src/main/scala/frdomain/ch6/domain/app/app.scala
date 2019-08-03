package frdomain.ch6
package domain
package app

import cats.implicits._
import frdomain.ch6.domain.repository.interpreter.AccountRepositoryInMemory
import frdomain.ch6.domain.service.interpreter.AccountService._
import frdomain.ch6.domain.service.interpreter.ReportingService._
import frdomain.ch6.domain.service.{Checking, Savings}
import zio.DefaultRuntime


object App extends DefaultRuntime {

  def main(args: Array[String]): Unit = {
    usecase1()
    usecase2()
    usecase3()
    usecase4()
  }

  def usecase1(): Unit = {
    val opens =
      for {
        _ <- open("a1234", "a1name", None, None, Checking)
        _ <- open("a2345", "a2name", None, None, Checking)
        _ <- open("a3456", "a3name", Some(BigDecimal(5.8)), None, Savings)
        _ <- open("a4567", "a4name", None, None, Checking)
        _ <- open("a5678", "a5name", Some(BigDecimal(2.3)), None, Savings)
      } yield (())

    val credits =
      for {
        _ <- credit("a1234", 1000)
        _ <- credit("a2345", 2000)
        _ <- credit("a3456", 3000)
        _ <- credit("a4567", 4000)
      } yield (())

    val c = for {
      _ <- opens
      _ <- credits
      a <- balanceByAccount
    } yield a

    unsafeRunSync(c.provide(new AccountRepositoryInMemory)).fold(
      err => println(err),
      res => res.foreach(println)
    )
    // (a2345,2000)
    // (a5678,0)
    // (a3456,3000)
    // (a1234,1000)
    // (a4567,4000)
  }

  def usecase2(): Unit = {
    val c = for {
      _ <- open("a1234", "a1name", None, None, Checking)
      _ <- credit("a2345", 2000)
      a <- balanceByAccount
    } yield a

    unsafeRunSync(c.provide(new AccountRepositoryInMemory)).fold(
      err => println(err),
      res => res.foreach(println)
    )
    // NonEmptyList(No existing account with no a2345)
  }

  def usecase3(): Unit = {
    val c = for {
      _ <- open("a1234", "a1name", None, None, Checking)
      _ <- credit("a1234", 2000)
      _ <- debit("a1234", 4000)
      a <- balanceByAccount
    } yield a

    unsafeRunSync(c.provide(new AccountRepositoryInMemory)).fold(
      err => println(err),
      res => res.foreach(println)
    )
    // NonEmptyList(Insufficient amount in a1234 to debit)
  }

  def usecase4(): Unit = {
    val c = for {
      a <- open("a134", "a1name", BigDecimal(-0.9).some, None, Savings)
      _ <- credit(a.no, 2000)
      _ <- debit(a.no, 4000)
      b <- balanceByAccount
    } yield b

    unsafeRunSync(c.provide(new AccountRepositoryInMemory)).fold(
      err => println(err),
      res => res.foreach(println)
    )
    // NonEmptyList(Account No has to be at least 5 characters long: found a134, Interest rate -0.9 must be > 0)
  }
}
