package frdomain.ch6
package domain

import java.util.Calendar
import cats.data._
import cats.implicits._

import _root_.zio._
import cats.data.NonEmptyList
import zio.{IO, UIO, ZIO}
import frdomain.ch6.domain.repository.AccountRepository

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime
  val validationOk = ZIO.succeed("")

  type ErrorOr[A] = ZIO[AccountRepository, AppException, A] //DPDPDP hur losa med Generic for repo?
  type Validation = IO[String, String]

  final case class Accumulated[E <: AppException](all: NonEmptyList[E]) extends AppException {
    def msg = all.map(_.fullMsg).toList.mkString(" ; ")
  }

  implicit class AccumulateErrorsNEL[A](in: Iterable[A]) {
    private def transform[R, E, B](seq: ZIO[R, Nothing, List[Either[E, B]]]) = {
      seq.flatMap { list =>
        val accumulated = list.traverse( _.toValidatedNel)
        ZIO.fromEither(accumulated.toEither)
      }
    }

    def accumulateNEL[R, E, B](f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], List[B]] = {
      transform(ZIO.foreach(in){ x => f(x).either })
    }

    def accumulateParNELN[R, E, B](n: Long)(f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], List[B]] = {
      transform(ZIO.foreachParN(n)(in){ x => f(x).either })
    }
  }

  implicit class AccumulateErrors[A](in: Iterable[A]) {

    def accumulate[R, E <: AppException, B](f: A => ZIO[R, E, B]): ZIO[R, Accumulated[E], List[B]] = {
      in.accumulateNEL(f).mapError(errors => Accumulated(errors))
    }

    def accumulateParN[R, E <: AppException, B](n: Long)(f: A => ZIO[R, E, B]): ZIO[R, Accumulated[E], List[B]] = {
      in.accumulateParNELN(n)(f).mapError(errors => Accumulated(errors))
    }
  }


}
