package frdomain.ch6
package domain

import java.util.Calendar

import cats.data._
import cats.implicits._
import cats.kernel.Order
import _root_.zio._
import _root_.zio.syntax._
import frdomain.ch6.domain.common.errors.{AppError, SystemError}

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime

  type PureResult[A] = Either[AppError, A]
  type IOResult[A] = ZIO[Any, AppError, A]

  object IOResult {
    def effect[A](error: String)(effect: => A): IOResult[A] = {
      IO.effect(effect).mapError(ex => SystemError(error, ex))
    }
    def effect[A](effect: => A): IOResult[A] = {
      this.effect("An error occured")(effect)
    }
    def effectM[A](error: String)(ioeffect: => IOResult[A]): IOResult[A] = {
      IO.effect(ioeffect).foldM(
        ex  => SystemError(error, ex).fail,
        res => res
      )
    }
    def effectM[A](ioeffect: => IOResult[A]): IOResult[A] = {
      effectM("An error occured")(ioeffect)
    }
  }
  object errors {



    trait AppError {
      def msg: String
      def fullMsg = this.getClass.getSimpleName + ": " + msg
    }

    final case class SystemError(msg: String, cause: Throwable) extends AppError {
      override def fullMsg: String = super.fullMsg + s"; cause was: ${cause.getMessage}"
    }

    final case class Unexpected(msg: String) extends AppError

    final case class Inconsistency(msg: String) extends AppError

    trait BaseChainError[E <: AppError] extends AppError {
      def cause: E
      def hint: String
      def msg = s"${hint}; cause was: ${cause.fullMsg}"
    }

    final case class Chained[E <: AppError](hint: String, cause: E) extends BaseChainError[E] {
      override def fullMsg: String = msg
    }

    final case class Accumulated[E <: AppError](all: NonEmptyList[E]) extends AppError {
      implicit val ord = new Order[E]() {
        override def compare(x: E, y: E): Int = String.CASE_INSENSITIVE_ORDER.compare(x.fullMsg, y.fullMsg)
      }
      def msg = all.map(_.fullMsg).toList.mkString(" ; ")
      def deduplicate = {
        Accumulated(all.distinct)
      }
    }

    implicit class IOChainError[R, E <: AppError, A](res: ZIO[R, E, A]) {
      def chainError(hint: String): ZIO[R, AppError, A] = res.mapError(err => Chained(hint, err))
    }

    implicit class PureChainError[R, E <: AppError, A](res: Either[E, A]) {
      def chainError(hint: String): Either[AppError, A] = res.leftMap(err => Chained(hint, err))
    }

    implicit class PureToIoResult[A](res: PureResult[A]) {
      def toIO: IOResult[A] = ZIO.fromEither(res)
    }

    implicit class OptionToIoResult[A](res: Option[A]) {
      def notOptional(error: String) = res match {
        case None    => Inconsistency(error).fail
        case Some(x) => x.succeed
      }
    }

    implicit class MandatoryOptionIO[R, E <: AppError, A](res: ZIO[R, E, Option[A]]) {
      def notOptional(error: String) = res.flatMap( _.notOptional(error))
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

      def accumulate[R, E <: AppError, B](f: A => ZIO[R, E, B]): ZIO[R, Accumulated[E], List[B]] = {
        in.accumulateNEL(f).mapError(errors => Accumulated(errors))
      }

      def accumulateParN[R, E <: AppError, B](n: Long)(f: A => ZIO[R, E, B]): ZIO[R, Accumulated[E], List[B]] = {
        in.accumulateParNELN(n)(f).mapError(errors => Accumulated(errors))
      }
    }


  }
}
