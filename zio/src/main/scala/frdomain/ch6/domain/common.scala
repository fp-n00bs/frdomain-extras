package frdomain.ch6
package domain

import java.util.Calendar

import cats.data._
import _root_.zio._
import frdomain.ch6.domain.repository.AccountRepository

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime

  type ErrorOr[A] = ZIO[AccountRepository, NonEmptyList[String], A] //DPDPDP hur losa med Generic for repo?
  type ValidationResult[A] = ValidatedNel[String, A]



}
