package frdomain.ch6
package domain
package service

import cats.data._
import frdomain.ch6.domain.repository.AccountRepository
trait ReportingService[Amount] {
  type ReportOperation[A] = Kleisli[Valid, AccountRepository, A]

  def balanceByAccount: ReportOperation[Seq[(String, Amount)]]
} 
