package frdomain.ch6
package domain
package service

import cats.data._
import frdomain.ch6.domain.repository.AccountRepository
import zio.ZIO
trait ReportingService[Amount] {
  type ReportOperation[A] = ZIO[AccountRepository, AccountServiceException, A]
  def balanceByAccount: AccountOperation[Seq[(String, Amount)]]
}
