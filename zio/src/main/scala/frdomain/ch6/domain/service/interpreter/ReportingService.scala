package frdomain.ch6
package domain
package service
package interpreter

import cats.data._
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.repository.AccountRepository
import frdomain.ch6.domain.repository.accountRepository.all
import zio.ZIO


class ReportingServiceInterpreter extends ReportingService[Amount] {

  def balanceByAccount: ErrorOr[Seq[(String, Amount)]] = {
    all.map(seq => seq.map( a => (a.no, a.balance.amount)))
  }
}

object ReportingService extends ReportingServiceInterpreter
