package frdomain.ch6
package domain
package service
package interpreter

import cats.data._
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.repository.AccountRepository


class ReportingServiceInterpreter extends ReportingService[Amount] {

  def balanceByAccount: ReportOperation[Seq[(String, Amount)]] = Kleisli[Valid, AccountRepository, Seq[(String, Amount)]] { (repo: AccountRepository) =>
    EitherT {
      repo.all.map {
        case Left(errs) => Left(MiscellaneousDomainExceptions(errs))
        case Right(as) => Right(as.map(a => (a.no, a.balance.amount)))
      }
    }
  }
} 

object ReportingService extends ReportingServiceInterpreter
