package frdomain.ch6
package domain
package service
package interpreter

import cats.data._
import cats.effect.IO
import frdomain.ch6.domain.common._
import frdomain.ch6.domain.model.Account

class InterestPostingServiceInterpreter extends InterestPostingService[Account, Amount] {
  /* def computeInterest = Kleisli[Valid, Account, Amount] { (account: Account) =>
     EitherT {
       IO {
         if (account.dateOfClose isDefined) Left(ClosedAccount(account.no))
         else Account.rate(account).map { r =>
           val a = account.balance.amount
           Right(a + a * r)
         }.getOrElse(Right(BigDecimal(0)))
       }
     }
   }

   def computeTax = Kleisli[Valid, Amount, Amount] { (amount: Amount) =>
     EitherT[IO, AccountException, Amount] {
       IO {
         Right(amount * 0.1)
       }
     }
   }*/
}

object InterestPostingService extends InterestPostingServiceInterpreter
