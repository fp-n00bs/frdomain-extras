package frdomain.ch6
package domain

import cats.data._
import cats.effect.IO
import frdomain.ch6.domain.repository.AccountRepository

package object service {
  type Valid[A] = EitherT[IO, AccountServiceException, A]
  type AccountOperation[A] = Kleisli[Valid, AccountRepository, A]
}
