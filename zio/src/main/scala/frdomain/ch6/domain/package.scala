package frdomain.ch6
package domain

import cats.data._
import cats.effect.IO
import frdomain.ch6.domain.repository.AccountRepository
import zio.ZIO

package object service {
  type AccountOperation[A] = ZIO[AccountRepository, AccountServiceException, A]

}
