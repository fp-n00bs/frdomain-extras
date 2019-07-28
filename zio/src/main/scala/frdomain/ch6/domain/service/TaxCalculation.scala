package frdomain.ch6.domain.service

import cats.data._

trait TaxCalculation[Amount] {
  def computeTax: Kleisli[Valid, Amount, Amount]
}
