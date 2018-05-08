package chapter8

import chapter8.Prop.{FailedCase, SuccessCount}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}
