package chapter8

import chapter6.RNG

/**
  * Abstraction that accepts a random number generator and the number of test cases to be used to verify a property.
  * */
case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Result) {
  // Ex 8.9
  def &&(p: Prop): Prop = Prop((maxsize, n, rng) =>
    run(maxsize, n, rng) match {
      case Passed | Proved => p.run(maxsize, n, rng)
      case f: Falsified => f
    }
  )

  // Ex 8.9
  def ||(p: Prop): Prop = Prop((maxsize, n, rng) =>
    run(maxsize, n, rng) match {
      case Falsified(_, _) => p.run(maxsize, n, rng)
      case p => p
    }
  )
}

object Prop {
  type MaxSize = Int
  type TestCases = Int  // Number of test cases to examine before we consider the property to have passed the test
  type FailedCase = String
  type SuccessCount = Int  // Number of tests that have passed

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val maxProp = Gen.forAll(Gen.listOf(Gen.choose(-10, 10)))( n => {
    val max = n.max
    !n.exists(_ > max)
  })
}
