package chapter8

import chapter6.RNG


case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Result) {
  // Ex 8.9
  def &&(p: Prop): Prop = Prop((maxsize, n, rng) =>
    run(maxsize, n, rng) match {
      case Passed => p.run(maxsize, n, rng)
      case f: Falsified => f
    }
  )

  // Ex 8.9
  def ||(p: Prop): Prop = Prop((maxsize, n, rng) =>
    run(maxsize, n, rng) match {
      case Passed => Passed
      case _ => p.run(maxsize, n, rng)
    }
  )
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  val maxProp = Gen.forAll(Gen.listOf(Gen.choose(-10, 10)))( n => {
    val max = n.max
    !n.exists(_ > max)
  })
}