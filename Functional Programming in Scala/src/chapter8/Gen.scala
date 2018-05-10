package chapter8

import chapter5.Stream
import chapter6.{RNG, State}

case class Gen[+A](sample: State[RNG, A]) {
  // Ex 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  // Ex 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  /**
    * Generates a property for the given generator and predicate function.
    * @param g generator, sampled to form test cases
    * @param f predicate function to verify
    * @tparam A type of elements obtained from sampling the generator
    * @return property to verify
    */
  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) => randomStream(g)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)  // If a test fails record the failed case and number that have passed so far
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }  // If a test case generates an exception record it in the result
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1  // We must generate the same number of random cases for each size
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))  // g(i) will give us a generator for the given size, e.g. List(), List(j), List(j, k)
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)  // Fix the number of test cases
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  // Ex 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
    State(RNG.nonNegativeInt).map(start + _ % (stopExclusive-start))  // Unbiased if the interval divides into Int.Max evenly (?)
  )

  // Ex 8.5
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  // Ex 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen(
    State(rng => RNG.boolean(rng)).flatMap(if(_) g1.sample else g2.sample)
  )

  // Ex 8.8 TODO
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

  // Ex 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size, g))

  // Ex 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size max 1, g))

  /** Generates an infinite stream of A values by repeatedly sampling a generator*/
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
