package chapter6


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, newRNG) = rng.nextInt
    (if(int < 0) -(int+1) else int, newRNG)
  }

  // Ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (int, newRNG) = nonNegativeInt(rng)
    (int / (Int.MaxValue.toDouble + 1.0), newRNG)
  }  // Ex 6.5 def double(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1.0))

  // Ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // Ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count==0) {
      (Nil, rng)
    } else {
      val (h, rng1) = rng.nextInt
      val (t, rng2) = ints(count-1)(rng1)
      (h::t, rng2)
    }
  }

  // Ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  // Ex 6.7
  def sequence[A](list: List[Rand[A]]): Rand[List[A]] =
    list.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_::_))

  // Ex 6.8
  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = s(rng)
      f(a)(rng1)
    }

  // Ex 6.9
  // def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(f andThen unit)
  // def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

}