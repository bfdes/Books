package chapter10

trait Monoid[A] {
  // Associative binary operation op on A
  def op(a1: A, a2: A): A

  // An identity zero
  def zero: A
}

object Monoid {
  // Ex 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, j: Int): Int = i + j

    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, j: Int): Int = i * j

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  // Ex 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  // Ex 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    def zero: A => A = identity
  }

  // If `op` is commutative then `dual(op)` is the same as `op`
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)

    def zero: A = m.zero
  }

  // Ex 10.4 TODO

  def concatenate[A](list: List[A], m: Monoid[A]): A =
    list.foldLeft(m.zero)(m.op)

  // Ex 10.5
  def foldMap[A, B](list: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(list.map(f), m)

  // Ex 10.6
  def foldRight[A, B](list: List[A])(z: B)(f: (A, B) => B): B =
    foldMap[A, B => B](list, endoMonoid)(f.curried)(z)

  def foldLeft[A, B](list: List[A])(z: B)(f: (B, A) => B): B =
    foldMap[A, B => B](list, dual(endoMonoid))(a => b => f(b, a))(z)
  // `dual(endoMonoid)` is necessary because we need to reverse the order in which the concatenation occurs (left-associative)

  // Ex 10.7
  def foldMap[A, B](v : IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if(v.isEmpty) {
      m.zero
    } else if(v.length == 1) {
      f(v(0))
    } else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMap(l, m)(f), foldMap(r, m)(f))
    }

  // Ex 10.8 TODO

  // Ex 10.9
  // Represent each possibly ordered section with Some(min, max, isOrdered), None if an empty section
  def isOrdered(v: IndexedSeq[Int]): Boolean =
    foldMap(v, new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (a1, a2) match {
          case (Some((i, j, k)), Some((l, m, n))) => Some((i min l, j max m, (k && n) && (k <= n)))  // Account for possible overlap
          case (pieceOrNone, None) => pieceOrNone
          case  (None, pieceOrNone) => pieceOrNone
        }

      def zero = None
    })(i => Some((i, i, true))) match {
      case Some((_, _, ordered)) => ordered
      case None => true
    }

  // Ex 10.16
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) = (a.op(x._1, y._1), b.op(x._2, y._2))

      def zero: (A, B) = (a.zero, b.zero)
    }

  // Ex 10.17
  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(a1: A => B, a2: A => B): A => B =
        a => b.op(a1(a), a2(a))

      def zero: A => B =
        a => b.zero
    }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) {(acc, k) =>
          acc.updated(k, v.op(a1.getOrElse(k, v.zero), a2.getOrElse(k, v.zero)))
        }
      def zero: Map[K, V] = Map[K, V]()
    }

  // Ex 10.18
  def bag[A](v: IndexedSeq[A]): Map[A, Int] = {
    val m = mapMergeMonoid[A, Int](intAddition)
    foldMap(v, m)(a => Map(a -> 1))
  }
}
