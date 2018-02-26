package chapter10

import chapter3.{Branch, Leaf, Tree}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Ex 10.15
  def toList[A]: List[A] = foldRight(this[A])(List.empty)(_ :: _)
}

object Foldable {
  // Ex 10.12
  val folableList: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as.map(f))(m.zero)(m.op)
  }

  val foldableSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as.map(f))(m.zero)(m.op)
  }

  val foldableStream: Foldable[Stream] = new Foldable[Stream] {
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldMap[A, B](as: Stream[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as.map(f))(m.zero)(m.op)
  }

  // Ex 10.13
  val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a) => f(z, a)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(a) => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B =
      as.fold(f)(m.op)
  }

  // Ex 10.14
  val foldableOption: Foldable[Option] = new Foldable[Option] {
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.map(f(z, _)).getOrElse(z)

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.map(f(_, z)).getOrElse(z)

    override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
      as.map(f).getOrElse(m.zero)
  }

}
