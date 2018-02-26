package chapter5

trait Stream[+A] {
  import Stream._
  // Ex 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h()::t().toList
    case Empty => Nil
  }

  // Ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  // Ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  } // Ex 5.5 foldRight(Empty)((a, b) => if p(a) Cons(a, b) else Empty)

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }  // If f does not evaluate its second argument the recursion never occurs

  // Ex 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Ex 5.6
  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  // Ex 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def append[B>:A](stream: => Stream[B]): Stream[B] =
    foldRight(stream)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  // Ex 5.13
  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, stream))({
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    })
  def zipAll[B](stream: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, stream))({
      case (Empty, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    })

  // Ex 5.14
  def startsWith[B >: A](stream: Stream[B]): Boolean =
    zipAll(stream).takeWhile(!_._2.isEmpty).forAll({
      case (Some(a), Some(b)) => a == b
    })

  // Ex 5.15
  def tails: Stream[Stream[A]] =
    unfold(this)({
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case Empty => None
    }) append Empty

  // Ex 5.16 TODO
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Ex 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // Ex 5.10
  def fibs: Stream[Int] = {
    def fibs(prev: Int, curr: Int): Stream[Int] =
      cons(prev, fibs(curr, prev+curr))
    fibs(0, 1)
  }

  // Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map({case (a, s) => cons(a, unfold(s)(f))})
    .getOrElse(empty[A])

  // Ex 5.12
  // def constant[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))
  // def ones: Stream[Int] = constant(1)
  // def from(n: Int): Stream[Int] = unfold(n)((i => Some((i, i+1))))
  // def fibs: Stream[Int] = unfold((0, 1))({case (prev, curr) => Some((prev, (curr, prev+curr)))})
}
