package chapter3

sealed trait List[+A] {
  import List._
  // Ex 3.1
  // Will match third statement, result will be 3

  // Ex 3.2
  def tail: List[A] = this match {
    case Nil => sys.error("Empty list")
    case Cons(_, tail) => tail
  }

  // Ex 3.3
  def setHead[B >: A](head: B): List[B] = this match {
    case Nil => List(head)
    case Cons(_, tail) => Cons(head, tail)
  }

  // Ex 3.4
  def drop(n: Int): List[A] = {
    assert(n > 0)
    this match {
      case Nil => Nil
      case Cons(_, tail) if n > 1 => tail drop(n-1)
      case Cons(_, tail) if n == 1 => tail
    }
  }

  // Ex 3.5
  def dropWhile(f: A => Boolean): List[A] = this match {
    case Cons(head, tail) if f(head) => tail dropWhile f
    case _ => this
  }  // Tips for writing these: Be specific and then simplify

  // Ex 3.6
  def init: List[A] = this match {
    case Nil => sys.error("Empty list")
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, tail.init)
  }

  // foldRight is a generalised recursion
  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(head, tail) => f(head, tail.foldRight(z)(f))
  }

  // Ex 3.7
  // No. foldRight takes its arguments by value, so f cannot choose whether to evaluate its second argument.
  // Otherwise the recursion could be "halted." We need Streams for this.

  // Ex 3.8
  // Earlier the book said foldRight has the effect of replacing the constructors of the list, Nil and Cons,
  // with z and f. Thus foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) will construct a new list List(1, 2, 3).

  // Ex 3.9
  def length: Int = foldRight(0)((_, len) => len+1)

  // Ex 3.10 n.b. tail-recursive
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Nil => z
    case Cons(head, tail) => tail.foldLeft(f(z, head))(f)
  }

  // Ex 3.12
  def reverse: List[A] = foldLeft[List[A]](Nil)((b, a) => Cons(a, b))

  // Ex 3.13
  // Can write foldRight as foldLeft composed with reverse

  // Ex 3.14
  // Cost of this op is linear in the length of the original list
  def append[B >: A](l: List[B]): List[B] = foldRight(l)(Cons(_, _))

  // Ex 3.18
  // Also covers 3.16, 3.17
  def map[B](f: A => B): List[B] = this match {
    case Cons(head, tail) => Cons(f(head), tail.map(f))
    case Nil => Nil
  }

  // Ex 3.19
  def filter(f: A => Boolean): List[A] = this match {
    case Cons(head, tail) if f(head) => Cons(head, tail.filter(f))
    case Cons(_, tail) => tail filter f
    case Nil => Nil
  }

  def flatMap[B](f: A => List[B]): List[B] = concatenate(this.map(f))

  // Ex. 3.21
  // def filter(f: A => Boolean): List[A] = flatMap(a => if(f(a)) List(a) else Nil)

  def startsWith[B >: A](l: List[B]): Boolean =
    (this.length >= l.length) && zip(this, l).foldRight(true)({ case ((l, r), acc) => (l == r) && acc})
}

case object Nil extends List[Nothing]
case class Cons[A](head: A, override val tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Ex 3.11
  def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)
  def product(l: List[Double]): Double = l.foldLeft(1.0)(_ * _)

  // Ex 3.15
  // Linear in the total length of all lists
  def concatenate[A](l: List[List[A]]): List[A] =
    l.foldRight(List[A]())(_ append _)

  // Ex 3.23
  // Also covers 3.22
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match {
      case (Nil, Cons(b, bs)) => Nil
      case (Cons(a, as), Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    }

  def zip[A, B](l: List[A], r: List[B]): List[(A, B)] = zipWith(l, r)((_, _))

  // Ex 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Cons(_, t) => sup.startsWith(sub) || hasSubsequence(t, sub)
    case Nil => sub == Nil
  }
}
