package chapter3

// Tree, like List, is an example of an ADT (Algebraic Data Type).
// An ADT is defined by one or more data constructors.
// The type is the sum of its constructors, and each data constructor is the product of its arguments.
sealed trait Tree[+A] {
  // Ex 3.25
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => l.size + r.size
  }

  // Ex 3.26 Easy, but has to be done for Tree[Int] specifically

  // Ex 3.27
  // Returns maximum path length from root to any leaf in the tree
  def depth: Int = this match {
    case Leaf(a) => 1
    case Branch(l, r) => l.depth max r.depth
  }

  // Ex 3.28
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  // Ex 3.29
  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
