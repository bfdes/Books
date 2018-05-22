package chapter13

import chapter12.Monad
import chapter7.Par._

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], k: A => Free[F, B]) extends Free[F, B]


object Free {
  // Translate a type that doesn't have a monad, like `Console`, to one that does
  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }

  type ~>[F[_], G[_]] = Translate[F, G]

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A ): Par[A]= unit(a)
    def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = fork(flatMap(fa)(f))
  }

  // Ex 13.1
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f] {
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f

    override def unit[A](a: => A): Free[F, A] = Return(a)
  }

  // Ex 13.2
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y flatMap (a => f(a) flatMap g))
    }
  }

  // Ex 13.3
  @annotation.tailrec
  def run[F[_], G[_], A](free: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(x, f) => x match {
      case Suspend(r) => G.flatMap(t(r))(a => run(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates all these cases")
    }
  }

  // Ex 13.4 TODO

  @annotation.tailrec
  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  private def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g)) // Have assumed Monad associativity relation
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }
}
