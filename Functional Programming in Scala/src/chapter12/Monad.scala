package chapter12

// All Monads are applicative Functors, but not all Applicative Functors are Monads
// See Ex 12.7
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((aToB, a) => aToB(a))

  // Ex 12.11
  /*
  def compose[G[_]](G: Monad[G]) = {
    val self = this
    new Monad[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        self.flatMap(fa)(ga => G.flatMap(ga)(a => ???))
        // We would need f to have the return type G[F[B]] instead and a way of transforming G[F[B]] to F[G[B]]
    }
  }
   */
}

object Monad {
  // Ex 12.5
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }
}

// Ex 12.4
// streamApplicative.sequence is a generalised zip of streams.