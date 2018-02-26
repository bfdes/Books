package chapter11

trait Functor[F[_]] {
  // Satisfies the relation `map(fa)(identity) == fa`. Preserves structure.
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // Generalised unzip
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

