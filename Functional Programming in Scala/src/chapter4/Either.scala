package chapter4

sealed trait Either[+E, +A] {
  // Ex 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >:E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e) => b
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Ex 4.6
  def map2[A, B, C, EA, EB >: EA](ea: Either[EA, A], eb: Either[EB, B])(f: (A, B) => C): Either[EB, C] =
    ea.flatMap(a => eb.map(b => f(a, b)))

  // Ex 4.7
  // def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
  // Doesn't compile for some reason

  def traverse[E, A, B](es: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(List.empty))((a, b) =>
      map2(f(a), b)(_ :: _)
    )  // foldRight will return the first error that occurs, if there is one
}