package chapter4

sealed trait Option[+A] {

  // Ex 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if(f(a)) Some(a) else None)

  // Ex 4.2 Doesn't belong in this file really

}
case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

object Option {
  // Ex 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  // Ex 4.4
  def sequence[A](list: List[Option[A]]): Option[List[A]] = traverse(list)(identity)

  // Ex 4.5
  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    list.foldRight[Option[List[B]]](Some(List()))((a, b) =>
      map2(f(a), b)(_ :: _)
    )  // Pass over and construct a new list in linear time
}
