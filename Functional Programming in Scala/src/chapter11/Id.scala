package chapter11

case class Id[A](value: A) {
  // Ex 11.17
  def unit(a: => A): Id[A] = Id(a)

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = Id(f(value))
}
