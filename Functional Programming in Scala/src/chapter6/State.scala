package chapter6

// Ex 16.10
case class State[S, +A](run: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] = flatMap(f andThen unit)

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object State {
  // Ex 16.10
  def unit[A, S](a: A): State[S, A] = State(s => (a, s))

  def map2[A, B, C, S](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sa.flatMap(a => sb.map(b => f(a, b)))

  def sequence[A, S](list: List[State[S, A]]): State[S, List[A]] =
    list.foldRight(unit[List[A], S](Nil))((a, b) => map2(a, b)(_::_))
}
