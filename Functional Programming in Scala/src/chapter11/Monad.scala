package chapter11

trait Monad[M[_]] extends Functor[M]{
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Ex 11.3
  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, b) => map2(f(a), b)(_ :: _))

  def sequence[A](la: List[M[A]]): M[List[A]] =
    traverse(la)(identity)

  // Ex 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  // Ex 11.5
  def filterM[A](la: List[A])(f: A => M[Boolean]): M[List[A]] =
    la.foldRight(unit(List.empty[A]))((a, b) =>
      map2(f(a), b)((p, unwrapped) => if(p) a :: unwrapped else unwrapped)
    )

  // Ex 11.7
  // Kleisli composition function
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Ex 11.8
  // def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: M[A]) => ma, f)(ma)

  // Ex 11.12
  def join[A](mma: M[M[A]]): M[A]  = flatMap(mma)(identity)

  // Ex 11.13
  // def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

}

object Monad {
}

// Ex 11.9
// Paper exercise. Remember to convert to infix notation.

// Ex 11.10, 11.11
// Paper exercise.

// Ex 11.14 Very messy when not written in terms of compose...
// Identity: join(map(x)(unit)) == x and join(map(unit(y))(f)) == f(y)
// Associativity: join(map(join(map(x)(f)))(g)) == join(map(x)(a => join(map(f(a))(g))))

// Ex 11.15 TODO

// Ex 11.16

// Ex 11.18

// Ex 11.19
