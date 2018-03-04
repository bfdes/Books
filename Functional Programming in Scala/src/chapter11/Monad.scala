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
/*
Associativity laws.
We show the two formulations, in terms of flatMap and compose, are equivalent.

Assume compose(compose(f, g), h) = compose(f, compose(g, h)).
Working with the LHS:
compose(compose(f, g), h) = a -> flatMap(compose(f, g)(a))(h)
                          = a -> flatMap(flatMap(f(a))(g))(h)
                          = a -> f(a).flatMap(g).flatMap(h) after converting to infix notation.
Working with the RHS:
compose(f, compose(g, h)) = a -> flatMap(f(a))(b -> flatMap(g(b))(h))
                          = a -> f(a).flatMap(b -> g(b).flatMap(h))
If we let x = f(a) this gives x.flatMap(g)(h) = x.flatMap(b -> g(b).flatMap(h)), which is what we were after.

Now assume the above result instead.
x.flatMap(g).flatMap(h)
=> a -> f(a).flatMap(g).flatMap(h) = a -> (compose(f, g)(a)).flatMap(h)
                                   = compose(compose(f, g), h)
Working with the RHS,
a -> f(a).flatMap(b -> g(b).flatMap(h)) = a -> f(a).flatMap(compose(g, h))
                                        = compose(f, compose(g, h)).
This gets us back to where we started. QED.
 */

// Ex 11.10
/*
Identity laws.
We show that the two formulations, in terms of flatMap and compose, are equivalent.

Assume compose(f, unit) = f (Left identity) and compose(unit, f) = f (Right identity).
Then the left identity gives a -> flatMap(f(a))(unit) = f. Applying a to both sides yields
flatMap(f(a))(unit) = f(a) or flatMap(x)(unit) = x if one lets x = f(a).

The right identity gives a -> flatMap(unit(a))(f) = f or flatMap(unit(a))(f) = f(a).

Now assume the flatMap formulations hold instead.
flatMap(x)(unit) = x => a -> flatMap(f(a))(unit) = f or compose(f, unit) = f.
flatMap(unit(a))(f) = f(a) => a -> flatMap(unit(a))(f) = f or compose(unit, f) = f.
 */


// Ex 11.11
/*
We prove that the Identity laws hold for Option as Monad.
In the flatMap formulation flatMap(x)(unit) = x and flatMap(unit(a))(f) = f(a).

If x = None then the left identity holds trivially.
If x = Some(a) then flatMap(x)(unit) = flatMap(Some(a))(Some(_)) = Some(a) = x.

And the right identity simplifies to flatMap(unit(a))(f) = flatMap(Some(a))(f) = f(a).
If f(a) = None then both sides agree. If f(a) = Some(b) then both sides still agree.
 */


// Ex 11.14 Very messy when not written in terms of compose...
// Identity: join(map(x)(unit)) == x and join(map(unit(y))(f)) == f(y)
// Associativity: join(map(join(map(x)(f)))(g)) == join(map(x)(a => join(map(f(a))(g))))

// Ex 11.15 TODO
// Need to complete chapters 7 and 8 first

// Ex 11.16 Strange exercise.

// Ex 11.18
/*
The signatures are
- def replicateM[A](n: Int, ma: State[A]): State[List[A]]
- def map2[A, B, C](ma: State[A], mb: State[B])(f: (A, B) => C): State[C]
- def sequence[A](list: List[State[A]]): State[List[A]]
Another strange exercise.
 */

// Ex 11.19 TODO
