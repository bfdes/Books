package chapter12

import chapter11.{Functor, Id}
import language.higherKinds
import language.implicitConversions


trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((aToB, a) => aToB(a))
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // Ex 12.1
  def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // Ex 12.2
  // The applicative interface can be formulated using apply and unit primitives instead.
  // Of course, apply can also be defined in terms of map2 (unit isn't needed) primitives
  // def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((aToB, a) => aToB(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  // Ex 12.3
  // If we partially curried f then map2 could be used to implement map3, and map3 to implement map4
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val cToD = apply(map(fa)(f.curried))(fb)
    apply(cToD)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val cToDToE = apply(map(fa)(f.curried))(fb)
    val dToE = apply(cToDToE)(fc)
    apply(dToE)(fd)
  }

  // Ex 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  // Ex 12.9
  def compose[G[_]](G: Applicative[G]) = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](ca: => A): F[G[A]] = self.unit(G.unit(ca))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
    }
  }

  // Ex 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.keySet.foldRight(this.unit(Map[K, V]()))((k, wrapped) =>
      map2(wrapped, ofa(k))(_.updated(k, _))
    )
}


object Applicative {
  // Ex 12.6
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(eah, eat), Failure(ebh, ebt)) => Failure(ebh, Vector(eah) ++ eat ++ ebt)
      case (Failure(eh, et), _) => Failure(eh, et)
      case (_, Failure(eh, et))=> Failure(eh, et)
    }
  }

  // For Ex 12.14
  def idApplicative: Applicative[Id] = new Applicative[Id] {
    def unit[A](a: => A) = Id.apply(a)

    override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] = fa.flatMap(a => fab.map(f => f(a)))
  }
}

// Ex 12.7
/*
We prove that the left (map2(unit(()), fa)((_, a) -> a) = fa) and right identities (map2(fa, unit(()))((a, _) -> a))
hold for the implementation of map2 above.

Left identity:
map2(unit(()), fa)((_, a) -> a) = flatMap(unit(()))(b -> map(fa)(a -> a))
                                = flatMap(unit(()))(b -> fa)
                                = fa

Right identity:
map2(fa, unit(()))((a, _) -> a) = flatMap(fa)(a -> map(unit(()))(b -> a))
                                = flatMap(fa)(a -> unit(a))
                                = map(fa)(identity)
                                = fa  since Monads obey the Functor laws.

Associativity
We show that product(product(fa, fb), fc) = map(product(fa, product(fb, fc)))(assoc)
where assoc = (a, (b, c)) -> ((a, b), c).

product(fa, product(fb, fc))  = map2(fa, product(fb, fc)((_, _)))
                              = flatMap(fa)(a -> map(product(fb, fc))(p -> (a, p)))
                              = flatMap(fa)(a -> flatMap(product(fb, fc))(p -> unit((a, p))))
                              = flatMap(fa)(a -> flatMap(map2(fb, fc)((_, _)))(p -> unit((a, p))))
                              = flatMap(fa)(a -> unit((a, (b, c))))
                              = unit((a, (b, c)))

After liberal use of the Monad definitions of map2, product and map.

Similarly, product(product(fa, fb), fc) = unit(((a, b), c)) and the result follows.


Naturality of Product
We show that map2(fa, fb)(productF(f, g)) = product(map(fa)(f), map(fb)(g)),
where productF(f: I -> O, g: I2 -> O2) = (i, i2) -> (f(i), g(i2)).

map2(fa, fb)(productF(f, g))  = flatMap(fa)(a -> map(fb)(b -> productF(f, g)(a, b))) definition of map2 for monads
                              = flatMap(fa)(a -> map(fb)(b -> (f(a), g(b))))
                              = map2(fa.map(f), fb.map(g))((_, _)) using the definition again
                              = product(fa.map(f), fb.map(g))
QED
 */

// Ex 12.10 TODO
