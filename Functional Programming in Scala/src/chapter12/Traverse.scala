package chapter12

import chapter10.Monoid
import chapter11.{Functor, Id}

trait Traverse[F[_]] extends Functor[F]{

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(identity)

  // Ex 12.14
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(a => Id(f(a))).value
}

object Traverse {
  // Ex 12.13
  def listTraverse = new Traverse[List] {
    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B]))((a, b) => G.map2(f(a), b)(_::_))
  }

  def optionTraverse = new Traverse[Option] {
    def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa.fold[G[Option[B]]](G.unit(None))(a => G.map(f(a))(Some.apply))
  }

  def treeTraverse = new Traverse[Tree] {
    def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(ta => traverse(ta)(f)))(Tree(_, _))
  }
}
