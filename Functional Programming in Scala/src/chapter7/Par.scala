package chapter7

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

private case class UnitFuture[A](get: A) extends Future[A] {
  def isDone: Boolean = true
  def get(timeout: Long, unit: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // Marks a computation for concurrent evaluation by run
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Ex 7.1, 7.3 TODO
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  // Ex 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // Ex 7.5
  def sequence[A](list: List[Par[A]]): Par[List[A]] =
    list.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_::_))

  def parMap[A, B](list: List[A])(f: A => B): Par[List[B]] =
    sequence(list.map(asyncF(f)))

  // Ex 7.6 TODO
  def parFilter[A](list: List[A])(f: A => Boolean): Par[List[A]] = ???

  // Fully evaluates a given Par and returns the result, spawning parallel computations as requested by fork
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(a)

}