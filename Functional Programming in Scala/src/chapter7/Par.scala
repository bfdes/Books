package chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Future}

import com.sun.tools.internal.xjc.reader.gbind.Choice

import scala.concurrent.duration.TimeUnit
import scala.language.implicitConversions

private case class UnitFuture[A](get: A) extends Future[A] {
  def isDone: Boolean = true
  def get(timeout: Long, unit: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}

sealed trait Future[A] {
  // n.b. Purely functional API with unobservable side-effects
  private[chapter7] def apply(cb: A => Unit): Unit
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  // OLD def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def unit[A](a: A): Par[A] = _ => new Future[A] {
    override private[chapter7] def apply(cb: A => Unit): Unit = cb(a)
  }

  /**
    * Marks a computation for execution off the main thread and returns immediately.
    *
    * @param pa: computation to execute away from the main thread, wrapped in a Par instance
    * @tparam A: Type of the computation's return value
    * @return Par instance which executes off the main thread
    */
  def fork[A](pa: => Par[A]): Par[A] = es => new Future[A] {
    override private[chapter7] def apply(cb: A => Unit): Unit = {
      es.submit(new Callable[Unit] {
        override def call(): Unit = cb(eval(es)(run(es)(pa)))
      })
    }
  }
  /*
  n.b. If the ExecutionContext only one thread then the computation will not result in a deadlock, rather it will run in
  a synchronous manner! (c.f. Ex 7.9).
  */

  /**
    * Helper to evaluate a function r in the context of an ExecutorService.
    *
    * @param es: ExecutorService instance
    * @param r: function to execute asynchronously
    */
  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r})

  /*
  Ex 7.8
  OLD impl.

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get  // Returns a java.util.concurrent.Future
    })

  This implementation does not satisfy fork(x) == x. From the book:
  The body of the function submits a callable to an executor service and the content of that callable is itself executed
  in the context of the same executor service in a blocking manner.

  Suppose the executor service is implemented with a thread pool of size one. Then when the outer callable is submitted
  to the sole thread it will deadlock as the inner callable attempts to access the same thread. (Sketch a diagram.)

  Ex 7.9
  In fact the implementation is not sound for any fixed size thread pool.
  For a thread pool of fixed size n, we are limited to n-1 nested levels of forking.
   */

  def delay[A](p: => Par[A]): Par[A] =
    es => p(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /*
  Ex 7.1, 7.3 TODO
  OLD impl

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
   */

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      override private[chapter7] def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None =>
              ar = Some(a)
            case Some(b) =>
              cb(f(a, b))
          }

          case Right(b) => ar match {
            case None =>
              br = Some(b)
            case Some(a) =>
              cb(f(a, b))
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
    }
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))  // In practice implement from scratch as the second thread spawned does nothing.

  // Ex 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // Ex 7.5
  def sequence[A](list: List[Par[A]]): Par[List[A]] =
    list.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_::_))

  def parMap[A, B](list: List[A])(f: A => B): Par[List[B]] =
    sequence(list.map(asyncF(f)))

  // Ex 7.6
  def parFilter[A](list: List[A])(f: A => Boolean): Par[List[A]] = lazyUnit(list.filter(f))

  // Ex 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = ???

  def choice[A](choice: Par[Boolean])(lhs: Par[A], rhs: Par[A]): Par[A] = ???

  // Ex 7.12
  def choiceMap[K, V](choice: Par[K])(choices: Map[K, Par[V]]): Par[V] = ???

  // Ex 7.13
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => new Future[B] {
      override private[chapter7] def apply(cb: B => Unit): Unit = ???
    }

  // Ex 7.14
  def join[A](a: Par[Par[A]]): Par[A] = ???


  /**
    * Fully evaluates the Par instance within the given context, blocking the current thread while it awaits the result.
    * @param es: context within which to evaluate the Par instance
    * @param p: computation to evaluate
    * @tparam A: return type of the computation
    * @return Evaluated result
    */
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]

    val latch = new CountDownLatch(1)

    p(es) { a => ref.set(a); latch.countDown() }

    latch.await()

    ref.get // n.b. blocks the calling thread, but this is unavoidable if one wishes to receive a result of type A.
  }

}