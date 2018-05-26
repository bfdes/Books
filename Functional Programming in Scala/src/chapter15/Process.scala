package chapter15

import chapter12.Monad

/*
Transforms an input of type I to an output of type O.
 */
sealed trait Process[I, O] {

  import Process._

  def apply(s: Stream[I]): Stream[O] = this match {
    case Emit(h, t) => Stream(h) ++ t(s)
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case e => recv(None)(e)  // empty
    }
    case Halt() => Stream.Empty
  }

  def map[O2](f: O => O2): Process[I,O2] = this |> lift(f)

  def ++(p: => Process[I,O]): Process[I,O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)  // Restart the process if it stops on its own
      case Await(recv) => Await {
        case None => recv(None)  // Don't repeated if terminated from source
        case s => go(recv(s))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  // Ex 15.5
  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt() => Halt()
    case Emit(h, t) => Emit(h, this |> t)
    case Await(f) => this match {
      case Halt() => Halt() |> f(None)
      case Emit(h, t) => t |> f(Some(h))
      case Await(g) => Await(i => g(i) |> p2)
    }
  }

  // Ex 15.6
  def zipWithIndex: Process[I, (O, Int)] = {
    def go(p: Process[I, O], n: Int): Process[I, (O, Int)] = p match {
      case Halt() => Halt()
      case Emit(h, t) => Emit((h, n), go(t, n+1))
      case Await(recv) => Await {i => go(recv(i), n)}
    }
    go(this, 0)
  }

  // Ex 15.7
  def feed(maybeI: Option[I]): Process[I, O] = this match {
    case Halt() => Halt()
    case Emit(h, t) => Emit(h, t feed maybeI)
    case Await(recv) => recv(maybeI)
  }
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]


object Process {
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def id[I]: Process[I, I] = lift(identity)

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I, O](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }
    go(0.0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await {
      case Some(i) => f(i, z) match {
        case (o, s2) => Emit(o, loop(s2)(f))
      }
      case None => Halt()
    }

  // Ex 15.1
  def take[I](n: Int): Process[I, I] =
    Await {
      case Some(i) if n > 0 => Emit(i, take(n-1))
      case _ => Halt()
    }

  def drop[I](n: Int): Process[I, I] = if(n < 0) id else Await {
    case Some(_) => drop(n-1)
    case None => Halt()
  }  // Slightly cleaner implementation by using id from fpinscala GitHub repo


  def takeWhile[I](f: I => Boolean): Process[I, I] =
    Await {
      case Some(i) if f(i) => Emit(i, takeWhile(f))
      case _ => Halt()
    }

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    Await {
      case Some(i) if f(i) => dropWhile(f)
      case Some(i) => Emit(i, id)
      case None => Halt()
    }

  // Ex 15.2, 15.4
  def count[I]: Process[I, Int] = loop(0)((_, count) => (count+1, count+1))
  // def sum: Process[Double, Double] = loop(0.0)((d, sum) => (d+sum, d+sum))

  // Ex 15.3
  def mean: Process[Double, Double] = {
    def go(acc: Double, i: Int): Process[Double, Double] =
      Await {
        case Some(d) => Emit((d+acc) / i, go(d+acc, i+1))
        case None => Halt()
      }
    go(0.0, 1)
  }

  // Ex 15.7 A, B less tedious to write than O, O2
  def zip[I, A, B](p: Process[I, A], p2: Process[I, B]): Process[I, (A, B)] = (p, p2) match {
    case (Halt(), _) | (_, Halt()) => Halt()
    case (Emit(ha, ta), Emit(hb, tb)) => Emit((ha, hb), zip(ta, tb))
    case (Await(f), aOrE) => Await(i => zip(f(i), aOrE.feed(i)))
    case (aOrE, Await(g)) => Await(i => zip(aOrE.feed(i), g(i)))  // Is feed really quite right here?
  }

  // def mean: Process[Double, Double] = zip(sum, count).map { case (acc, i) => acc / i }


  // Ex 15.8
  def exists[I](f: I => Boolean): Process[I, Boolean] = Await {
    case Some(i)  => if(f(i)) Emit(true) else Emit(false, exists(f))
    case None => Halt()
  }  // n.b. This is the form that halts and yields all intermediate results

  def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] =
    new Monad[({ type f[x] = Process[I,x]})#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)
      def flatMap[O,O2](p: Process[I,O])(f: O => Process[I,O2]): Process[I,O2] = p flatMap f
    }
}