package chapter14

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    override def run(s: S): (B, S) = {
      val (a, sb) = self.run(s)
      (f(a), sb)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override def run(s: S): (B, S) = {
      val (b, sb) = self.run(s)
      f(b).run(sb)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      override def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.size)

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), a)
    }
  }

  def fromList[S, A:Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })

  // Ex 14.1
  def fill(xs: Map[Int, A]): ST[S, Unit] = {
    xs.foldRight(ST[S, Unit]())({case ((i, v), _) =>
      write(i, v)
    })
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()

  // Ex 14.2
  def quicksort(xs: List[Int]): List[Int] = if(xs.isEmpty) xs else {

    def partition[S](a: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] =
      for {
        pivotVal <- a.read(pivot)
        _ <- a.swap(pivot, r)
        jr <- STRef(n)
        _ <- (n until r).foldRight(ST[S, Unit])((i, _) => for {
          v <- a.read(i)
          _ <- if(v < pivotVal) for {
            j <- jr.read
            _ <- a.swap(i, j)
            _ <- jr.write(j+1)
          } yield () else ST[S, Unit]
        } yield ())
      }

    def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = if(n < r) {
      for {
        pi <- partition(a, n, r, n + (n - r) / 2)
        _ <- qs(a, n, pi-1)
        _ <- qs(a, n, pi+1)
      } yield ()
    } else ST[S, Unit]()
  }
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    }
}