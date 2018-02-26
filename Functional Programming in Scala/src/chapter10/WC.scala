package chapter10

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC


object WC {
  // Ex 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(ll, m, rl), Part(lr, n, rr)) => Part(ll, m + (if((rl+lr).isEmpty) 0 else 1) + n, rr)
      case (Stub(s), Part(l, words, r)) => Part(s+l, words, r)
      case (Part(l, words, r), Stub(s)) => Part(l, words, r+s)
      case (Stub(l), Stub(r)) => Stub(l+r)
    }

    def zero: WC = Stub("")
  }

  // Ex 10.11 TODO
}