package chapter6

// Ex 6.11
sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Input {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(in => {
      val out = inputs.foldRight(in)({
        case (_, m) if m.candies == 0 => m
        case (Turn, m) if m.locked => m
        case (Coin, m) if !m.locked => m
        case (Turn, Machine(false, candies, coins))  => Machine(locked=true, candies-1, coins)
        case (Coin, Machine(true, candies, coins)) => Machine(locked=false, candies, coins-1)
      })
      ((out.candies, out.coins), out)
    })
}
