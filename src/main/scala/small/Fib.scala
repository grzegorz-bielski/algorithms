package small

object Fib:
  enum Strategy:
    case NaiveRecursive

  def apply(
    strategy: Strategy,
    n: Int
  ): Int = 
    strategy match
        case Strategy.NaiveRecursive => fib1(n)
    

  def fib1(n: Int): Int =
    if n < 2 then n else fib1(n - 1) + fib1(n - 2)
