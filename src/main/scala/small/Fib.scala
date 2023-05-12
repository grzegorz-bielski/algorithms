package small

sealed trait Fib extends (Int => BigInt)
object Fib:
  object NaiveRecursive extends Fib:
    def apply(n: Int): BigInt =
      if n < 2 then n else apply(n - 1) + apply(n - 2)

  object NaiveRecursiveMemoized extends Fib:
    import scala.collection.mutable

    def apply(n: Int): BigInt =
      val cache = mutable.Map[Int, BigInt](0 -> 0, 1 -> 1)

      def go(n: Int): BigInt =
        cache.getOrElseUpdate(n, go(n - 1) + go(n - 2))

      go(n)

  // rewritten into a loop that does not consume stack space
  object TailRec extends Fib:
    def apply(n: Int): BigInt =
      @scala.annotation.tailrec
      def go(n: Int, prev1: Int, prev2: Int): BigInt =
        n match
          case 0 => prev1 // fib(n-2)
          case 1 => prev2 // fib(n-1)
          case n => go(n - 1, prev2, prev1 + prev2)

      go(n, 0, 1)

  object Iterative extends Fib:
    def apply(n: Int): BigInt =
      var prev1 = 0
      var prev2 = 1
      var i = 0
      while i < n do
        i += 1
        val oldPrev1 = prev1
        prev1 = prev2
        prev2 = oldPrev1 + prev2
      prev1

  // memoizes by design
  final class Lazy(strategy: "scan" | "zip") extends Fib:
    def apply(n: Int): BigInt =
      strategy match
        case "scan" => lazyScan(n)
        case "zip"  => lazyZip(n)

    private def lazyScan(n: Int): BigInt =
      // lazy val for memoization - holding on to the head
      lazy val fib: LazyList[BigInt] =
        // 0 - 1 (initial value) -> scan not run

        // fib - could be treated as a list of already computed values
        0 #:: fib.scanLeft(BigInt(1)): (acc, c) =>
          println(s"$acc + $c")
          // fib(n-1) + fib(n-2)
          acc + c

      fib(n)

    private def lazyZip(n: Int): BigInt =
      lazy val fib: LazyList[BigInt] =
        LazyList(0, 1) #::: fib.lazyZip(fib.tail).map(_ + _)

      fib(n)

  // moves computation from stack to the heap
  // trampoline - the size of the stack keeps growing and shrinking by one frame for every step
  // slower that straight tailrec / iteration, but easier to reason about
  object Trampolined extends Fib:
    import scala.util.control.TailCalls.*

    def apply(n: Int): BigInt =
      def go(n: Int): TailRec[BigInt] =
        if n < 2 then done(n)
        else
          for
            a <- tailcall(go(n - 2)) // fib(n-2)
            b <- tailcall(go(n - 1)) // fib(n-1)
          yield a + b

      go(n).result

end Fib
