package algos.small

import scala.collection.mutable

// TODO: any number of towers?

object TowersOfHanoi:
  type Disc = Int
  type Tower = mutable.Stack[Disc]
  type Towers = (Tower, Tower, Tower)

  object Direct:
    def moveInPlace(A: Tower, B: Tower, C: Tower, n: Int): Unit =
      if n == 1 then B.push(A.pop())
      else
        // move upper n-1 discs from tower A to B using C as temp
        moveInPlace(A, C, B, n - 1)
        // move bottom disc from A to C
        moveInPlace(A, B, C, 1)
        // move upper n-1 discs from B to C using A as temp
        moveInPlace(C, B, A, n - 1)

  object CpsNonTailRec:
    def moveInPlace(A: Tower, B: Tower, C: Tower, n: Int): Unit =
      def go(A: Tower, B: Tower, C: Tower, n: Int)(co: => Unit): Unit =
        if n == 1 then
          B.push(A.pop())
          co
        else
          // move upper n-1 discs from tower A to B using C as temp
          go(A, C, B, n - 1):
            // move bottom disc from A to C
            go(A, B, C, 1):
              // move upper n-1 discs from B to C using A as temp
              go(C, B, A, n - 1)(co)

      go(A, B, C, n)(())

  object DirectTailRec:
    def moveInPlace(A: Tower, B: Tower, C: Tower, n: Int): Unit =
      case class Move(from: Tower, to: Tower, temp: Tower, n: Int)
      @scala.annotation.tailrec
      def go(list: List[Move]): Unit =
        list match
          case Move(a, b, c, n) :: tail =>
            if n == 1 then
              b.push(a.pop())
              go(tail)
            else
              go(
                Move(a, c, b, n - 1) +:
                  Move(a, b, c, 1) +:
                  Move(c, b, a, n - 1) +:
                  tail
              )
          case Nil => ()

      go(Move(A, B, C, n) +: Nil)

  object Trampolined:
    import scala.util.control.TailCalls.*

    def moveInPlace(A: Tower, B: Tower, C: Tower, n: Int): Unit =
      def go(A: Tower, B: Tower, C: Tower, n: Int): TailRec[Unit] =
        if n == 1 then
          B.push(A.pop())
          done(())
        else
          for
            // move upper n-1 discs from tower A to B using C as temp
            _ <- tailcall(go(A, C, B, n - 1))
            // move bottom disc from A to C
            _ <- tailcall(go(A, B, C, 1))
            // move upper n-1 discs from B to C using A as temp
            _ <- tailcall(go(C, B, A, n - 1))
          yield ()

      go(A, B, C, n).result
