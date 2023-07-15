package algos.small

// In case we hate Gauss summation formula...
def sum(n: Int): Int =
  @scala.annotation.tailrec
  def go(n: Int, acc: Int): Int = n match
    case 0 => acc
    case n => go(n - 1, acc + n)

  go(n, 0)
