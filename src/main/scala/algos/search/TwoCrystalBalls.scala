package algos.search

// Given two crystal balls that will break if dropped from a high enough
// distance, determine the exact spot in which it will break in the most optimized way

def twoCrystalBalls(breaks: Array[Boolean]): Option[Int] =
  breaks.length match
    case 0     => None
    case 1 | 2 => (0 until breaks.length).find(breaks) // linear for 2 elements
    case length => // O(sqrt(n)) for n elements
      val jumpAmount = math.floor(math.sqrt(length)).toInt

      for
        // first ball
        i <- (0 to (length, jumpAmount)).find(breaks) 
        // jump back to scan forward 1 step at a time
        prev = math.max(i - jumpAmount, 0)
        // second ball
        i <- (prev to i).find(breaks)
      yield i

def twoCrystalBallsImperative(breaks: Array[Boolean]): Option[Int] =
  breaks.length match
    case 0     => None
    case 1 | 2 => (0 until breaks.length).find(breaks)
    case length =>
      val jumpAmount = math.floor(math.sqrt(length)).toInt

      var i = 0
      // jump until 1 ball breaks
      var done1 = false
      while i < length && !done1 do
        i = i + jumpAmount
        if breaks(i) then done1 = true

      // jump back
      i = i - jumpAmount

      // walk forward the `jumpAmount` steps
      var j = 0
      var done2 = false
      while j <= jumpAmount && !done2 do
        if breaks(i) then done2 = true
        else
          i = i + 1
          j = j + 1

      if done2 then Some(i) else None
