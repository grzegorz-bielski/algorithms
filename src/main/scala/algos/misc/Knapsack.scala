package algos.misc

object Knapsack:
  // What items could be put into a knapsack with a maximum capacity of 75kg?

  // each item has a weight and a value and could be taken only once

  // brute force approach: O(2^n)
  // where n is the number of items

  // dynamic programming approach: O(n * W)
  // where n is the number of items and W is the capacity of the knapsack

  // dynamic programming - solve subproblems, store them and build up to the solution to the original problem

  final case class Item(name: String, weight: Int, value: Int)

  def findBestCombination(items: Vector[Item], maxCapacity: Int): Vector[Item] =
    // table[combinations of items][max capacity] = summed value
    val table = Array.ofDim[Int](items.size + 1, maxCapacity + 1)

    // build the solutions table
    for
      i <- items.indices
      item = items(i)
      c <- 1 to maxCapacity
    yield
      val curr = table(i)(c)
      val next = if c >= item.weight then math.max(item.value + table(i)(c - item.weight), curr) else curr

      table(i + 1)(c) = next

    val bestCombination = Vector.newBuilder[Item]

    // start at the bottom right corner of the table
    (items.size until 0 by -1).foldLeft(maxCapacity): (c, i) =>
      // check if there was a change in the value inserted into the table
      // if it was - the item was the best choice in that combination

      if table(i - 1)(c) != table(i)(c) then
        bestCombination += items(i - 1)
        c - items(i - 1).weight // move up the table
      else c

    bestCombination.result()
