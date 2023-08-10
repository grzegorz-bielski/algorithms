package algos.misc

import org.scalatest.*, funsuite.*, matchers.*

class KnapsackSpec extends AnyFunSuite, should.Matchers:
  test("finds the best combination in terms if resources / value within max capacity"):
    val items = Vector(
      ("television", 50, 500),
      ("stereo", 35, 400),
      ("laptop", 3, 1000),
      ("food", 15, 50),
      ("clothing", 20, 800),
      ("jewelry", 1, 4000),
      ("books", 100, 300),
      ("printer", 18, 30),
      ("refrigerator", 200, 700),
      ("painting", 10, 1000),
      ("candlesticks", 2, 300)
    ).map(Knapsack.Item.apply.tupled)

    val result = Knapsack.findBestCombination(
      items = items,
      maxCapacity = 75
    )

    result shouldBe Vector(
      Knapsack.Item("candlesticks", 2, 300),
      Knapsack.Item("painting", 10, 1000),
      Knapsack.Item("jewelry", 1, 4000),
      Knapsack.Item("clothing", 20, 800),
      Knapsack.Item("laptop", 3, 1000),
      Knapsack.Item("stereo", 35, 400)
    )

    assert(result.map(_.weight).sum <= 75)
