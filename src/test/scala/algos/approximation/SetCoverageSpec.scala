package algos.approximation

import org.scalatest.*
import flatspec.*
import matchers.*

class SetCoverageSpec extends AnyFlatSpec, should.Matchers:
  case class Item(name: String)
  case class Place(name: String)
  val toCover = Set(
    Item("mt"),
    Item("wa"),
    Item("or"),
    Item("id"),
    Item("nv"),
    Item("ut"),
    Item("ca"),
    Item("az")
  )
  val available = Map(
    Place("kone") -> Set(Item("id"), Item("nv"), Item("ut")),
    Place("ktwo") -> Set(Item("wa"), Item("id"), Item("mt")),
    Place("kthree") -> Set(Item("or"), Item("nv"), Item("ca")),
    Place("kfour") -> Set(Item("nv"), Item("ut")),
    Place("kfive") -> Set(Item("ca"), Item("az"))
  )

  "apply" should "find the best `Place` to cover the set as much as possible" in:
    val shared = Set(Place("ktwo"), Place("kthree"), Place("kfive"))
    val possibilities = (shared + Place("kone"), shared + Place("kfour"))

    SetCoverage(available, toCover) should:
      be(possibilities._1) or be(possibilities._2)
    