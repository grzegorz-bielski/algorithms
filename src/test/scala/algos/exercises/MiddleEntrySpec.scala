package algos.exercises.middleentry

import org.scalatest.*
import flatspec.*
import matchers.*

class MiddleEntrySpec extends AnyFlatSpec, should.Matchers:
  "findMiddle" should "work correctly" in:
    val entries = Vector(
      ("computer science", "algebra"),
      ("mining", "dunking"),
      ("statistics", "mining"),
      ("system design", "computer science"),
      ("algebra", "statistics")
    )

    Solution.findMiddle(entries) shouldEqual Some("algebra")
