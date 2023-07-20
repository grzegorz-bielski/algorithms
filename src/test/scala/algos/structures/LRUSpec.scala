package algos.structures

import org.scalatest.*, funsuite.*, matchers.*

class LRUSpec extends AnyFunSuite, should.Matchers:
  test("should pass test workflow"):
    val lru = LRU.empty[String, Int](capacity = 3)

    lru.get("foo") shouldBe None
    lru.update("foo", 69)
    lru.get("foo") shouldBe Some(69)

    lru.update("bar", 420)
    lru.get("bar") shouldBe Some(420)

    lru.update("baz", 1337)
    lru.get("baz") shouldBe Some(1337)

    lru.update("ball", 69420)
    lru.get("ball") shouldBe Some(69420)
    lru.get("foo") shouldBe None
    lru.get("bar") shouldBe Some(420)
    lru.update("foo", 69)
    lru.get("bar") shouldBe Some(420)
    lru.get("foo") shouldBe Some(69)

    lru.get("baz") shouldBe None