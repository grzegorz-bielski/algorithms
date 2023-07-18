package algos.graphs.trees

import org.scalatest.*, funsuite.*, matchers.*

class TrieSpecScala extends AnyFunSuite, should.Matchers:
  test("passes the test workflow"):
    SpellCheckingTrie(
      value = None,
      children = List(
        SpellCheckingTrie(
          Some('c'),
          List(
            SpellCheckingTrie(
              Some('a'),
              List(
                SpellCheckingTrie(
                  Some('t'),
                  List(
                    SpellCheckingTrie(Some('s'), Nil, isWord = true)
                  ),
                  isWord = true
                ),
                SpellCheckingTrie(
                  Some('r'),
                  List(SpellCheckingTrie(Some('d'), Nil, isWord = true))
                )
              )
            )
          )
        )
      )
    )
