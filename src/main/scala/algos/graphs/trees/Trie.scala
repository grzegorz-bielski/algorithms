package algos.graphs.trees

/** A re-trie-val tree or a prefix tree
  *
  * Use-cases: \- auto-complete \- caching
  */
trait Trie[T]:
  def value: Option[T] // no value for root
  def children: List[Trie[T]] // could also be backed by flat array of size 26 (for English alphabet)

  // O(c) where c is the length of the word
  // def search(value: T): Option[T]

final case class SpellCheckingTrie(
    value: Option[Char],
    children: List[SpellCheckingTrie],
    isWord: Boolean = false
) extends Trie[Char]
