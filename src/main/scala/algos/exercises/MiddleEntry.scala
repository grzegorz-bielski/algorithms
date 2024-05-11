package algos.exercises.middleentry

object Solution:
  def findMiddle(entries: Vector[(String, String)]): Option[String] =
    val prerequisite2entries = entries.toMap
    val entriesSet = prerequisite2entries.values.toSet

    entries
      .find((prerequisite, _) => !entriesSet(prerequisite))
      .map: (firstPrerequisite, _) =>
        def go(prerequisite: String, acc: Vector[String]): Vector[String] =
          prerequisite2entries.get(prerequisite) match
            case None       => acc :+ prerequisite
            case Some(next) => go(next, acc :+ prerequisite)

        val entriesInOrder = go(firstPrerequisite, Vector.empty)

        entriesInOrder((entriesInOrder.length / 2) - 1) // leftmost one
