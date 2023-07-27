package algos.graphs.csp

// Find digits that satisfy the equation `SEND + MORE = MONEY`
// Each letter represents a unique digit (0-9)
// When letter repeats, it means the same digit repeats.

@main def sendMoreMoneyProgram =
  sendMoreMoney.fold(println, solution => println(s"Solution found: $solution"))

def sendMoreMoney =
  val letters = Vector('S', 'E', 'N', 'D', 'M', 'O', 'R', 'Y')
  val digits = (0 to 9).toVector
  val domains = letters
    .map(_ -> digits)
    .toMap
    .updated('M', Vector(1)) // prevent leading 0

  val constraint = new Constraint[Char, Int]:
    def variables: List[Char] = letters.toList
    def satisfied(assignment: Map[Char, Int]): Boolean =
      lazy val noDuplicates = assignment.values.toSet.size == assignment.size

      lazy val allVariablesAssigned = assignment.size == variables.size

      lazy val assignmentHasAllLetters =
        val s = assignment('S')
        val e = assignment('E')
        val n = assignment('N')
        val d = assignment('D')
        val m = assignment('M')
        val o = assignment('O')
        val r = assignment('R')
        val y = assignment('Y')
        val send = s * 1000 + e * 100 + n * 10 + d
        val more = m * 1000 + o * 100 + r * 10 + e
        val money = m * 10000 + o * 1000 + n * 100 + e * 10 + y

        send + more == money

      if noDuplicates
      then if allVariablesAssigned then assignmentHasAllLetters else true
      else false

  CSP
    .search(domains, constraint)
    .map: n =>
      val send = List('S', 'E', 'N', 'D').map(n).mkString
      val more = List('M', 'O', 'R', 'E').map(n).mkString
      val money = List('M', 'O', 'N', 'E', 'Y').map(n).mkString

      n -> s"$send + $more = $money"
