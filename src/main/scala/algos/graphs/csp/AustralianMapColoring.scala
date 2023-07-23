package algos.graphs.csp

// Can you color the map of Australia with only three colors such that no two adjacent regions do not have the same color?

enum Region:
  case WA, NT, SA, Q, NSW, V, T

enum Color:
  case Red, Green, Blue

@main
def australianMapColoringProgram =
  println:
    australianMapColoring.fold(
      e => s"Failed with: $e",
      s => s"Found solution: $s"
    )

final case class AusConstraint(a: Region, b: Region) extends Constraint[Region, Color]:
  def variables = List(a, b)
  def satisfied(assignment: Map[Region, Color]): Boolean =
    val placesNotInAssignment =
      !assignment.contains(a) || !assignment.contains(b)
    val colorsAreTheSame =
      assignment.get(a) == assignment.get(b)

    placesNotInAssignment || !colorsAreTheSame

def australianMapColoring =
  val domains =
    Region.values.map(_ -> Color.values.toVector).toMap

  val constraints =
    Vector(
      Region.WA -> Region.NT,
      Region.WA -> Region.SA,
      Region.SA -> Region.NT,
      Region.Q -> Region.NT,
      Region.Q -> Region.SA,
      Region.Q -> Region.NSW,
      Region.NSW -> Region.SA,
      Region.V -> Region.SA,
      Region.V -> Region.NSW,
      Region.V -> Region.T
    ).map(AusConstraint.apply.tupled)

  CSP
    .create(domains, constraints*)
    .flatMap(_.search.toRight("No solution found"))
