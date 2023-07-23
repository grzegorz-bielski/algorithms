package algos.graphs.csp

trait Constraint[Variable, Domain]:
  /** The variables that the constraint is between.
    */
  def variables: List[Variable]

  def satisfied(assignment: Map[Variable, Domain]): Boolean
