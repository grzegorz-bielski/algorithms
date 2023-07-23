package algos.graphs.csp

import scala.util.control.TailCalls.*

/** A backtracking constraint-satisfaction problem solver
  */
final class CSP[V, D](
    domains: Map[V, Vector[D]],
    constraints: Map[V, Vector[Constraint[V, D]]]
):
  private lazy val variables: Vector[V] = domains.keys.toVector

  /** check if value assignment is consistent by checking all constraints
    * @param variable
    *   a variable added to assignment
    * @param assignment
    *   a specific domain value selected for a variable tested for consistency
    * @return
    */
  def consistent(variable: V, assignment: Map[V, D]): Boolean =
    constraints.get(variable).exists(_.forall(_.satisfied(assignment)))

  /** Find solution using depth-first search with backtracking
    *
    * Non-deterministic, depends on implicit Map ordering
    */
  def search: Option[Map[V, D]] =
    def go(assignment: Map[V, D]): TailRec[Option[Map[V, D]]] =
      if assignment.size == variables.size
      then done(Some(assignment))
      else
        // select first unassigned variable...
        variables
          .find(!assignment.contains(_))
          .fold(done(None)): unassigned =>
            def fn(domains: Vector[D]): TailRec[Option[Map[V, D]]] =
              domains match
                case domain +: tail =>
                  // try assigning all possible domain values for that variable, one at a time
                  val localAssignment = assignment + (unassigned -> domain)

                  if consistent(unassigned, localAssignment)
                  then
                    // variable assignment is consistent, test it against all other variables
                    tailcall(go(localAssignment)).flatMap:
                      // assignment was not consistent after all, backtrack and try next domain value
                      case None => tailcall(fn(tail))
                      // final assignment was consistent, solution found
                      case Some(value) => done(Some(value))
                  else
                    // assignment is not consistent, try next domain value
                    tailcall(fn(tail))

                // no assignment was consistent, solution not found
                case _ => done(None)

            fn(domains.get(unassigned).toVector.flatten)

    go(Map.empty).result

object CSP:
  def create[V, D](domains: Map[V, Vector[D]], cx: Constraint[V, D]*) =
    val variables = domains.keys
    if cx.forall(c => variables.forall(c.variables.contains)) then
      Left:
        "Variable in constraint is not present in CSP"
    else
      Right:
        val init = Map.empty[V, Vector[Constraint[V, D]]]
        val constraints = cx.foldLeft(init): (acc, c) =>
          c.variables.foldLeft(acc):
            _.updatedWith(_)(m => Some(m.fold(Vector(c))(_ :+ c)))

        CSP(domains, constraints)
