package approximation

import scala.annotation.tailrec

object SetCoverage:
  type Available[K, V] = Map[K, Set[V]]
  type Values[V] = Set[V]

  def apply[K, V](available: Available[K, V], toCover: Values[V]): Values[K] =

    @tailrec
    def go(
        toCover: Values[V],
        covered: Values[K]
    ): Values[K] =
      if toCover.size == 0 then covered
      else
        val res =
          available.foldLeft[(Option[K], Set[V])]((None, Set()))(
            (acc, curr) => {
              val coveredPrim = toCover `intersect` curr._2
              if coveredPrim.size > acc._2.size then (Some(curr._1), coveredPrim)
              else acc
            })

        go(
          toCover `diff` res._2,
          res._1.map(covered + _) getOrElse covered
        )

    go(toCover, Set())
