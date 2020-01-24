package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Intuitively, given a set search variables and set of constraints (represented by
 * their cost variables), this constraint maintains, for each search variable,
 * the number of unsatisfied constraints the variable is involved in.
 *
 * More formally, given variables x[i], 1 <= i <= n, Boolean variables c[j], 1 <= j <= m,
 * and the Boolean (n x m) matrix M, this constraint maintains the distribution d with
 * d[i] = |{j | 1 <= j <= m /\ M[i, j] /\ s(c[j]) > 0}| for all 1 <= i <= n.
 *
 * @param involvementMatrix maps each c[j] to the indices of the search variables involved
 *                          in the constraint represented by c[j]
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class SatisfactionGoalTracker
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     involvementMatrix: immutable.Map[BooleanVariable, immutable.IndexedSeq[Int]],
     distribution: Distribution)
    extends Constraint(id)
{

    private val n = distribution.size

    require(involvementMatrix.forall(_._2.forall(_ < n)))

    override def toString = "satisfactionGoalTracker(%s, %s)".format(involvementMatrix, distribution)

    override def inVariables = involvementMatrix.view.map(_._1)
    override def outVariables = Nil

    override def initialize(now: SearchState) = {
        val f = new Array[Int](n)
        for ((costVar, is) <- involvementMatrix) {
            val satisfied = now.value(costVar).truthValue
            if (! satisfied) {
                for (i <- is) {
                    f.update(i, f(i) + 1)
                }
            }
        }
        for (i <- 0 until n) {
            distribution.setFrequency(i, f(i))
        }
        Nil
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        Nil

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for {costVar <- move.involvedVariables.map(_.asInstanceOf[BooleanVariable])
             satisfiedBefore = before.value(costVar).truthValue
             satisfiedAfter = after.value(costVar).truthValue
             if satisfiedBefore != satisfiedAfter
             delta = if (satisfiedBefore && ! satisfiedAfter) 1 else -1
             i <- involvementMatrix(costVar)}
        {
            distribution.addFrequencyDelta(i, delta)
        }
        Nil
    }

}
