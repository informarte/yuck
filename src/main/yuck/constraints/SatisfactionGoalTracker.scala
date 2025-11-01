package yuck.constraints

import scala.collection.*

import yuck.core.*
import yuck.util.Collections.*

/**
 * Intuitively, given a set of search variables and a set of constraints (represented by
 * their cost variables), this constraint maintains, for each search variable, the number
 * of unsatisfied constraints the variable is involved in.
 *
 * More formally, given variables x[i], 1 <= i <= n, Boolean variables c[j], 1 <= j <= m,
 * and a Boolean (n x m) matrix M, this constraint maintains the distribution d with
 * d[i] = |{j | 1 <= j <= m /\ M[i, j] /\ s(c[j]) > 0}| for all 1 <= i <= n.
 *
 * @param involvementMap maps each c[j] to the indices of the search variables involved
 *                       in the constraint represented by c[j]
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class SatisfactionGoalTracker
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     involvementMap: immutable.Map[BooleanVariable, IntArraySeq],
     distribution: Distribution)
    extends Constraint(id)
{

    private val n = distribution.size

    require(involvementMap.forall(_._2.inlineForall(i => i >= 0 && i < n)))

    override def toString =
        "satisfaction_goal_tracker([%s])".format(involvementMap.iterator.map(_._1).mkString(", "))

    override def inVariables = involvementMap.view.keys
    override def outVariables = Nil

    override def initialize(now: SearchState) = {
        val f = new Array[Long](n)
        for ((costVar, is) <- involvementMap) {
            val satisfied = now.value(costVar).truthValue
            if (! satisfied) {
                is.inlineForeach(i => f.update(i, f(i) + 1))
            }
        }
        f.inlineForeach(distribution.setFrequency)
        Nil
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        Nil

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for (costVar <- move.involvedVariables.map(_.asInstanceOf[BooleanVariable])) {
             val satisfiedBefore = before.value(costVar).truthValue
             val satisfiedAfter = after.value(costVar).truthValue
             if (satisfiedBefore != satisfiedAfter) {
                 val delta = if satisfiedBefore && ! satisfiedAfter then 1L else -1L
                 involvementMap(costVar).inlineForeach(i => distribution.addFrequencyDelta(i, delta))
             }
        }
        Nil
    }

}

/**
 * Companion object to SatisfactionGoalTracker.
 *
 * @author Michael Marte
 */
object SatisfactionGoalTracker {

    /**
     * Given search variables x[i], 1 <= i <= n, and constraints represented by their
     * Boolean cost variables c[j], 1 <= j <= m, this method computes a map associating
     * each c[j] with the indices of the search variables involved in the constraint
     * represented by c[j].
     */
    def computeInvolvementMap
        (space: Space, xs: IndexedSeq[AnyVariable], cs: Iterable[BooleanVariable]):
        immutable.Map[BooleanVariable, IntArraySeq] =
    {
        val x2i = xs.indices.stream.toMapUsingKeyGenerator(xs.apply)
        val ys = xs.toSet
        def involvedSearchVariables(c: BooleanVariable) =
            space.involvedSearchVariables(c).stream
                .filter(ys.contains).mapToInt(x2i.applyAsInt).toArray.sortInPlace().toArraySeq
        cs.iterator.map(c => (c, involvedSearchVariables(c))).toMap
    }

}
