package yuck.constraints

import yuck.constraints.Increasing.deduplicated

import scala.collection.*
import yuck.core.*
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * Base class for the family of increasing constraints.
 *
 * @author Michael Marte
 */
abstract class Increasing
    [V <: OrderedValue[V], X <: OrderedVariable[V]]
    (id: Id[Constraint])
    (implicit valueTraits: OrderedValueTraits[V])
    extends Constraint(id)
{

    protected val xs: immutable.IndexedSeq[X]
    protected val strict: Boolean
    protected val costs: BooleanVariable

    protected val n = xs.size

    require(n > 1)

    override def inVariables = xs
    override def outVariables = List(costs)

    // x2is maps each input variable to the constraints the variable is involved in.
    // (Constraint i constrains xs(i) and xs(i + 1).)
    private val x2is: immutable.Map[AnyVariable, immutable.IndexedSeq[Int]] =
        xs.view
            .zipWithIndex
            .map((x, i) => (x, if i == 0 then Seq(i) else if i == n - 1 then Seq(i - 1) else Seq(i - 1, i)))
            .groupBy(_._1)
            .view
            .mapValues(_.flatMap(_._2.iterator).toVector)
            .toMap

    private var currentCosts = 0L
    private var futureCosts = 0L

    private val effect = costs.reuseableEffect

    final override def propagate(): PropagationEffects = {
        if (costs.domain == TrueDomain) {
            val leftToRight = (0 until n - 1).foldLeft(NoPropagationOccurred: PropagationEffects)(propagate)
            val rightToLeft = (n - 2 to 0 by -1).foldLeft(leftToRight)(propagate)
            rightToLeft
        } else {
            NoPropagationOccurred
        }
    }

    private def propagate(effects: PropagationEffects, i: Int): PropagationEffects = {
        val x = xs(i)
        val y = xs(i + 1)
        val (dx1, dy1) =
            if strict
            then valueTraits.domainPruner.ltRule(x.domain, y.domain)
            else valueTraits.domainPruner.leRule(x.domain, y.domain)
        effects.pruneDomains(x, dx1, y, dy1)
    }

    final override def initialize(now: SearchState) = {
        currentCosts = (0 until n - 1).iterator
            .map(i => computeCosts(now.value(xs(i)), now.value(xs(i + 1))))
            .foldLeft(0L)(safeAdd)
        effect.a = BooleanValue(currentCosts)
        effect
    }

    final override def consult (before: SearchState, after: SearchState, move: Move) = {
        futureCosts = currentCosts
        for (i <- move.effectsIterator.flatMap(effect => x2is(effect.x).iterator).distinct) {
            val x = xs(i)
            val y = xs(i + 1)
            val delta = safeSub(computeCosts(after.value(x), after.value(y)), computeCosts(before.value(x), before.value(y)))
            futureCosts = safeAdd(futureCosts, delta)
        }
        effect.a = BooleanValue(futureCosts)
        effect
    }

    final override def commit(before: SearchState, after: SearchState, move: Move) = {
        currentCosts = futureCosts
        effect
    }

    final override def isCandidateForImplicitSolving(space: Space) =
        xs.exists(space.isSearchVariable) &&
        ! xs.exists(space.isChannelVariable) &&
        xs.forall(_.domain.isFinite) &&
        (if strict
         then xs.toSet.size == n
         else deduplicated(xs).toSet.size == deduplicated(xs).size)

    protected def maybeSmallestFeasibleValue(x: X, maybePreviousValue: Option[V]): Option[V]

    final protected def solve(space: Space): Boolean = {
        if (isCandidateForImplicitSolving(space)) {
            type Assignments = List[(Variable[V], Option[V])]
            val assignments: Assignments = xs.foldLeft(Nil: Assignments) {
                case (acc@(_, None) :: _, _) => acc
                case (Nil, x) => (x, maybeSmallestFeasibleValue(x, None)) :: Nil
                case ((y, Some(a)) :: tail, x) => (x, maybeSmallestFeasibleValue(x, Some(a))) :: (y, Some(a)) :: tail
            }
            if (assignments.head._2.isEmpty) {
                false
            } else {
                for (case (x, Some(a)) <- assignments) {
                    space.setValue(x, a)
                }
                space.setValue(costs, True)
                true
            }
        } else {
            false
        }
    }

    private def computeCosts(a: V, b: V): Long =
        if strict
        then valueTraits.costModel.ltViolation(a, b)
        else valueTraits.costModel.leViolation(a, b)

}

/**
 * Companion object to Increasing.
 *
 * @author Michael Marte
 */
object Increasing {

    // [a, b, c, c, d, c] -> [a, b, c, d, c]
    def deduplicated[A](seq: collection.Seq[A]): collection.Seq[A] =
        seq.foldLeft(mutable.ArrayBuffer.empty[A]) {
            (acc, elem) => if acc.lastOption.contains(elem) then acc else acc.addOne(elem)
        }

}
