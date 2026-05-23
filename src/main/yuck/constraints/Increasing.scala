package yuck.constraints

import scala.collection.*

import yuck.constraints.Increasing.deduplicated
import yuck.core.*

/**
 * Base class for the family of increasing constraints.
 */
abstract class Increasing
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint])
    (implicit typeTraits: OrderedTypeTraits[A, D, X])
    extends Constraint(id)
{

    val xs: immutable.IndexedSeq[X]
    val strict: Boolean
    val costs: BooleanVariable

    protected val n = xs.size

    override def inVariables = xs
    override def outVariables = List(costs)

    // x2is maps each input variable to the constraints the variable is involved in.
    // (Constraint i constrains xs(i) and xs(i + 1).)
    private val x2is: HashMap[AnyVariable, Vector[Int]] =
        xs.view
            .zipWithIndex
            .map((x, i) => (x, if i == 0 then Seq(i) else if i == n - 1 then Seq(i - 1) else Seq(i - 1, i)))
            .groupBy(_._1)
            .view
            .mapValues(_.flatMap(_._2.iterator).toVector)
            .to(HashMap)

    private var currentCosts = 0L
    private var futureCosts = 0L

    private val effect = new ReusableMoveEffectWithFixedVariable(costs)

    final override def propagate(): PropagationEffects = {
        if costs.domain == TrueDomain then {
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
            then typeTraits.domainPruner.ltRule(x.domain, y.domain)
            else typeTraits.domainPruner.leRule(x.domain, y.domain)
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
        for i <- move.effectsIterator.flatMap(effect => x2is(effect.x).iterator).distinct do {
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

    protected def maybeSmallestFeasibleValue(x: X, maybePreviousValue: Option[A]): Option[A]

    protected final def solve(space: Space): Boolean = {
        if isCandidateForImplicitSolving(space) then {
            type Assignments = List[(X, Option[A])]
            val assignments: Assignments = xs.foldLeft(Nil: Assignments) {
                case (acc@(_, None) :: _, _) => acc
                case (Nil, x) => (x, maybeSmallestFeasibleValue(x, None)) :: Nil
                case ((y, Some(a)) :: tail, x) => (x, maybeSmallestFeasibleValue(x, Some(a))) :: (y, Some(a)) :: tail
            }
            if assignments.head._2.isEmpty then {
                false
            } else {
                for case (x, Some(a)) <- assignments do {
                    space.setValue(x, a)
                }
                space.setValue(costs, True)
                true
            }
        } else {
            false
        }
    }

    private def computeCosts(a: A, b: A): Long =
        if strict
        then typeTraits.costModel.ltViolation(a, b)
        else typeTraits.costModel.leViolation(a, b)

}

object Increasing {

    // [a, b, c, c, d, c] -> [a, b, c, d, c]
    def deduplicated[A](seq: collection.Seq[A]): collection.Seq[A] =
        seq.foldLeft(mutable.ArrayBuffer.empty[A]) {
            (acc, elem) => if acc.lastOption.contains(elem) then acc else acc.addOne(elem)
        }

}
