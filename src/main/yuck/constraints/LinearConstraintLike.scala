package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Maintains the violation of sum a(i) * x(i) R z where R is an ordering relation.
 *
 * y is a helper channel for propagation: Conceptually, sum a(i) * x(i) = y /\ y R z.
 *
 * @author Michael Marte
 */
abstract class LinearConstraintLike
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal)
    extends Constraint(id, goal)
{

    protected val valueTraits: NumericalValueTraits[Value]

    protected val n: Int
    protected val y: NumericalVariable[Value]
    protected val relation: OrderingRelation
    protected val z: NumericalVariable[Value]
    protected val costs: BooleanVariable
    protected def a(i: Int): Value
    protected def x(i: Int): NumericalVariable[Value]

    override def toString =
        "sum([%s], %s, %s, %s)".format(
            (for (i <- 0 until n) yield AX(a(i), x(i))).mkString(", "),
            relation, z, costs)
    override def inVariables = (0 until n).view.map(x) :+ z
    override def outVariables = List(costs)

    protected var currentSum = valueTraits.zero
    protected var futureSum = valueTraits.zero
    protected val effects = List(new ReusableMoveEffectWithFixedVariable[BooleanValue](costs))
    protected val effect = effects.head

    // Propagates sum a(i) * x(i) = y.
    private def propagate1(effects: PropagationEffects): PropagationEffects = {
        // An Iterable does not compare to other sequences, so we have to use a Seq to facilitate mocking.
        val lhs0 = new Seq[(Value, NumericalDomain[Value])] {
            override def iterator = (0 until n).iterator.map(apply)
            override def length = n
            override def apply(i: Int) = (a(i), x(i).domain)
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.linEqRule(lhs0, rhs0)
        effects.pruneDomains((0 until n).iterator.map(x).zip(lhs1.iterator)).pruneDomain(y, rhs1)
    }

    // Propagates y relation z.
    private def propagate2(effects: PropagationEffects): PropagationEffects = {
        type Domain = NumericalDomain[Value]
        val domainPruner = valueTraits.domainPruner
        val propagator = new ReifiedBinaryConstraintPropagator[Domain, Domain] {
            override protected def enforce(lhs: Domain, rhs: Domain) =
                relation match {
                    case EqRelation => domainPruner.eqRule(lhs, rhs)
                    case NeRelation => domainPruner.neRule(lhs, rhs)
                    case LtRelation => domainPruner.ltRule(lhs, rhs)
                    case LeRelation => domainPruner.leRule(lhs, rhs)
                }
            override protected def prohibit(lhs0: Domain, rhs0: Domain) =
                relation match {
                    case EqRelation => domainPruner.neRule(lhs0, rhs0)
                    case NeRelation => domainPruner.eqRule(lhs0, rhs0)
                    case LtRelation => domainPruner.leRule(rhs0, lhs0).swap
                    case LeRelation => domainPruner.ltRule(rhs0, lhs0).swap
                }
        }
        val (dy0, dz0, costsDomain0) = (y.domain, z.domain, BooleanDomain.ensureDecisionDomain(costs.domain))
        val (dy1, dz1, costsDomain1) = propagator.propagate(dy0, dz0, costsDomain0)
        effects.pruneDomains(y, dy1, z, dz1, costs, costsDomain1)
    }

    final override def propagate = {
        propagate2(propagate1(NoPropagationOccurred))
    }

    protected final def computeCosts(a: Value, b: Value): BooleanValue = {
        val costModel = valueTraits.orderingCostModel
        relation match {
            case EqRelation => costModel.eqViolation(a, b)
            case NeRelation => costModel.neViolation(a, b)
            case LtRelation => costModel.ltViolation(a, b)
            case LeRelation => costModel.leViolation(a, b)
        }
    }

    final override def initialize(now: SearchState) = {
        currentSum = valueTraits.zero
        for (i <- 0 until n) {
            currentSum += a(i) * now.value(x(i))
        }
        effect.a = computeCosts(currentSum, now.value(z))
        effects
    }

    final override def commit(before: SearchState, after: SearchState, move: Move) = {
        currentSum = futureSum
        effects
    }

}
