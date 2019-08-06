package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Computes the violation of sum a(i) * x(i) R z where R is an ordering relation.
 *
 * as contains the coefficients.
 * If as is null, then the coefficients are assumed to be 1.
 *
 * y is a helper variable for propagation: Conceptionally, sum a(i) * x(i) = y /\ y R z.
 *
 * @author Michael Marte
 */
final class LinearConstraint
    [Value <: NumericalValue[Value]] private
    (id: Id[Constraint], goal: Goal,
     as: immutable.IndexedSeq[Value], xs: immutable.IndexedSeq[NumericalVariable[Value]],
     y: NumericalVariable[Value], relation: OrderingRelation, z: NumericalVariable[Value],
     costs: BooleanVariable)
    (implicit valueTraits: NumericalValueTraits[Value])
    extends Constraint(id, goal)
{

    require(as == null || as.size == xs.size)
    require(xs.toSet.size == xs.size)

    override def toString =
        "sum([%s], %s, %s, %s)".format(
            (if (as == null) xs else for ((a, x) <- as.zip(xs)) yield AX(a, x)).mkString(", "),
            relation, z, costs)
    override def inVariables = xs.iterator ++ Iterator.single(z)
    override def outVariables = List(costs)

    private val x2i: immutable.Map[AnyVariable, Int] =
        if (as == null) null else new immutable.HashMap[AnyVariable, Int] ++ (xs.iterator.zipWithIndex)

    private var currentSum = valueTraits.zero
    private var futureSum = valueTraits.zero
    private val effects = List(new ReusableMoveEffectWithFixedVariable[BooleanValue](costs))
    private val effect = effects.head

    // Propagates sum a(i) * x(i) = y.
    private def propagate1(effects: PropagationEffects): PropagationEffects = {
        // An Iterable does not compare other sequences, so we have to use a Seq to facilitate mocking.
        val lhs0 = new Seq[(Value, NumericalDomain[Value])] {
            override def iterator = (0 until xs.size).iterator.map(apply)
            override def length = xs.size
            override def apply(i: Int) = (if (as == null) valueTraits.one else as(i), xs(i).domain)
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.linEq(lhs0, rhs0)
        effects.pruneDomains(xs.iterator.zip(lhs1.iterator)).pruneDomain(y, rhs1)
    }

    // Propagates y relation z.
    private def propagate2(effects: PropagationEffects): PropagationEffects = {
        type Domain = NumericalDomain[Value]
        val domainPruner = valueTraits.domainPruner
        val propagator = new ReifiedBinaryConstraintPropagator[Domain, Domain] {
            override protected def enforce(lhs: Domain, rhs: Domain) =
                relation match {
                    case EqRelation => domainPruner.eq(lhs, rhs)
                    case NeRelation => domainPruner.ne(lhs, rhs)
                    case LtRelation => domainPruner.lt(lhs, rhs)
                    case LeRelation => domainPruner.le(lhs, rhs)
                }
            override protected def prohibit(lhs0: Domain, rhs0: Domain) =
                relation match {
                    case EqRelation => domainPruner.ne(lhs0, rhs0)
                    case NeRelation => domainPruner.eq(lhs0, rhs0)
                    case LtRelation => domainPruner.le(rhs0, lhs0).swap
                    case LeRelation => domainPruner.lt(rhs0, lhs0).swap
                }
        }
        val (dy0, dz0, costsDomain0) = (y.domain, z.domain, BooleanDomain.ensureDecisionDomain(costs.domain))
        val (dy1, dz1, costsDomain1) = propagator.propagate(dy0, dz0, costsDomain0)
        effects.pruneDomains(y, dy1, z, dz1, costs, costsDomain1)
    }

    override def propagate = {
        propagate2(propagate1(NoPropagationOccurred))
    }

    private def computeCosts(a: Value, b: Value): BooleanValue = relation match {
        case EqRelation => valueTraits.orderingCostModel.eq(a, b)
        case NeRelation => valueTraits.orderingCostModel.ne(a, b)
        case LtRelation => valueTraits.orderingCostModel.lt(a, b)
        case LeRelation => valueTraits.orderingCostModel.le(a, b)
    }

    override def initialize(now: SearchState) = {
        currentSum = valueTraits.zero
        if (as == null) {
            for (x <- xs) {
                currentSum += now.value(x)
            }
        } else {
            for (i <- 0 until xs.size) {
                currentSum += as(i) * now.value(xs(i))
            }
        }
        effect.a = computeCosts(currentSum, now.value(z))
        effects
    }


    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureSum = currentSum
        for (x0 <- move) {
            if (x0 != z) {
                if (as == null) {
                    val x = valueTraits.safeDowncast(x0)
                    futureSum = futureSum.addAndSub(after.value(x), before.value(x))
                } else {
                    val i = x2i(x0)
                    val x = xs(i)
                    futureSum = futureSum.addAndSub(as(i), after.value(x), before.value(x))
                }
            }
        }
        effect.a = computeCosts(futureSum, after.value(z))
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        currentSum = futureSum
        effects
    }

}

/**
 * @author Michael Marte
 *
 */
final object LinearConstraint {

    def postLinearConstraint
        [Value <: NumericalValue[Value]]
        (space: Space, goal: Goal,
         axs: Seq[AX[Value]], relation: OrderingRelation, z: NumericalVariable[Value],
         costs: BooleanVariable)
        (implicit valueTraits: NumericalValueTraits[Value]):
        Constraint =
    {
        val as = if (axs.forall(ax => ax.a == valueTraits.one)) null else axs.iterator.map(_.a).toIndexedSeq
        val xs = axs.iterator.map(_.x).toIndexedSeq
        val y = valueTraits.createChannel(space)
        val constraint = new LinearConstraint(space.nextConstraintId, goal, as, xs, y, relation, z, costs)
        space.post(constraint)
        constraint
    }

}
