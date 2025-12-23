package yuck.constraints.test

import org.junit.jupiter.api.Test
import org.mockito.AdditionalAnswers.*
import org.mockito.ArgumentMatchers.*
import org.mockito.Mockito.*

import yuck.constraints.*
import yuck.constraints.OrderingRelation.*
import yuck.core.*
import yuck.test.util.UnitTest

abstract class LinearConstraintLikeTest
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    extends UnitTest
{

    protected val baseTypeTraits: NumericalTypeTraits[A, D, X]

    protected val randomGenerator = new JavaRandomGenerator

    private def nonEmptyRandomSubdomain(d: D): D =
        Iterator.continually(d).map(_.randomSubdomain(randomGenerator)).dropWhile(_.isEmpty).next()

    protected val space = new Space(logger, sigint)

    protected val relation: OrderingRelation
    protected val costsDomain: BooleanDomain
    protected val baseDomain: D
    protected val axs: IndexedSeq[AX[A, D, X]]
    protected final lazy val y = baseTypeTraits.createChannel(space)
    protected final lazy val z = baseTypeTraits.createVariable(space, "z", nonEmptyRandomSubdomain(baseDomain))
    protected final val costs = new BooleanVariable(space.nextVariableId(), "costs", costsDomain)
    space.registerObjectiveVariable(costs)
    private val costModel = mock(classOf[OrderingCostModel[A]])
    private val domainPruner = mock(classOf[NumericalDomainPruner[A, D]])
    protected val typeTraits: NumericalTypeTraits[A, D, X] = mock(classOf[NumericalTypeTraits[A, D, X]])
    protected lazy val constraint: Constraint

    private def setupTypeTraits(): Unit = {
        when(typeTraits.costModel).thenReturn(costModel)
        when(typeTraits.domainPruner).thenReturn(domainPruner)
        when(typeTraits.zero).thenReturn(baseTypeTraits.zero)
        when(typeTraits.one).thenReturn(baseTypeTraits.one)
        when(typeTraits.createChannel(any[Space])).thenAnswer(answer(baseTypeTraits.createChannel(_)))
        when(typeTraits.safeDowncast(any[AnyVariable])).thenAnswer(answer(baseTypeTraits.safeDowncast(_: AnyVariable)))
    }

    @Test
    def testBasics(): Unit = {
        setupTypeTraits()
        assertEq(constraint.toString, "sum([%s], %s, %s, %s)".format(axs.mkString(", "), relation, z, costs))
        assertEq(constraint.inVariables.size, axs.size + 1)
        assertEq(constraint.inVariables.toSet, axs.map(_.x).toSet.union(Set(z)))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        setupTypeTraits()
        // We simulate a propagation process where the first call to propagate computes a fixed point.
        val lhs0 = for i <- axs.indices yield (axs(i).a, axs(i).x.domain)
        val dy0 = baseTypeTraits.completeDomain
        val dz0 = z.domain
        val lhs1 = for (a, dx) <- lhs0 yield (a, nonEmptyRandomSubdomain(dx))
        val dy1 = nonEmptyRandomSubdomain(baseDomain)
        val dz1 = nonEmptyRandomSubdomain(dz0)
        when(domainPruner.linEqRule(lhs0, dy0)).thenReturn((lhs1.iterator.map(_._2), dy1))
        when(domainPruner.linEqRule(lhs1, dy1)).thenReturn((lhs1.iterator.map(_._2), dy1))
        if costsDomain == TrueDomain then relation match {
            case EqRelation =>
                when(domainPruner.eqRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.eqRule(dy1, dz1)).thenReturn((dy1, dz1))
            case NeRelation =>
                when(domainPruner.neRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.neRule(dy1, dz1)).thenReturn((dy1, dz1))
            case LtRelation =>
                when(domainPruner.ltRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.ltRule(dy1, dz1)).thenReturn((dy1, dz1))
            case LeRelation =>
                when(domainPruner.leRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.leRule(dy1, dz1)).thenReturn((dy1, dz1))
        }
        else if costsDomain == FalseDomain then relation match {
            case EqRelation =>
                when(domainPruner.neRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.neRule(dy1, dz1)).thenReturn((dy1, dz1))
            case NeRelation =>
                when(domainPruner.eqRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.eqRule(dy1, dz1)).thenReturn((dy1, dz1))
            case LtRelation =>
                when(domainPruner.leRule(dz0, dy1)).thenReturn((dz1, dy1))
                when(domainPruner.leRule(dz1, dy1)).thenReturn((dz1, dy1))
            case LeRelation =>
                when(domainPruner.ltRule(dz0, dy1)).thenReturn((dz1, dy1))
                when(domainPruner.ltRule(dz1, dy1)).thenReturn((dz1, dy1))
        } else if costsDomain == CompleteBooleanDomain then relation match {
            case EqRelation =>
                when(domainPruner.eqRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.neRule(dy1, dz0)).thenReturn((dy1, dz1))
            case NeRelation =>
                when(domainPruner.neRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.eqRule(dy1, dz0)).thenReturn((dy1, dz1))
            case LtRelation =>
                when(domainPruner.ltRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.leRule(dz0, dy1)).thenReturn((dz1, dy1))
            case LeRelation =>
                when(domainPruner.leRule(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.ltRule(dz0, dy1)).thenReturn((dz1, dy1))
        }
        space.post(constraint)
        space.propagate()
        if costsDomain.isSingleton then {
            for i <- axs.indices do {
                assertEq(axs(i).x.domain, lhs1(i)._2)
            }
            assertEq(z.domain, dz1)
            verify(domainPruner, atMost(2)).eqRule(any[D], any[D])
            verify(domainPruner, atMost(2)).neRule(any[D], any[D])
            verify(domainPruner, atMost(2)).ltRule(any[D], any[D])
            verify(domainPruner, atMost(2)).leRule(any[D], any[D])
            verify(domainPruner, times(2)).linEqRule(any[Iterable[(A, D)]], any[D])
        }
    }

    @Test
    def testCostComputation(): Unit = {
        setupTypeTraits()
        val maxViolation = 10
        space.post(constraint)
        for ax <- axs do {
            val x = ax.x
            space.setValue(x, x.domain.randomValue(randomGenerator))
            space.registerObjectiveVariable(x)
        }
        space.setValue(z, z.domain.randomValue(randomGenerator))
        space.registerObjectiveVariable(z)
        val now = space.searchState
        if true then {
            val a = axs.map(ax => ax.a * now.value(ax.x)).sum(using baseTypeTraits.numericalOperations)
            val b = now.value(z)
            val c = randomGenerator.nextInt(maxViolation).toLong
            relation match {
                case EqRelation => when(costModel.eqViolation(a, b)).thenReturn(c)
                case NeRelation => when(costModel.neViolation(a, b)).thenReturn(c)
                case LtRelation => when(costModel.ltViolation(a, b)).thenReturn(c)
                case LeRelation => when(costModel.leViolation(a, b)).thenReturn(c)
            }
            space.initialize()
            assertEq(now.value(costs).violation, c)
        }
        if true then {
            val move =
                new ChangeValues(
                    space.nextMoveId(),
                    (axs.map(_.x) :+ z).map(_.nextRandomMoveEffect(space, randomGenerator)))
            val a = axs.map(ax => ax.a * move.value(ax.x)).sum(using baseTypeTraits.numericalOperations)
            val b = move.value(z)
            val c = randomGenerator.nextInt(maxViolation).toLong
            relation match {
                case EqRelation => when(costModel.eqViolation(a, b)).thenReturn(c)
                case NeRelation => when(costModel.neViolation(a, b)).thenReturn(c)
                case LtRelation => when(costModel.ltViolation(a, b)).thenReturn(c)
                case LeRelation => when(costModel.leViolation(a, b)).thenReturn(c)
            }
            val after = space.consult(move)
            assertEq(after.value(costs).violation, c)
            space.commit(move)
            assertEq(now.value(costs).violation, c)
            space.initialize()
            assertEq(now.value(costs).violation, c)
        }
        relation match {
            case EqRelation => verify(costModel, times(3)).eqViolation(any[A], any[A])
            case NeRelation => verify(costModel, times(3)).neViolation(any[A], any[A])
            case LtRelation => verify(costModel, times(3)).ltViolation(any[A], any[A])
            case LeRelation => verify(costModel, times(3)).leViolation(any[A], any[A])
        }
    }

}
