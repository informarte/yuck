package yuck.constraints.test

import org.junit._

import org.mockito.AdditionalAnswers._
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._

import yuck.constraints._
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
abstract class LinearConstraintLikeTest[V <: NumericalValue[V]] extends UnitTest {

    protected val baseValueTraits: NumericalValueTraits[V]

    protected val randomGenerator = new JavaRandomGenerator

    private def nonEmptyRandomSubdomain(d: NumericalDomain[V]): NumericalDomain[V] =
        LazyList.continually(d).map(_.randomSubdomain(randomGenerator)).dropWhile(_.isEmpty).head

    protected val space = new Space(logger, sigint)

    protected val relation: OrderingRelation
    protected val costsDomain: BooleanDecisionDomain
    protected val baseDomain: NumericalDomain[V]
    protected val axs: IndexedSeq[AX[V]]
    protected final lazy val y = baseValueTraits.createChannel(space)
    protected final lazy val z = baseValueTraits.createVariable(space, "z", nonEmptyRandomSubdomain(baseDomain))
    protected final val costs = new BooleanVariable(space.nextVariableId, "costs", costsDomain)
    protected def createConstraint(implicit valueTraits: NumericalValueTraits[V]): Constraint

    private val orderingCostModel = mock(classOf[OrderingCostModel[V]])
    private val domainPruner = mock(classOf[NumericalDomainPruner[V]])
    private implicit val valueTraits: NumericalValueTraits[V] = mock(classOf[NumericalValueTraits[V]])

    private def setupValueTraits(): Unit = {
        when(valueTraits.orderingCostModel).thenReturn(orderingCostModel)
        when(valueTraits.domainPruner).thenReturn(domainPruner)
        when(valueTraits.zero).thenReturn(baseValueTraits.zero)
        when(valueTraits.one).thenReturn(baseValueTraits.one)
        when(valueTraits.createChannel(any[Space])).thenAnswer(answer(baseValueTraits.createChannel(_)))
        when(valueTraits.safeDowncast(any[AnyVariable])).thenAnswer(answer(baseValueTraits.safeDowncast(_: AnyVariable)))
    }

    @Test
    def testBasics: Unit = {
        setupValueTraits()
        val constraint = createConstraint
        assertEq(constraint.toString, "sum([%s], %s, %s, %s)".format(axs.mkString(", "), relation, z, costs))
        assertEq(constraint.inVariables.size, axs.size + 1)
        assertEq(constraint.inVariables.toSet, axs.map(_.x).toSet.union(Set(z)))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation: Unit = {
        setupValueTraits()
        // We simulate a propagation process where the first call to propagate computes a fixed point.
        val lhs0 = for (i <- 0 until axs.size) yield (axs(i).a, axs(i).x.domain)
        val dy0 = baseValueTraits.completeDomain
        val dz0 = z.domain
        val lhs1 = for ((a, dx) <- lhs0) yield (a, nonEmptyRandomSubdomain(dx))
        val dy1 = nonEmptyRandomSubdomain(baseDomain)
        val dz1 = nonEmptyRandomSubdomain(dz0)
        when(domainPruner.linEqRule(lhs0, dy0)).thenReturn((lhs1.iterator.map(_._2), dy1))
        when(domainPruner.linEqRule(lhs1, dy1)).thenReturn((lhs1.iterator.map(_._2), dy1))
        if (costsDomain == TrueDomain) relation match {
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
        else if (costsDomain == FalseDomain) relation match {
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
        } else if (costsDomain == CompleteBooleanDecisionDomain) relation match {
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
        space.post(createConstraint)
        space.propagate()
        if (costsDomain.isSingleton) {
            for (i <- 0 until axs.size) {
                assertEq(axs(i).x.domain, lhs1(i)._2)
            }
            assertEq(z.domain, dz1)
            verify(domainPruner, atMost(2)).eqRule(any[Domain[V]], any[Domain[V]])
            verify(domainPruner, atMost(2)).neRule(any[Domain[V]], any[Domain[V]])
            verify(domainPruner, atMost(2)).ltRule(any[OrderedDomain[V]], any[OrderedDomain[V]])
            verify(domainPruner, atMost(2)).leRule(any[OrderedDomain[V]], any[OrderedDomain[V]])
            verify(domainPruner, times(2)).linEqRule(any[Iterable[(V, NumericalDomain[V])]], any[NumericalDomain[V]])
        }
    }

    @Test
    def testCostComputation: Unit = {
        setupValueTraits()
        val maxViolation = 10
        space.post(createConstraint)
        for (ax <- axs) {
            val x = ax.x
            space.setValue(x, x.domain.randomValue(randomGenerator))
        }
        space.setValue(z, z.domain.randomValue(randomGenerator))
        val now = space.searchState
        if (true) {
            val a = axs.map{case ax => ax.a * now.value(ax.x)}.sum(baseValueTraits.numericalOperations)
            val b = now.value(z)
            val c = randomGenerator.nextInt(maxViolation).toLong
            relation match {
                case EqRelation => when(orderingCostModel.eqViolation(a, b)).thenReturn(c)
                case NeRelation => when(orderingCostModel.neViolation(a, b)).thenReturn(c)
                case LtRelation => when(orderingCostModel.ltViolation(a, b)).thenReturn(c)
                case LeRelation => when(orderingCostModel.leViolation(a, b)).thenReturn(c)
            }
            space.initialize()
            assertEq(now.value(costs).violation, c)
        }
        if (true) {
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    (axs.map(_.x) :+ z).map(_.nextRandomMoveEffect(space, randomGenerator)))
            val a = axs.map{case ax => ax.a * move.value(ax.x)}.sum(baseValueTraits.numericalOperations)
            val b = move.value(z)
            val c = randomGenerator.nextInt(maxViolation).toLong
            relation match {
                case EqRelation => when(orderingCostModel.eqViolation(a, b)).thenReturn(c)
                case NeRelation => when(orderingCostModel.neViolation(a, b)).thenReturn(c)
                case LtRelation => when(orderingCostModel.ltViolation(a, b)).thenReturn(c)
                case LeRelation => when(orderingCostModel.leViolation(a, b)).thenReturn(c)
            }
            val after = space.consult(move)
            assertEq(after.value(costs).violation, c)
            space.commit(move)
            assertEq(now.value(costs).violation, c)
            space.initialize()
            assertEq(now.value(costs).violation, c)
        }
        relation match {
            case EqRelation => verify(orderingCostModel, times(3)).eqViolation(any[V], any[V])
            case NeRelation => verify(orderingCostModel, times(3)).neViolation(any[V], any[V])
            case LtRelation => verify(orderingCostModel, times(3)).ltViolation(any[V], any[V])
            case LeRelation => verify(orderingCostModel, times(3)).leViolation(any[V], any[V])
        }
    }

}
