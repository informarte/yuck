package yuck.constraints.test

import org.junit._

import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._

import scala.collection._
import scala.jdk.CollectionConverters._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class LinearConstraintTest
    (relation: OrderingRelation, costsDomain: BooleanDecisionDomain, withUnitCoefficients: Boolean)
    extends UnitTest
{

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val baseDomain = new IntegerRange(Zero, Nine)
    private val numberOfTerms = 3
    private val initialDomains =
        for (i <- 0 until numberOfTerms) yield baseDomain.randomSubdomain(randomGenerator)
    private val xs =
        for (i <- 0 until numberOfTerms) yield
            new IntegerVariable(space.nextVariableId, "x%d".format(i + 1), initialDomains(i))
    private val z = new IntegerVariable(space.nextVariableId, "z", baseDomain.randomSubdomain(randomGenerator))
    private val costs = new BooleanVariable(space.nextVariableId, "costs", costsDomain)
    private val axs = xs.map(AX(if (withUnitCoefficients) One else baseDomain.randomValue(randomGenerator), _))

    private def nonEmptyRandomSubdomain(d: IntegerDomain) =
        LazyList.continually(d).map(_.randomSubdomain(randomGenerator)).dropWhile(_.isEmpty).head

    @Test
    def testSearchVariables: Unit = {
        LinearConstraint.postLinearConstraint(space, null, axs, relation, z, costs)
        assertEq(space.searchVariables, (xs :+ z).toSet)
    }

    @Test
    def testPropagation: Unit = {
        // We simulate a propagation process where the first call to propagate computes a fixed point.
        class DomainPruner extends NumericalDomainPruner[IntegerValue] {
            override type DomainImpl = IntegerDomain
        }
        val domainPruner = mock(classOf[DomainPruner], RETURNS_SMART_NULLS)
        val lhs0 = for (i <- 0 until xs.size) yield (axs(i).a, initialDomains(i))
        val dy0 = CompleteIntegerRange
        val dz0 = z.domain
        val lhs1 = initialDomains.map(nonEmptyRandomSubdomain)
        val dy1 = nonEmptyRandomSubdomain(baseDomain)
        val dz1 = nonEmptyRandomSubdomain(dz0)
        when(domainPruner.linEq(lhs0, dy0)).thenReturn((lhs1.iterator, dy1))
        when(domainPruner.linEq(for (i <- 0 until xs.size) yield (axs(i).a, lhs1(i)), dy1)).thenReturn((lhs1.iterator, dy1))
        if (costsDomain.containsTrue) relation match {
            case EqRelation =>
                when(domainPruner.eq(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.eq(dy1, dz1)).thenReturn((dy1, dz1))
            case NeRelation =>
                when(domainPruner.ne(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.ne(dy1, dz1)).thenReturn((dy1, dz1))
            case LtRelation =>
                when(domainPruner.lt(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.lt(dy1, dz1)).thenReturn((dy1, dz1))
            case LeRelation =>
                when(domainPruner.le(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.le(dy1, dz1)).thenReturn((dy1, dz1))
        }
        if (costsDomain.containsFalse) relation match {
            case EqRelation =>
                when(domainPruner.ne(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.ne(dy1, dz1)).thenReturn((dy1, dz1))
            case NeRelation =>
                when(domainPruner.eq(dy1, dz0)).thenReturn((dy1, dz1))
                when(domainPruner.eq(dy1, dz1)).thenReturn((dy1, dz1))
            case LtRelation =>
                when(domainPruner.le(dz0, dy1)).thenReturn((dz1, dy1))
                when(domainPruner.le(dz1, dy1)).thenReturn((dz1, dy1))
            case LeRelation =>
                when(domainPruner.lt(dz0, dy1)).thenReturn((dz1, dy1))
                when(domainPruner.lt(dz1, dy1)).thenReturn((dz1, dy1))
        }
        implicit val valueTraits = mock(classOf[NumericalValueTraits[IntegerValue]], RETURNS_SMART_NULLS)
        when(valueTraits.one).thenReturn(One)
        when(valueTraits.domainPruner).thenReturn(domainPruner)
        when(valueTraits.createChannel(space)).thenAnswer(new Answer[NumericalVariable[IntegerValue]] {
            override def answer(invocation: InvocationOnMock) = {
                val space = invocation.getArgument(0, classOf[Space])
                IntegerValueTraits.createChannel(space)
            }
        })
        LinearConstraint.postLinearConstraint(space, null, axs, relation, z, costs)
        space.propagate
        if (costsDomain.isSingleton) {
            for (i <- 0 until xs.size) {
                assertEq(xs(i).domain, lhs1(i))
            }
            assertEq(z.domain, dz1)
            verify(domainPruner, atMost(2)).eq(any[IntegerDomain], any[IntegerDomain])
            verify(domainPruner, atMost(2)).ne(any[IntegerDomain], any[IntegerDomain])
            verify(domainPruner, atMost(2)).lt(any[IntegerDomain], any[IntegerDomain])
            verify(domainPruner, atMost(2)).le(any[IntegerDomain], any[IntegerDomain])
            verify(domainPruner, times(2)).linEq(any[Iterable[(IntegerValue, IntegerDomain)]], any[IntegerDomain])
        }

    }

    @Test
    def testConsultAndCommit: Unit = {
        val orderingCostModel = mock(classOf[OrderingCostModel[IntegerValue]], RETURNS_SMART_NULLS)
        implicit val valueTraits = mock(classOf[NumericalValueTraits[IntegerValue]], RETURNS_SMART_NULLS)
        when(valueTraits.orderingCostModel).thenReturn(orderingCostModel)
        when(valueTraits.zero).thenReturn(Zero)
        LinearConstraint.postLinearConstraint(space, null, axs, relation, z, costs)
        for (x <- xs) {
            space.setValue(x, x.domain.randomValue(randomGenerator))
        }
        space.setValue(z, z.domain.randomValue(randomGenerator))
        val now = space.searchState
        if (true) {
            val a = axs.map{case AX(a, x) => a * now.value(x)}.foldLeft(Zero)(_ + _)
            val b = now.value(z)
            val c = BooleanValue.get(baseDomain.randomValue(randomGenerator).value)
            relation match {
                case EqRelation => when(orderingCostModel.eq(a, b)).thenReturn(c)
                case NeRelation => when(orderingCostModel.ne(a, b)).thenReturn(c)
                case LtRelation => when(orderingCostModel.lt(a, b)).thenReturn(c)
                case LeRelation => when(orderingCostModel.le(a, b)).thenReturn(c)
            }
            space.initialize
            assertEq(now.value(costs), c)
        }
        if (true) {
            val move = new ChangeValues(space.nextMoveId, (xs :+ z).map(_.nextRandomMoveEffect(space, randomGenerator)))
            val a = axs.map{case AX(a, x) => a * move.value(x)}.foldLeft(Zero)(_ + _)
            val b = move.value(z)
            val c = BooleanValue.get(baseDomain.randomValue(randomGenerator).value)
            relation match {
                case EqRelation => when(orderingCostModel.eq(a, b)).thenReturn(c)
                case NeRelation => when(orderingCostModel.ne(a, b)).thenReturn(c)
                case LtRelation => when(orderingCostModel.lt(a, b)).thenReturn(c)
                case LeRelation => when(orderingCostModel.le(a, b)).thenReturn(c)
            }
            val after = space.consult(move)
            assertEq(after.value(costs), c)
            space.commit(move)
            assertEq(now.value(costs), c)
            space.initialize
            assertEq(now.value(costs), c)
        }
        relation match {
            case EqRelation => verify(orderingCostModel, times(3)).eq(any[IntegerValue], any[IntegerValue])
            case NeRelation => verify(orderingCostModel, times(3)).ne(any[IntegerValue], any[IntegerValue])
            case LtRelation => verify(orderingCostModel, times(3)).lt(any[IntegerValue], any[IntegerValue])
            case LeRelation => verify(orderingCostModel, times(3)).le(any[IntegerValue], any[IntegerValue])
        }
    }

}

/**
 * @author Michael Marte
 *
 */
final object LinearConstraintTest {

    private def configurations =
        for (relation <- List(EqRelation, NeRelation, LtRelation, LeRelation);
             costsDomain <- List(TrueDomain, FalseDomain, CompleteBooleanDecisionDomain);
             withUnitCoefficients <- List(true, false))
            yield Vector(relation, costsDomain, withUnitCoefficients)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}, {2}")
    def parameters = configurations.map(_.toArray).asJava

}
