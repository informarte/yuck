package yuck.constraints.test

import org.junit._

import scala.jdk.CollectionConverters._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.{UnitTest, Mocking}

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
    with Mocking
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
        abstract class DomainPruner extends NumericalDomainPruner[IntegerValue] {
        }
        abstract class ValueTraits extends NumericalValueTraits[IntegerValue] {
            final override val domainPruner = mock[DomainPruner]
            final override val zero = Zero
            final override def createChannel(space: Space) = IntegerValueTraits.createChannel(space)
        }
        implicit val valueTraits = stub[ValueTraits]
        val domainPruner = valueTraits.domainPruner
        val lhs0 = for (i <- 0 until xs.size) yield (axs(i).a, initialDomains(i))
        val dy0 = CompleteIntegerRange
        val dz0 = z.domain
        val lhs1 = for ((a, d) <- lhs0) yield (a, nonEmptyRandomSubdomain(d))
        val dy1 = nonEmptyRandomSubdomain(baseDomain)
        val dz1 = nonEmptyRandomSubdomain(dz0)
        (domainPruner.linEqRule _).expects(lhs0, dy0).returns((lhs1.iterator.map(_._2), dy1))
        (domainPruner.linEqRule _).expects(lhs1, dy1).returns((lhs1.iterator.map(_._2), dy1))
        if (costsDomain == TrueDomain) relation match {
            case EqRelation =>
                (domainPruner.eqRule _).expects(dy1, dz0).returns((dy1, dz1))
                (domainPruner.eqRule _).expects(dy1, dz1).returns((dy1, dz1))
            case NeRelation =>
                (domainPruner.neRule _).expects(dy1, dz0).returns((dy1, dz1))
                (domainPruner.neRule _).expects(dy1, dz1).returns((dy1, dz1))
            case LtRelation =>
                (domainPruner.ltRule _).expects(dy1, dz0).returns((dy1, dz1))
                (domainPruner.ltRule _).expects(dy1, dz1).returns((dy1, dz1))
            case LeRelation =>
                (domainPruner.leRule _).expects(dy1, dz0).returns((dy1, dz1))
                (domainPruner.leRule _).expects(dy1, dz1).returns((dy1, dz1))
        }
        else if (costsDomain == FalseDomain) relation match {
            case EqRelation =>
                (domainPruner.neRule _).expects(dy1, dz0).returns((dy1, dz1))
                (domainPruner.neRule _).expects(dy1, dz1).returns((dy1, dz1))
            case NeRelation =>
                (domainPruner.eqRule _).expects(dy1, dz0).returns((dy1, dz1))
                (domainPruner.eqRule _).expects(dy1, dz1).returns((dy1, dz1))
            case LtRelation =>
                (domainPruner.leRule _).expects(dz0, dy1).returns((dz1, dy1))
                (domainPruner.leRule _).expects(dz1, dy1).returns((dz1, dy1))
            case LeRelation =>
                (domainPruner.ltRule _).expects(dz0, dy1).returns((dz1, dy1))
                (domainPruner.ltRule _).expects(dz1, dy1).returns((dz1, dy1))
        } else if (costsDomain == CompleteBooleanDecisionDomain) relation match {
            case EqRelation =>
                (domainPruner.eqRule _).expects(dy1, dz0).returns((dy1, dz1)).twice()
                (domainPruner.neRule _).expects(dy1, dz0).returns((dy1, dz1)).twice()
            case NeRelation =>
                (domainPruner.neRule _).expects(dy1, dz0).returns((dy1, dz1)).twice()
                (domainPruner.eqRule _).expects(dy1, dz0).returns((dy1, dz1)).twice()
            case LtRelation =>
                (domainPruner.ltRule _).expects(dy1, dz0).returns((dy1, dz1)).twice()
                (domainPruner.leRule _).expects(dz0, dy1).returns((dz1, dy1)).twice()
            case LeRelation =>
                (domainPruner.leRule _).expects(dy1, dz0).returns((dy1, dz1)).twice()
                (domainPruner.ltRule _).expects(dz0, dy1).returns((dz1, dy1)).twice()
        }
        LinearConstraint.postLinearConstraint(space, null, axs, relation, z, costs)
        space.propagate
        if (costsDomain.isSingleton) {
            for (i <- 0 until xs.size) {
                assertEq(xs(i).domain, lhs1(i)._2)
            }
            assertEq(z.domain, dz1)
        }
    }

    @Test
    def testConsultAndCommit: Unit = {
        abstract class ValueTraits extends NumericalValueTraits[IntegerValue] {
            final override val orderingCostModel = mock[OrderingCostModel[IntegerValue]]
            final override val zero = Zero
        }
        implicit val valueTraits = stub[ValueTraits]
        val orderingCostModel = valueTraits.orderingCostModel
        LinearConstraint.postLinearConstraint(space, null, axs, relation, z, costs)
        for (x <- xs) {
            space.setValue(x, x.domain.randomValue(randomGenerator))
        }
        space.setValue(z, z.domain.randomValue(randomGenerator))
        val now = space.searchState
        if (true) {
            val a = axs.map{case AX(a, x) => a * now.value(x)}.sum
            val b = now.value(z)
            val c = BooleanValue.get(baseDomain.randomValue(randomGenerator).value)
            relation match {
                case EqRelation => (orderingCostModel.eqViolation _).expects(a, b).returns(c)
                case NeRelation => (orderingCostModel.neViolation _).expects(a, b).returns(c)
                case LtRelation => (orderingCostModel.ltViolation _).expects(a, b).returns(c)
                case LeRelation => (orderingCostModel.leViolation _).expects(a, b).returns(c)
            }
            space.initialize
            assertEq(now.value(costs), c)
        }
        if (true) {
            val move = new ChangeValues(space.nextMoveId, (xs :+ z).map(_.nextRandomMoveEffect(space, randomGenerator)))
            val a = axs.map{case AX(a, x) => a * move.value(x)}.sum
            val b = move.value(z)
            val c = BooleanValue.get(baseDomain.randomValue(randomGenerator).value)
            relation match {
                case EqRelation => (orderingCostModel.eqViolation _).expects(a, b).returns(c).twice()
                case NeRelation => (orderingCostModel.neViolation _).expects(a, b).returns(c).twice()
                case LtRelation => (orderingCostModel.ltViolation _).expects(a, b).returns(c).twice()
                case LeRelation => (orderingCostModel.leViolation _).expects(a, b).returns(c).twice()
            }
            val after = space.consult(move)
            assertEq(after.value(costs), c)
            space.commit(move)
            assertEq(now.value(costs), c)
            space.initialize
            assertEq(now.value(costs), c)
        }
    }

}

/**
 * @author Michael Marte
 *
 */
object LinearConstraintTest {

    private def configurations =
        for (relation <- List(EqRelation, NeRelation, LtRelation, LeRelation);
             costsDomain <- List(TrueDomain, FalseDomain, CompleteBooleanDecisionDomain);
             withUnitCoefficients <- List(true, false))
            yield Vector(relation, costsDomain, withUnitCoefficients)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}, {2}")
    def parameters = configurations.map(_.toArray).asJava

}
