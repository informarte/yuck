package yuck.constraints.test

import org.junit._

import yuck.constraints._
import yuck.core._
import yuck.test.util.{Mocking, UnitTest}

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
abstract class LinearConstraintLikeTest[Value <: NumericalValue[Value]] extends UnitTest with Mocking {

    protected val baseValueTraits: NumericalValueTraits[Value]

    protected val randomGenerator = new JavaRandomGenerator
    protected val space = new Space(logger, sigint)

    protected val relation: OrderingRelation
    protected val costsDomain: BooleanDecisionDomain
    protected val baseDomain: NumericalDomain[Value]
    protected val axs: IndexedSeq[AX[Value]]
    protected final lazy val y = baseValueTraits.createChannel(space)
    protected final lazy val z = baseValueTraits.createVariable(space, "z", baseDomain.randomSubdomain(randomGenerator))
    protected final val costs = new BooleanVariable(space.nextVariableId, "costs", costsDomain)
    protected def createConstraint(implicit valueTraits: NumericalValueTraits[Value]): Constraint

    protected def nonEmptyRandomSubdomain(d: NumericalDomain[Value]) =
        LazyList.continually(d).map(_.randomSubdomain(randomGenerator)).dropWhile(_.isEmpty).head

    // ScalaMock cannot mock vals, so we have to provide them using overrides,
    // see https://github.com/paulbutcher/ScalaMock/issues/148.
    private abstract class CostModel extends OrderingCostModel[Value]
    private abstract class DomainPruner extends NumericalDomainPruner[Value]
    private abstract class ValueTraits extends NumericalValueTraits[Value] {
        final override val orderingCostModel = mock[CostModel]
        final override val domainPruner = mock[DomainPruner]
        final override lazy val zero = baseValueTraits.zero
        final override lazy val one = baseValueTraits.one
        final override def createChannel(space: Space) = baseValueTraits.createChannel(space)
        final override def safeDowncast(x: AnyVariable) = baseValueTraits.safeDowncast(x)
    }
    private implicit val valueTraits = stub[ValueTraits]

    @Test
    def testSearchVariables: Unit = {
        space.post(createConstraint)
        assertEq(space.searchVariables, (axs.map(_.x) :+ z).toSet)
    }

    @Test
    def testPropagation: Unit = {
        // We simulate a propagation process where the first call to propagate computes a fixed point.
        val domainPruner = valueTraits.domainPruner
        val lhs0 = for (i <- 0 until axs.size) yield (axs(i).a, axs(i).x.domain)
        val dy0 = baseValueTraits.completeDomain
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
        space.post(createConstraint)
        space.propagate()
        if (costsDomain.isSingleton) {
            for (i <- 0 until axs.size) {
                assertEq(axs(i).x.domain, lhs1(i)._2)
            }
            assertEq(z.domain, dz1)
        }
    }

    @Test
    def testConsultAndCommit: Unit = {
        val costModel = valueTraits.orderingCostModel
        val maxViolation = 10
        space.post(createConstraint)
        for (ax <- axs) {
            val x = ax.x
            space.setValue(x, x.domain.randomValue(randomGenerator))
        }
        space.setValue(z, z.domain.randomValue(randomGenerator))
        val now = space.searchState
        if (true) {
            val a = axs.map{case AX(a, x) => a * now.value(x)}.sum(baseValueTraits.numericalOperations)
            val b = now.value(z)
            val c = randomGenerator.nextInt(maxViolation)
            relation match {
                case EqRelation => (costModel.eqViolation _).expects(a, b).returns(c)
                case NeRelation => (costModel.neViolation _).expects(a, b).returns(c)
                case LtRelation => (costModel.ltViolation _).expects(a, b).returns(c)
                case LeRelation => (costModel.leViolation _).expects(a, b).returns(c)
            }
            space.initialize()
            assertEq(now.value(costs).violation, c)
        }
        if (true) {
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    (axs.map(_.x) :+ z).map(_.nextRandomMoveEffect(space, randomGenerator)))
            val a = axs.map{case AX(a, x) => a * move.value(x)}.sum(baseValueTraits.numericalOperations)
            val b = move.value(z)
            val c = randomGenerator.nextInt(maxViolation)
            relation match {
                case EqRelation => (costModel.eqViolation _).expects(a, b).returns(c).twice()
                case NeRelation => (costModel.neViolation _).expects(a, b).returns(c).twice()
                case LtRelation => (costModel.ltViolation _).expects(a, b).returns(c).twice()
                case LeRelation => (costModel.leViolation _).expects(a, b).returns(c).twice()
            }
            val after = space.consult(move)
            assertEq(after.value(costs).violation, c)
            space.commit(move)
            assertEq(now.value(costs).violation, c)
            space.initialize()
            assertEq(now.value(costs).violation, c)
        }
    }

}
