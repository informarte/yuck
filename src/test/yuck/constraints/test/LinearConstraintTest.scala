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
    (override protected val relation: OrderingRelation,
     override protected val costsDomain: BooleanDecisionDomain)
    extends LinearConstraintLikeTest[IntegerValue]
{
    override protected val baseValueTraits = IntegerValueTraits
    override protected val baseDomain = IntegerRange(Zero, Nine)
    private val numberOfTerms = 3
    override protected val axs =
        for (i <- 0 until numberOfTerms) yield AX(
            baseDomain.randomValue(randomGenerator),
            new IntegerVariable(
                space.nextVariableId, "x%d".format(i + 1), baseDomain.randomSubdomain(randomGenerator)))
    override protected def createConstraint(implicit valueTraits: NumericalValueTraits[IntegerValue]) =
        new LinearConstraint(space.nextConstraintId, null, axs, y, relation, z, costs)(valueTraits)
}

/**
 * @author Michael Marte
 *
 */
object LinearConstraintTest {

    private def configurations =
        for (relation <- List(EqRelation, NeRelation, LtRelation, LeRelation);
             costsDomain <- List(TrueDomain, FalseDomain, CompleteBooleanDecisionDomain))
            yield Vector(relation, costsDomain)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
