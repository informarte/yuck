package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.*
import yuck.constraints.OrderingRelation.*
import yuck.core.{given, *}

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class LinearConstraintTest
    (override protected val relation: OrderingRelation,
     override protected val costsDomain: BooleanDomain)
    extends LinearConstraintLikeTest[IntegerValue]
{
    private val NumberOfTerms = 3
    override protected val baseValueTraits = IntegerValueTraits
    override protected val baseDomain: IntegerRange = IntegerRange(0, 9)
    override protected val axs =
        for (i <- 1 to NumberOfTerms) yield AX(
            baseDomain.randomValue(randomGenerator),
            new IntegerVariable(
                space.nextVariableId(), "x%d".format(i), baseDomain.randomSubdomain(randomGenerator)))
    override protected def createConstraint(using valueTraits: NumericalValueTraits[IntegerValue]) =
        new LinearConstraint(space.nextConstraintId(), null, axs, y, relation, z, costs)(using valueTraits)
}

/**
 * @author Michael Marte
 *
 */
object LinearConstraintTest {

    private def configurations =
        for (relation <- List(EqRelation, NeRelation, LtRelation, LeRelation);
             costsDomain <- List(TrueDomain, FalseDomain, CompleteBooleanDomain))
            yield Vector(relation, costsDomain)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
