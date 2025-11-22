package yuck.constraints.test

import scala.jdk.CollectionConverters.*

import org.junit.*

import yuck.constraints.*
import yuck.constraints.OrderingRelation.*
import yuck.core.*

@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class SumConstraintTest
    (override protected val relation: OrderingRelation,
     override protected val costsDomain: BooleanDomain)
    extends LinearConstraintLikeTest[IntegerValue]
{
    private val numberOfTerms = 3
    override protected val baseValueTraits = IntegerValueTraits
    override protected val baseDomain: IntegerRange = IntegerRange(0, 9)
    override protected val axs =
        for (i <- 1 to numberOfTerms) yield AX(
            One,
            new IntegerVariable(
                space.nextVariableId(), "x%d".format(i), baseDomain.randomSubdomain(randomGenerator)))
    override protected def createConstraint(using valueTraits: NumericalValueTraits[IntegerValue]) =
        new SumConstraint(space.nextConstraintId(), null, axs.map(_.x), y, relation, z, costs)(using valueTraits)
}

object SumConstraintTest {

    private def configurations =
        for (relation <- List(EqRelation, NeRelation, LtRelation, LeRelation);
             costsDomain <- List(TrueDomain, FalseDomain, CompleteBooleanDomain))
            yield Vector(relation, costsDomain)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
