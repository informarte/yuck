package yuck.constraints.test

import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.constraints.*
import yuck.constraints.OrderingRelation.*
import yuck.core.*

@ParameterizedClass
@MethodSource(Array("parameters"))
final class LinearConstraintTest
    (override protected val relation: OrderingRelation,
     override protected val costsDomain: BooleanDomain)
    extends LinearConstraintLikeTest[IntegerValue, IntegerDomain, IntegerVariable]
{

    private val numberOfTerms = 3
    override protected val baseTypeTraits = IntegerTypeTraits
    override protected val baseDomain: IntegerRange = IntegerRange(0, 9)
    override protected val axs =
        for i <- 1 to numberOfTerms yield AX(
            baseDomain.randomValue(randomGenerator),
            new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain.randomSubdomain(randomGenerator)))
    override protected lazy val constraint =
        new LinearConstraint(space.nextConstraintId(), axs, y, relation, z, costs)(using typeTraits)

    override protected def testAxs(constraint: LinearConstraintLike[?, ?, ?]): Unit = {
        assertEq(constraint.asInstanceOf[LinearConstraint[?, ?, ?]].axs, axs)
    }

}

object LinearConstraintTest {

    private def configurations =
        for relation <- List(EqRelation, NeRelation, LtRelation, LeRelation)
            costsDomain <- List(TrueDomain, FalseDomain, CompleteBooleanDomain)
        yield
            Array(relation, costsDomain)

    def parameters = configurations.toArray

}
