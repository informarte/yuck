package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.Eq
import yuck.core.*
import yuck.util.Collections.*

/**
 * Turns the constraint graph into a DAG by introducing channels and equality constraints.
 */
final class CycleBreaker(override protected val cc: CompilationContext) extends CompilationPhase {

    override def run(): Unit = {
        cc.space.breakCycles(breakCycle)
    }

    private def breakCycle(scc: Set[Constraint]): Unit = {
        def feedsIntoScc(x: AnyVariable) = cc.space.directlyAffectedConstraints(x).intersects(scc)
        def inDegreeToOutDegreeRatio(constraint: Constraint) =
            constraint.inVariables.size.toFloat / constraint.outVariables.count(feedsIntoScc).toFloat
        val constraint = scc.maxBy(inDegreeToOutDegreeRatio)
        cc.logger.log("Breaking cycle at %s".format(constraint))
        val replacements = constraint.outVariables.view.filter(feedsIntoScc).map(x => (x, createChannel(x))).toMap
        cc.space.retract(constraint)
        cc.space.post(constraint.copy(replacements))
        for ((x, y) <- replacements) {
            val costs = createBoolChannel()
            cc.space.post(createEqConstraint(x, y, costs))
            cc.costVars += costs
        }
    }

    private def createChannel(x: AnyVariable): AnyVariable = x match {
        case _: BooleanVariable => createBoolChannel()
        case _: IntegerVariable => createIntChannel()
        case _: IntegerSetVariable => createIntSetChannel()
    }

    private def createEqConstraint(x: AnyVariable, y: AnyVariable, costs: BooleanVariable): Constraint = x match {
        case _: BooleanVariable =>
            new Eq(cc.space.nextConstraintId(), x.asInstanceOf[BooleanVariable], y.asInstanceOf[BooleanVariable], costs)
        case _: IntegerVariable =>
            new Eq(cc.space.nextConstraintId(), x.asInstanceOf[IntegerVariable], y.asInstanceOf[IntegerVariable], costs)
        case _: IntegerSetVariable =>
            new Eq(cc.space.nextConstraintId(), x.asInstanceOf[IntegerSetVariable], y.asInstanceOf[IntegerSetVariable], costs)
    }

}
