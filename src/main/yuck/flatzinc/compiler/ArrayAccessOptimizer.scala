package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.{ElementVar, ElementsVar}
import yuck.core.*

/**
 * Tries to replace ElementVar with ElementsVar constraints.
 *
 * Tries to reduce the number of inputs by removing or replacing those array members which cannot be indexed.
 *
 * @author Michael Marte
 */
final class ArrayAccessOptimizer
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    override def run() = {
        val (layers, _) = cc.logger.withTimedLogScope("Computing layers") {
            cc.space.computeLayers()
        }
        layers.foreach(optimizeArrayAccess)
    }

    private def optimizeArrayAccess(layer: Set[yuck.core.Constraint]): Unit = {
        val before = cc.space.searchVariables
        optimizeVarArrayAccess(layer)
        val after = cc.space.searchVariables
        assert(after.subsetOf(before))
    }

    private def optimizeVarArrayAccess(layer: Set[yuck.core.Constraint]): Unit = {
        val elementVarConstraints: Map[(immutable.IndexedSeq[Variable[?]], Int), Vector[ElementVar[?]]] =
            layer.view
                .filter(_.isInstanceOf[ElementVar[?]])
                .map(_.asInstanceOf[ElementVar[?]])
                .filterNot(constraint => constraint.xs.contains(constraint.i))
                .groupBy(constraint => (constraint.xs, constraint.offset))
                .view
                .mapValues(_.toVector)
                .toMap
        for (((xs, offset), constraints) <- elementVarConstraints) {
            val n = xs.size
            val m = constraints.size
            if (m * (n + 1) > 3 * (n + m)) {
                /*
                When n = |xs| is large, then each change to one of the xs triggers m ElementVar constraints
                which, together with the scheduling overhead, can be quite expensive. Hence, to avoid most of
                the scheduling overhead, the idea is to replace the ElementVar constraints with a single
                ElementsVar constraint. However, ElementsVar comes with internal management overhead, so the
                challenge is to determine the break-even point. The above formula is based on the assumption
                that changes to the variables (including indices) are evenly distributed. The costs of using
                ElementsVar (relative to that of ElementVar) is captured by a constant which was determined
                empirically. (It includes the scheduling overhead and is surely inaccurate to some extent
                but does the job.)
                */
                cc.logger.log("Merging %d ElementVar constraints".format(constraints.size))
                constraints.foreach(cc.space.retract)
                val is = constraints.map(_.i)
                val ys = constraints.map(_.y)
                inline def postConstraint[V <: Value[V]]()(using valueTraits: ValueTraits[V]): Unit = {
                    val (xs1, offset1) =
                        uselessInputsRemoved(
                            xs.asInstanceOf[immutable.IndexedSeq[Variable[V]]],
                            is.foldLeft(IntegerValueTraits.emptyDomain)((u, i) => u.union(i.domain)),
                            offset)
                    if (xs1 != xs) {
                        cc.logger.log(
                            "Dropping %d inputs while merging ElementVar constraints".format(xs.size - xs1.size))
                    }
                    cc.post(
                        new ElementsVar(
                            constraints.head.id,
                            constraints.head.maybeGoal,
                            xs1.asInstanceOf[immutable.IndexedSeq[Variable[V]]],
                            is,
                            ys.asInstanceOf[immutable.IndexedSeq[Variable[V]]],
                            offset1))
                }
                xs.head.match {
                    case _: BooleanVariable => postConstraint[BooleanValue]()
                    case _: IntegerVariable => postConstraint[IntegerValue]()
                    case _: IntegerSetVariable => postConstraint[IntegerSetValue]()
                }
            } else for (constraint <- constraints) {
                val (xs1, offset1) = uselessInputsRemoved(constraint.xs, constraint.i.domain, constraint.offset)
                inline def postConstraint[V <: Value[V]]()(using valueTraits: ValueTraits[V]): Unit = {
                    cc.post(
                        new ElementVar(
                            constraint.id,
                            constraint.maybeGoal,
                            xs1.asInstanceOf[immutable.IndexedSeq[Variable[V]]],
                            constraint.i,
                            constraint.y.asInstanceOf[Variable[V]],
                            offset1))
                }
                if (xs1 != xs) {
                    cc.logger.log("Dropping %d inputs from ElementVar constraint".format(xs.size - xs1.size))
                    cc.space.retract(constraint)
                    xs.head.match {
                        case _: BooleanVariable => postConstraint[BooleanValue]()
                        case _: IntegerVariable => postConstraint[IntegerValue]()
                        case _: IntegerSetVariable => postConstraint[IntegerSetValue]()
                    }
                }
            }
        }
    }

    /*
    If xs(j) does not play a role (because j is not a valid index), then there is no need
    to monitor xs(j) and we either drop or replace it by some indexable xs(j').
    This way we remove an useless arc from the constraint network.
    */
    private def uselessInputsRemoved
        [V <: Value[V]]
        (xs: IndexedSeq[Variable[V]], indices: IntegerDomain, offset: Int):
        (Vector[Variable[V]], Int) =
    {
        val indexRange = IntegerRange(offset, offset + xs.size - 1)
        val indexRange1 = indexRange.intersect(indices.hull)
        val offset1 = indexRange1.lb.toInt
        val xs1 = xs.drop(max(0, indices.lb.toInt - indexRange.lb.toInt)).take(indexRange1.size)
        val xs2 = indexRange1.values.iterator
            .map(j => xs1((if indices.contains(j) then j else indices.lb).toInt - offset1))
            .toVector
        (xs2, offset1)
    }

}
