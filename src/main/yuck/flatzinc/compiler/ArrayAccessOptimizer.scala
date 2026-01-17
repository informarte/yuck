package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.{ElementVar, ElementsVar}
import yuck.core.*

/**
 * Tries to replace ElementVar with ElementsVar constraints.
 *
 * Tries to reduce the number of inputs by removing or replacing those array members which cannot be indexed.
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
        val elementVarConstraints: Map[(immutable.IndexedSeq[Variable[?, ?, ?]], Int), Vector[ElementVar[?, ?, ?]]] =
            layer.view
                .filter(_.isInstanceOf[ElementVar[?, ?, ?]])
                .map(_.asInstanceOf[ElementVar[?, ?, ?]])
                .filterNot(constraint => constraint.xs.contains(constraint.i))
                .groupBy(constraint => (constraint.xs.asInstanceOf[immutable.IndexedSeq[Variable[?, ?, ?]]], constraint.offset))
                .view
                .mapValues(_.toVector)
                .toMap
        for ((xs, offset), constraints) <- elementVarConstraints do {
            val n = xs.size
            val m = constraints.size
            if m * (n + 1) > 3 * (n + m) then {
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
                val goals = constraints.view.flatMap(cc.space.goals).toSet
                constraints.foreach(cc.space.retract)
                val is = constraints.map(_.i)
                val ys = constraints.map(_.y)
                inline def postConstraint
                    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
                    ()
                    (using typeTraits: TypeTraits[A, D, X]):
                    Unit =
                {
                    val (xs1, offset1) =
                        uselessInputsRemoved(
                            xs.asInstanceOf[immutable.IndexedSeq[X]],
                            is.foldLeft(IntegerTypeTraits.emptyDomain)((u, i) => u.union(i.domain)),
                            offset)
                    if xs1 != xs then {
                        cc.logger.log(
                            "Dropping %d inputs while merging ElementVar constraints".format(xs.size - xs1.size))
                    }
                    cc.post(
                        goals,
                        new ElementsVar(
                            constraints.head.id,
                            xs1.asInstanceOf[immutable.IndexedSeq[X]],
                            is,
                            ys.asInstanceOf[immutable.IndexedSeq[X]],
                            offset1))
                }
                xs.head.match {
                    case _: BooleanVariable => postConstraint()(using BooleanTypeTraits)
                    case _: IntegerVariable => postConstraint()(using IntegerTypeTraits)
                    case _: IntegerSetVariable => postConstraint()(using IntegerSetTypeTraits)
                }
            } else for constraint <- constraints do {
                val (xs1, offset1) = uselessInputsRemoved(constraint.xs, constraint.i.domain, constraint.offset)
                val goals = cc.space.goals(constraint)
                inline def postConstraint
                    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
                    ()
                    (using typeTraits: TypeTraits[A, D, X]):
                    Unit =
                {
                    cc.post(
                        goals,
                        new ElementVar(
                            constraint.id,
                            xs1.asInstanceOf[immutable.IndexedSeq[X]],
                            constraint.i,
                            constraint.y.asInstanceOf[X],
                            offset1))
                }
                if xs1 != xs then {
                    cc.logger.log("Dropping %d inputs from ElementVar constraint".format(xs.size - xs1.size))
                    cc.space.retract(constraint)
                    xs.head.match {
                        case _: BooleanVariable => postConstraint()(using BooleanTypeTraits)
                        case _: IntegerVariable => postConstraint()(using IntegerTypeTraits)
                        case _: IntegerSetVariable => postConstraint()(using IntegerSetTypeTraits)
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
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        (xs: IndexedSeq[X], indices: IntegerDomain, offset: Int):
        (Vector[X], Int) =
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
