package yuck.core

import scala.annotation.tailrec

/**
 * Implements a lexicographic ordering on the given objectives.
 *
 * @author Michael Marte
 */
final class HierarchicalObjective
    (override val primitiveObjectives: List[PrimitiveObjective],
     focusOnTopObjective: Boolean,
     firstSolutionIsGoodEnough: Boolean)
    extends AnyObjective
{
    require(! primitiveObjectives.isEmpty)
    override def toString =
        primitiveObjectives.toString
    override def objectiveVariables =
        primitiveObjectives.iterator.flatMap(_.objectiveVariables).toSeq
    override def targetCosts =
        new PolymorphicListValue(primitiveObjectives.map(_.targetCosts))
    override def costs(searchState: SearchState) =
        new PolymorphicListValue(primitiveObjectives.map(_.costs(searchState)))
    override def isSolution(costs: Costs) =
        primitiveObjectives.head.isSolution(costs.asInstanceOf[PolymorphicListValue].value.head)
    override def isSolution(searchState: SearchState) =
        primitiveObjectives.head.isSolution(searchState)
    override def isGoodEnough(costs: Costs) =
        isSolution(costs) && (firstSolutionIsGoodEnough || super.isGoodEnough(costs))
    override def isGoodEnough(searchState: SearchState) =
        isSolution(searchState) && (firstSolutionIsGoodEnough || super.isGoodEnough(searchState))
    override def isOptimal(costs: Costs) =
        primitiveObjectives.iterator.zip(costs.asInstanceOf[PolymorphicListValue].value)
            .forall{case (objective, costs) => objective.isOptimal(costs)}
    override def isOptimal(searchState: SearchState) =
        primitiveObjectives.forall(_.isOptimal(searchState))
    override def compareCosts(lhs0: Costs, rhs0: Costs) = {
        val lhs = lhs0.asInstanceOf[PolymorphicListValue].value
        val rhs = rhs0.asInstanceOf[PolymorphicListValue].value
        if (focusOnTopObjective) {
            val topObjective :: subordinateObjectives = primitiveObjectives
            if (topObjective.isSolution(lhs.head) && topObjective.isSolution(rhs.head)) {
                compareCosts(subordinateObjectives, lhs.tail, rhs.tail)
            } else {
                topObjective.compareCosts(lhs.head, rhs.head)
            }
        } else {
            compareCosts(primitiveObjectives, lhs, rhs)
        }
    }
    @tailrec
    private def compareCosts(objectives: List[AnyObjective], lhs: List[Costs], rhs: List[Costs]): Int =
        (objectives, lhs, rhs) match {
            case (Nil, Nil, Nil) => 0
            case (o :: t, l :: u, r :: v) => {
                val result = o.compareCosts(l, r)
                if (result == 0) compareCosts(t, u, v) else result
            }
            case _ => ???
        }
    override def assessMove(before: SearchState, after: SearchState) = {
        if (focusOnTopObjective) {
            val topObjective :: subordinateObjectives = primitiveObjectives
            if (topObjective.isSolution(before) && topObjective.isSolution(after)) {
                assessMove(subordinateObjectives, before, after)
            } else {
                topObjective.assessMove(before, after)
            }
        } else {
            assessMove(primitiveObjectives, before, after)
        }
    }
    @tailrec
    private def assessMove(objectives: List[AnyObjective], before: SearchState, after: SearchState): Double =
        objectives match {
            case Nil => 0
            case (h :: t) => {
                val delta = h.assessMove(before, after)
                if (delta == 0) assessMove(t, before, after) else delta
            }
        }
    override private[core] def findActualObjectiveValue(space: Space, rootObjective: AnyObjective) =
        primitiveObjectives.foreach(_.findActualObjectiveValue(space, rootObjective))
    override def tighten(space: Space) =
        tighten(space, primitiveObjectives)
    private def tighten(space: Space, objectives: List[AnyObjective]): Set[AnyVariable] =
        objectives match {
            case Nil => Set.empty
            case (h :: t) =>
                if (h.isGoodEnough(space.searchState)) h.tighten(space).union(tighten(space, t))
                else h.tighten(space)
        }
}
