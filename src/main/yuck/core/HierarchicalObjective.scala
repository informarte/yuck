package yuck.core

/**
 * Implements a lexicographic ordering on the given objectives.
 *
 * @author Michael Marte
 */
final class HierarchicalObjective(
    override val primitiveObjectives: List[PrimitiveObjective],
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
    override def compareCosts(lhs: Costs, rhs: Costs) =
        compareCosts(
            primitiveObjectives,
            lhs.asInstanceOf[PolymorphicListValue].value,
            rhs.asInstanceOf[PolymorphicListValue].value)
    private def compareCosts(objectives: List[AnyObjective], lhs: List[Costs], rhs: List[Costs]): Int =
        (objectives, lhs, rhs) match {
            case (Nil, Nil, Nil) => 0
            case (o :: t, l :: u, r :: v) => {
                val result = o.compareCosts(l, r)
                if (result == 0) compareCosts(t, u, v) else result
            }
            case _ => ???
        }
    override def assessMove(before: SearchState, after: SearchState) =
        assessMove(primitiveObjectives, before, after)
    private def assessMove(objectives: List[AnyObjective], before: SearchState, after: SearchState): Double =
        objectives match {
            case Nil => 0
            case (h :: t) => {
                val delta = h.assessMove(before, after)
                if (delta == 0) assessMove(t, before, after) else delta
            }
        }
    override def tighten(space: Space, rootObjective: AnyObjective) =
        tighten(space, primitiveObjectives, rootObjective)
    private def tighten
        (space: Space, objectives: List[AnyObjective], rootObjective: AnyObjective):
        TighteningResult =
    {
        objectives match {
            case Nil => TighteningResult(space.searchState, None)
            case (h :: t) =>
                if (h.isGoodEnough(h.costs(space.searchState))) tighten(space, t, rootObjective)
                else h.tighten(space, rootObjective)
        }
    }
}
