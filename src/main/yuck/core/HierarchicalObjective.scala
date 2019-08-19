package yuck.core

/**
 * Implements a lexicographic ordering on the given objectives.
 *
 * @author Michael Marte
 */
final class HierarchicalObjective(
    val objectives: List[AnyObjective],
    firstSolutionIsGoodEnough: Boolean)
    extends AnyObjective
{
    require(! objectives.isEmpty)
    override val targetCosts =
        new PolymorphicListValue(objectives.map(_.targetCosts))
    override def toString =
        objectives.toString
    override def costs(searchState: SearchState) =
        new PolymorphicListValue(objectives.map(_.costs(searchState)))
    override def isSolution(costs: Costs) =
        objectives.head.isSolution(costs.asInstanceOf[PolymorphicListValue].value.head)
    override def isGoodEnough(costs: Costs) =
        isSolution(costs) && (firstSolutionIsGoodEnough || ! isHigherThan(costs, targetCosts))
    override def compareCosts(lhs: Costs, rhs: Costs) =
        compareCosts(
            objectives,
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
        assessMove(objectives, before, after)
    private def assessMove(objectives: List[AnyObjective], before: SearchState, after: SearchState): Double =
        objectives match {
            case Nil => 0
            case (h :: t) => {
                val delta = h.assessMove(before, after)
                if (delta == 0) assessMove(t, before, after) else delta
            }
        }
    override def tighten(space: Space, rootObjective: AnyObjective) =
        tighten(space, objectives, rootObjective)
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
