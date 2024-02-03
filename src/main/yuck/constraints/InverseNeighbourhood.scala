package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
abstract class InverseNeighbourhood
    (protected val space: Space,
     protected val f: InverseFunction,
     protected val g: InverseFunction)
    extends Neighbourhood
{

    protected final def value(x: IntegerVariable): IntegerValue = space.searchState.value(x)
    protected final def rawValue(x: IntegerVariable): Int = value(x).toInt

    require(f.xs.size == g.xs.size)
    require(f.isSuitableForImplicitSolving(space))
    require(g.isSuitableForImplicitSolving(space))
    require(f.xs.forall(x => x.domain.isSubsetOf(g.indexDomain)))
    require(g.xs.forall(x => x.domain.isSubsetOf(f.indexDomain)))
    require(f.xs.forall(_.hasValidValue(space.searchState)))
    require(g.xs.forall(_.hasValidValue(space.searchState)))
    require(f.indexRange.forall(i => rawValue(g.xs(rawValue(f.xs(i - f.offset)) - g.offset)) == i))

    final override def searchVariables = (f.xs.iterator ++ g.xs.iterator).filterNot(_.domain.isSingleton).toSet

    final override def children = Nil

}
