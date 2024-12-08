package yuck.core

/**
 * Simulates the application of the given move to the given search state.
 *
 * @author Michael Marte
 */
final class MoveSimulator(val before: SearchState, val move: Move) extends SearchState {
    override def clone =
        new HashMapBackedAssignment(this)
    override def mappedVariables =
        before.mappedVariables ++ move.involvedVariablesIterator
    override def hasValue(x: AnyVariable) =
        before.hasValue(x) || move.involves(x)
    override def value(x: AnyVariable) = {
        val a = move.valueOrNull(x)
        if a == null then before.value(x) else a
    }
    override def maybeValue(x: AnyVariable) = {
        val maybeA = move.maybeValue(x)
        if (maybeA.isDefined) maybeA else before.maybeValue(x)
    }
}
