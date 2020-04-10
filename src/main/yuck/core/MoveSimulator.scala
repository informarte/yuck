package yuck.core

/**
 * Simulates the application of the given move to the given search state.
 *
 * @author Michael Marte
 */
final class MoveSimulator(before: SearchState, move: Move) extends SearchState {
    override def clone =
        new Assignment(this)
    override def mappedVariables =
        before.mappedVariables ++ move.involvedVariablesIterator
    override def hasValue(x: AnyVariable) =
        before.hasValue(x) || move.involves(x)
    override def value(x: AnyVariable) =
        move.maybeValue(x).getOrElse(before.value(x))
    override def maybeValue(x: AnyVariable) = {
        val maybeA = move.maybeValue(x)
        if (maybeA.isDefined) maybeA else before.maybeValue(x)
    }
}
