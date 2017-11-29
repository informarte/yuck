package yuck.core

/**
 * Simulates the application of the given move to the given search state.
 *
 * @author Michael Marte
 */
final class MoveSimulator(before: SearchState, move: Move) extends SearchState {
    override def mappedVariables =
        before.mappedVariables ++ move.involvedVariables
    override def hasValue(x: AnyVariable) =
        before.hasValue(x) || move.involves(x)
    override def anyValue(x: AnyVariable) =
        move.maybeAnyValue(x).getOrElse(before.anyValue(x))
    override def maybeAnyValue(x: AnyVariable) = {
        val maybeA = move.maybeAnyValue(x)
        if (maybeA.isDefined) maybeA else before.maybeAnyValue(x)
    }
}
