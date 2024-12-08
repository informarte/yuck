package yuck.core

/**
 * A total ordering on variables.
 *
 * @author Michael Marte
 */
object AnyVariableOrdering extends Ordering[AnyVariable] {
    inline override def compare(x: AnyVariable, y: AnyVariable) =
        java.lang.Integer.compare(x.id.rawId, y.id.rawId)
}
