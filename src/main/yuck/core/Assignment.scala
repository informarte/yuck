package yuck.core

import scala.collection._

/**
 * Implements mutable search states.
 *
 * @author Michael Marte
 */
final class Assignment extends SearchState {

    private val valDir = new mutable.AnyRefMap[AnyVariable, AnyValue]

    override def clone = valDir./:(new Assignment){case (s, (x, a)) => s.setValue(x, a)}
    override def mappedVariables = valDir.toList.map(_._1).toSet
    override def hasValue(x: AnyVariable) = valDir.contains(x)
    override def anyValue(x: AnyVariable) = valDir(x)
    override def maybeAnyValue(x: AnyVariable) = valDir.get(x)

    /** Assigns the given value to the given variable. */
    def setValue[Value <: AnyValue](x: Variable[Value], a: Value): Assignment = {
        require(
            x.domain.contains(a),
            "Domain %s of variable %s does not contain value %s".format(x.domain, x, a))
        valDir += x -> a
        this
    }

    /** Copies the value assignments from the given search state. */
    def setValues(searchState: SearchState): Assignment = {
        for (x <- searchState.mappedVariables) {
            setValue(x, searchState.anyValue(x))
        }
        this
    }

    private def setValue(x: AnyVariable, a: AnyValue): Assignment = {
        require(x.valueType == a.valueType)
        valDir += x -> a
        this
    }

}
