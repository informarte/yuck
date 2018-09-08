package yuck.core

import scala.collection._

/**
 * Implements mutable search states.
 *
 * @author Michael Marte
 */
final class Assignment(valDir: mutable.AnyRefMap[AnyVariable, AnyValue]) extends SearchState {

    def this() = this(new mutable.AnyRefMap[AnyVariable, AnyValue])
    def this(searchState: SearchState) = {
        this()
        setValues(searchState)
    }

    override def clone = new Assignment(valDir.clone)
    override def mappedVariables = valDir.toList.map(_._1).toSet
    override def hasValue(x: AnyVariable) = valDir.contains(x)
    override def anyValue(x: AnyVariable) = valDir(x)
    override def maybeAnyValue(x: AnyVariable) = valDir.get(x)

    /** Assigns the given value to the given variable. */
    def setValue[Value <: AnyValue](x: Variable[Value], a: Value): Assignment = {
        x.checkAssignment(a)
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
