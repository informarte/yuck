package yuck.core

import scala.collection._

/**
 * Implements mutable search states.
 *
 * @author Michael Marte
 */
final class HashMapBackedAssignment(valDir: mutable.AnyRefMap[AnyVariable, AnyValue]) extends SearchState {

    def this() = this(new mutable.AnyRefMap[AnyVariable, AnyValue])
    def this(searchState: SearchState) = {
        this()
        setValues(searchState)
    }

    override def clone = new HashMapBackedAssignment(valDir.clone)
    override def mappedVariables = valDir.keysIterator.toSet
    override def hasValue(x: AnyVariable) = valDir.contains(x)
    override def value(x: AnyVariable) = valDir(x)
    override def maybeValue(x: AnyVariable) = valDir.get(x)

    /** Assigns the given value to the given variable. */
    def setValue[Value <: AnyValue](x: Variable[Value], a: Value): HashMapBackedAssignment = {
        valDir += x -> a
        this
    }

    /** Copies the value assignments from the given search state. */
    def setValues(searchState: SearchState): HashMapBackedAssignment = {
        for (x <- searchState.mappedVariables) {
            setValue(x, searchState.value(x))
        }
        this
    }

    private def setValue(x: AnyVariable, a: AnyValue): HashMapBackedAssignment = {
        require(x.valueType == a.valueType)
        valDir += x -> a
        this
    }

}
