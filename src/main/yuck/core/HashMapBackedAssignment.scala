package yuck.core

import scala.collection.*

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

    inline override def mappedVariables = valDir.keysIterator.toSet
    inline override def hasValue(x: AnyVariable) = valDir.contains(x)
    inline override def value(x: AnyVariable) = valDir(x)
    inline override def maybeValue(x: AnyVariable) = valDir.get(x)

    /** Assigns the given value to the given variable. */
    inline def setValue[V <: Value[V]](x: Variable[V], a: V): HashMapBackedAssignment = {
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
