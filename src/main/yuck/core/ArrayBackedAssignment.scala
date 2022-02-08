package yuck.core

import scala.collection._

/**
 * Implements mutable search states.
 *
 * @author Michael Marte
 */
final class ArrayBackedAssignment private
    (variables: mutable.ArrayBuffer[AnyVariable], values: mutable.ArrayBuffer[AnyValue])
    extends SearchState
{

    def this() = this(new mutable.ArrayBuffer[AnyVariable], new mutable.ArrayBuffer[AnyValue])
    def this(searchState: SearchState) = {
        this()
        setValues(searchState)
    }

    override def clone = new ArrayBackedAssignment(variables.clone, values.clone)

    override def mappedVariables = variables.iterator.filter(_.ne(null)).toSet
    override def hasValue(x: AnyVariable) = x.id.rawId < values.size && values(x.id.rawId).ne(null)
    override def value(x: AnyVariable) = {
        val a = values(x.id.rawId)
        if (a.eq(null)) {
            throw new RuntimeException("%s has no value assigned".format(x))
        }
        a
    }
    inline override def maybeValue(x: AnyVariable) = Option(values(x.id.rawId))

    /** Assigns the given value to the given variable. */
    inline def setValue[V <: AnyValue](x: Variable[V], a: V): ArrayBackedAssignment = {
        setValueUnsafe(x, a)
        this
    }

    /** Copies the value ArrayBackedAssignments from the given search state. */
    def setValues(searchState: SearchState): ArrayBackedAssignment = {
        for (x <- searchState.mappedVariables) {
            val a = searchState.value(x)
            require(x.valueType == a.valueType)
            setValueUnsafe(x, a)
        }
        this
    }

    private def setValueUnsafe(x: AnyVariable, a: AnyValue): Unit = {
        val i = x.id.rawId
        if (variables.size < i + 1) {
            variables.sizeHint(i + 1)
            values.sizeHint(i + 1)
            while (variables.size < i + 1) {
                variables += null
                values += null
            }
        }
        variables.update(i, x)
        values.update(i, a)
    }

}
