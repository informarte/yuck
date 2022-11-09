package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Base class for constraints that need to keep track of which values are
 * assigned to a set of variables and how often each value is assigned.
 *
 * The given (immutable) map serves as a factory for the value registry.
 *
 * @author Michael Marte
 */
abstract class ValueFrequencyTracker
    [V <: AnyValue, Result <: AnyValue]
    (id: Id[Constraint],
     protected val xs: immutable.Seq[Variable[V]], protected val y: Variable[Result])
    (using protected val valueTraits: ValueTraits[V])
    extends Constraint(id)
{

    override def inVariables: Iterable[AnyVariable] = xs
    override def outVariables = List(y)

    type VariableRegistry = immutable.Map[AnyVariable, Int]
    protected def createVariableRegistry(): VariableRegistry = immutable.TreeMap[AnyVariable, Int]()
    private def registerVariable(registry: VariableRegistry, x: AnyVariable) =
        registry + (x -> (registry.getOrElse(x, 0) + 1))
    private val variableRegistry =
        xs.foldLeft(createVariableRegistry())(registerVariable)

    type ValueRegistry = immutable.Map[V, Int]
    protected def createValueRegistry(): ValueRegistry = immutable.HashMap[V, Int]()
    private var valueRegistry: ValueRegistry = null
    private var futureValueRegistry: ValueRegistry = null
    inline private def registerValue(valueRegistry: ValueRegistry, a0: V, n: Int): ValueRegistry = {
        val a = valueTraits.normalizedValue(a0)
        valueRegistry + (a -> safeAdd(valueRegistry.getOrElse(a, 0), n))
    }
    inline private def deregisterValue(valueRegistry: ValueRegistry, a0: V, n: Int): ValueRegistry = {
        val a = valueTraits.normalizedValue(a0)
        val occurenceCount = valueRegistry(a) - n
        if (occurenceCount == 0) valueRegistry - a else valueRegistry + (a -> occurenceCount)
    }

    private val effect = y.reuseableEffect

    override def initialize(now: SearchState) = {
        valueRegistry = createValueRegistry()
        for (x <- xs) {
            valueRegistry = registerValue(valueRegistry, now.value(x), 1)
        }
        effect.a = computeResult(now, valueRegistry)
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureValueRegistry = valueRegistry
        val it = todo(move).iterator
        while (it.hasNext) {
            val x0 = it.next()
            val x = valueTraits.safeDowncast(x0)
            val n = variableRegistry(x0)
            futureValueRegistry =
                registerValue(deregisterValue(futureValueRegistry, before.value(x), n), after.value(x), n)
        }
        effect.a = computeResult(after, futureValueRegistry)
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        valueRegistry = futureValueRegistry
        effect
    }

    /**
     * Subclasses are required to define this method for computing the result from
     * the given value registry.
     */
    protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry): Result

    /**
     * Returns the input variables to be considered by consult and commit for processing
     * the given move.
     *
     * The default implementation returns all variables involved in the move but this hook
     * can be used to limit processing to a subset thereof.
     */
    protected def todo(move: Move): Iterator[AnyVariable] =
        move.involvedVariablesIterator

}
