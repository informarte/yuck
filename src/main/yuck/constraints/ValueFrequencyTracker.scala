package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Base class for constraints that need to keep track of which values are
 * assigned to a set of variables and how often each value is assigned.
 *
 * The given (immutable) map serves as a factory for the value registry.
 *
 * @author Michael Marte
 */
abstract class ValueFrequencyTracker
    [Value <: AnyValue,
     Result <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     xs: immutable.Seq[Variable[Value]], result: Variable[Result],
     val variableRegistryFactory: immutable.Map[AnyVariable, Int],
     val valueRegistryFactory: immutable.Map[Value, Int])
    (implicit valueTraits: ValueTraits[Value])
    extends Constraint(id, goal)
{

    override def inVariables: IterableOnce[AnyVariable] = xs
    override def outVariables = List(result)

    type VariableRegistry = immutable.Map[AnyVariable, Int]
    private def registerVariable(registry: VariableRegistry, x: AnyVariable) =
        registry + (x -> (registry.getOrElse(x, 0) + 1))
    private val variableRegistry =
        xs.foldLeft(variableRegistryFactory.empty)(registerVariable)

    type ValueRegistry = immutable.Map[Value, Int]
    private var valueRegistry: ValueRegistry = null
    private var futureValueRegistry: ValueRegistry = null
    private def registerValue(valueRegistry: ValueRegistry, a: Value, n: Int): ValueRegistry =
        valueRegistry + (a -> safeAdd(valueRegistry.getOrElse(a, 0), n))
    private def deregisterValue(valueRegistry: ValueRegistry, a: Value, n: Int): ValueRegistry = {
        val occurenceCount = valueRegistry(a) - n
        if (occurenceCount == 0) valueRegistry - a else valueRegistry + (a -> occurenceCount)
    }
    private val effects = List(new ReusableMoveEffectWithFixedVariable[Result](result))
    private val effect = effects.head

    override def initialize(now: SearchState) = {
        valueRegistry = valueRegistryFactory.empty.asInstanceOf[ValueRegistry]
        for (x <- xs) {
            valueRegistry = registerValue(valueRegistry, now.value(x), 1)
        }
        effect.a = computeResult(now, valueRegistry)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureValueRegistry = valueRegistry
        val it = todo(move).iterator
        while (it.hasNext) {
            val x0 = it.next
            val x = valueTraits.safeDowncast(x0)
            val n = variableRegistry(x0)
            futureValueRegistry =
                registerValue(deregisterValue(futureValueRegistry, before.value(x), n), after.value(x), n)
        }
        effect.a = computeResult(after, futureValueRegistry)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        valueRegistry = futureValueRegistry
        effects
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
    protected def todo(move: Move): IterableOnce[AnyVariable] =
        move.involvedVariables

}
