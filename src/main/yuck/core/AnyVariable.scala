package yuck.core

/**
 * Provides an interface for working with variables of unknown type.
 *
 * @author Michael Marte
 */
abstract class AnyVariable
    (val id: Id[AnyVariable], val name: String)
    extends Ordered[AnyVariable]
{

    @inline final override def hashCode = id.hashCode
    override def toString = if (name.isEmpty) "_YUCK_%d".format(id.rawId) else name
    @inline final override def compare(that: AnyVariable) = this.id.compare(that.id)

    /** Returns the variable's domain. */
    def domain: AnyDomain

    /** Returns the type of the elements of the variable's domain. */
    @inline final def valueType: Class[_] = domain.valueType

    /** Returns true iff the variable's domain is singleton. */
    @inline final def isParameter: Boolean = domain.isSingleton

    /** Returns true iff the variable's domain is not singleton. */
    @inline final def isVariable: Boolean = ! isParameter

    /**
     * Assigns the variable a random value from its domain.
     *
     * Throws when the domain is empty or infinite.
     */
    def assignRandomValue(space: Space, randomGenerator: RandomGenerator): Space

    /**
     * Returns an effect for assigning the variable a random value that differs from
     * its current value.
     *
     * Throws when the domain is infinite or has less than two elements.
     *
     * For the sake of efficiency, implementations should use and reuse an instance of
     * [[yuck.core.ReusableEffectWithFixedVariable ReusableEffectWithFixedVariable]].
     */
    def nextRandomEffect(space: Space, randomGenerator: RandomGenerator): AnyEffect

    /**
     * Creates a random move that changes the value of the variable.
     *
     * Throws when the domain is infinite or has less than two elements.
     */
    def nextMove(space: Space, randomGenerator: RandomGenerator): Move

}
