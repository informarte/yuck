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

    /**
     * Assigns the variable a random value from its domain.
     *
     * Throws when the domain is empty or infinite.
     */
    def assignRandomValue(space: Space, randomGenerator: RandomGenerator): Space

    /**
     * Returns an effect for assigning the variable a random value.
     *
     * If the variable's domain has at least two elements, the proposed value is
     * guaranteed to differ from the variable's current value.
     *
     * Throws when the domain is empty or infinite.
     *
     * For the sake of efficiency, implementations should use and reuse an instance of
     * [[yuck.core.ReusableEffectWithFixedVariable ReusableEffectWithFixedVariable]].
     */
    def nextRandomEffect(space: Space, randomGenerator: RandomGenerator): AnyEffect

    /**
     * Creates a random move that involves only the variable.
     *
     * If the variable's domain has at least two elements, the proposed move is
     * guaranteed to change the variable's assignment.
     *
     * Throws when the domain is empty or infinite.
     */
    def nextMove(space: Space, randomGenerator: RandomGenerator): Move

}
