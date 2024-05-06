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

    final override def hashCode = id.rawId

    override def toString = if (name.isEmpty) "_YUCK_%d".format(id.rawId) else name

    inline final override def compare(that: AnyVariable) = this.id.rawId - that.id.rawId
    inline final def ==(that: AnyVariable): Boolean = this.id.rawId == that.id.rawId
    inline final def !=(that: AnyVariable): Boolean = this.id.rawId != that.id.rawId

    /**
     * Returns the variable's domain.
     *
     * Notice that a search variable must take its value from its domain;
     * in contrast a channel variable may take any value.
     */
    def domain: AnyDomain

    /** Returns a function that will restore the domain of the variable. */
    def createDomainRestorer: () => Unit

    /** Returns the type of the elements of the variable's domain. */
    inline final def valueType: Class[?] = domain.valueType

    /**
      * Returns true iff the value assigned to the variable is contained in the variable's domain.
      *
      * Throws when the variable has no value assigned.
      */
    def hasValidValue(searchState: SearchState): Boolean

    /**
      * Returns an effect for assigning the variable a random value.
      *
      * If the variable's domain has at least two elements, the proposed value is
      * guaranteed to differ from the variable's current value.
      *
      * Throws when the domain is empty or infinite.
      *
      * For the sake of efficiency, implementations should use and reuse an instance of
      * [[yuck.core.ReusableMoveEffectWithFixedVariable ReusableMoveEffectWithFixedVariable]].
      */
    def randomMoveEffect(randomGenerator: RandomGenerator): AnyMoveEffect

    /**
     * Returns an effect for assigning the variable a random value.
     *
     * If the variable's domain has at least two elements, the proposed value is
     * guaranteed to differ from the variable's current value.
     *
     * Throws when the domain is empty or infinite.
     *
     * For the sake of efficiency, implementations should use and reuse an instance of
     * [[yuck.core.ReusableMoveEffectWithFixedVariable ReusableMoveEffectWithFixedVariable]].
     */
    def nextRandomMoveEffect(space: Space, randomGenerator: RandomGenerator): AnyMoveEffect

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
