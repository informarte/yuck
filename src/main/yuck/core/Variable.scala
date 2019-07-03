package yuck.core

/**
 * Provides an interface for working with typed variables.
 *
 * @author Michael Marte
 */
abstract class Variable
    [Value <: AnyValue]
    (id: Id[AnyVariable], name: String)
    extends AnyVariable(id, name)
{

    /**
     * Intersects the variable's domain with the given domain.
     *
     * Returns true iff the variable's domain was actually pruned.
     *
     * Throws a [[yuck.core.DomainWipeOutException DomainWipeOutException]]
     * when the variable's domain became empty.
     */
    def pruneDomain(restriction: Domain[Value]): Boolean

    /**
     * Replaces the variable's domain with the given domain.
     *
     * Returns true iff the variable's domain was actually relaxed.
     *
     * Throws when the new domain is not a superset of the current domain.
     */
    def relaxDomain(relaxation: Domain[Value]): Boolean

    override def domain: Domain[Value]

    final override def createDomainRestorer = new Function0[Unit] {
        private val backup = domain
        override def apply {
            relaxDomain(backup)
        }
    }

    override def nextMove(space: Space, randomGenerator: RandomGenerator) =
        new ChangeValue(
            space.nextMoveId,
            this,
            domain.nextRandomValue(randomGenerator, space.searchState.value(this)))

    override def assignRandomValue(space: Space, randomGenerator: RandomGenerator) = {
        space.setValue(this, if (domain.isSingleton) domain.singleValue else domain.randomValue(randomGenerator))
    }

    private val reuseableEffect = new ReusableMoveEffectWithFixedVariable[Value](this)

    override def nextRandomMoveEffect(space: Space, randomGenerator: RandomGenerator) = {
        reuseableEffect.setNextRandomValue(space.searchState, randomGenerator)
        reuseableEffect
    }

}
