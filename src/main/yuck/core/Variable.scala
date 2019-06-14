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
        override def apply = {
            relaxDomain(backup)
        }
    }

    final override def hasValidValue(space: Space) =
        domain.contains(space.searchState.value(this))

    private val reuseableEffect = new ReusableMoveEffectWithFixedVariable[Value](this)

    final override def randomMoveEffect(randomGenerator: RandomGenerator) = {
        reuseableEffect.setValue(if (domain.isSingleton) domain.singleValue else domain.randomValue(randomGenerator))
        reuseableEffect
    }

    final override def nextRandomMoveEffect(space: Space, randomGenerator: RandomGenerator) = {
        reuseableEffect.setValue(domain.nextRandomValue(randomGenerator, space.searchState.value(this)))
        reuseableEffect
    }

    final override def nextMove(space: Space, randomGenerator: RandomGenerator) =
        new ChangeValues(space.nextMoveId, Some(nextRandomMoveEffect(space, randomGenerator)))

}
