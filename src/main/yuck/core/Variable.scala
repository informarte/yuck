package yuck.core

/**
 * Provides an interface for working with variables of known type.
 */
abstract class Variable
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[AnyVariable], name: String)
    extends AnyVariable(id, name)
{

    override def domain: D

    protected def setDomain(domain: D): Unit

    /**
     * Intersects the variable's domain with the given domain.
     *
     * Returns true iff the variable's domain was actually pruned.
     *
     * Throws a [[yuck.core.DomainWipeOutException DomainWipeOutException]]
     * when the variable's domain became empty.
     */
    final def pruneDomain(restriction: D): Boolean = {
        if restriction != domain then {
            // We try to avoid useless and expensive intersections.
            if domain.isSubsetOf(restriction) then {
                false
            } else {
                if restriction.isSubsetOf(domain) then {
                    setDomain(restriction)
                } else {
                    setDomain(domain.intersect(restriction))
                }
                if domain.isEmpty then {
                    throw new DomainWipeOutException(this)
                }
                true
            }
        } else {
            false
        }
    }

    /**
     * Replaces the variable's domain with the given domain.
     *
     * Returns true iff the variable's domain was actually relaxed.
     *
     * Throws when the new domain is not a superset of the current domain.
     */
    final def relaxDomain(relaxation: D): Boolean = {
        if relaxation != domain then {
            require(
                domain.isSubsetOf(relaxation),
                "%s is not a superset of %s".format(relaxation, domain))
            require(domain.isSubsetOf(relaxation))
            setDomain(relaxation)
            true
        } else {
            false
        }
    }

    final override def createDomainRestorer = new (() => Unit) {
        private val backup = domain
        override def apply() = {
            relaxDomain(backup)
        }
    }

    final override def hasValidValue(searchState: SearchState) =
        domain.contains(searchState.value(this.asInstanceOf[X]))

    final override def randomMoveEffect(randomGenerator: RandomGenerator) =
        new ImmutableMoveEffect(
            this.asInstanceOf[X],
            if domain.isSingleton then domain.singleValue else domain.randomValue(randomGenerator))

    final override def nextRandomMoveEffect(space: Space, randomGenerator: RandomGenerator): MoveEffect[A, D, X] =
        new ImmutableMoveEffect(
            this.asInstanceOf[X],
            domain.nextRandomValue(randomGenerator, space.searchState.value(this.asInstanceOf[X])))

    final override def nextMove(space: Space, randomGenerator: RandomGenerator) =
        new ChangeValues(space.nextMoveId(), Some(nextRandomMoveEffect(space, randomGenerator)))

}
