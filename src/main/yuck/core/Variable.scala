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

    override def nextMove(space: Space, randomGenerator: RandomGenerator) =
        new ChangeValue(
            space.nextMoveId,
            this,
            domain.nextRandomValue(randomGenerator, space.searchState.value(this)))

    override def assignRandomValue(space: Space, randomGenerator: RandomGenerator) = {
        space.setValue(this, if (domain.isSingleton) domain.singleValue else domain.randomValue(randomGenerator))
    }

    private val reuseableEffect = new ReusableEffectWithFixedVariable[Value](this)

    override def nextRandomEffect(space: Space, randomGenerator: RandomGenerator) = {
        reuseableEffect.setNextRandomValue(space.searchState, randomGenerator)
        reuseableEffect
    }

}

/**
 * @author Michael Marte
 *
 */
final object Variable {

    /**
     * Prunes the domains of the given variables.
     *
     * Returns true iff at least one of the domains was pruned.
     */
    def pruneDomains
        [Value1 <: AnyValue, Value2 <: AnyValue]
        (x: Variable[Value1], dx: Domain[Value1], y: Variable[Value2], dy: Domain[Value2]):
        Boolean =
    {
        x.pruneDomain(dx) ||| y.pruneDomain(dy)
    }

    /**
     * Prunes the domains of the given variables.
     *
     * Returns true iff at least one of the domains was pruned.
     */
    def pruneDomains
        [Value1 <: AnyValue, Value2 <: AnyValue, Value3 <: AnyValue]
        (x: Variable[Value1], dx: Domain[Value1], y: Variable[Value2], dy: Domain[Value2], z: Variable[Value3], dz: Domain[Value3]):
        Boolean =
    {
        x.pruneDomain(dx) ||| y.pruneDomain(dy) ||| z.pruneDomain(dz)
    }

    /**
     * Prunes the domains of the given variables.
     *
     * Returns true iff at least one of the domains was pruned.
     */
    def pruneDomains
        [Value <: AnyValue]
        (xds: TraversableOnce[(Variable[Value], Domain[Value])]):
        Boolean =
    {
        xds.foldLeft(false){case (z, (x, dx)) => z ||| x.pruneDomain(dx)}
    }

}