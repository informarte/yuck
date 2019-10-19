package yuck.core

/**
 * Helper for implementing variables.
 *
 * @author Michael Marte
 */
trait DomainHolder[Value <: AnyValue, Domain <: yuck.core.Domain[Value]] {

    protected var currentDomain: Domain
    protected def thisVariable: Variable[Value]
    protected def intersectCurrentDomainWith(domain: Domain): Domain

    final def pruneDomainImpl(restriction: Domain): Boolean = {
        if (restriction != currentDomain) {
            // We try to avoid useless and expensive intersections.
            if (currentDomain.isSubsetOf(restriction)) {
                false
            } else {
                if (restriction.isSubsetOf(currentDomain)) {
                    currentDomain = restriction
                } else {
                    currentDomain = intersectCurrentDomainWith(restriction)
                }
                if (currentDomain.isEmpty) {
                    throw new DomainWipeOutException(thisVariable)
                }
                true
            }
        } else {
            false
        }
    }

    final def relaxDomainImpl(relaxation: Domain): Boolean = {
        if (relaxation != currentDomain) {
            require(
                currentDomain.isSubsetOf(relaxation),
                "%s is not a superset of %s".format(relaxation, currentDomain))
            require(currentDomain.isSubsetOf(relaxation))
            currentDomain = relaxation
            true
        } else {
            false
        }
    }

}

