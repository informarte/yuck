package yuck.flatzinc.compiler

import scala.collection._

import yuck.constraints._
import yuck.core._

/**
 * Relaxes the domains of FlatZinc variables that were turned into channel variables
 * (such that they can hold all intermediate values) and enforces their domains by
 * adding appropriate constraints.
 *
 * @author Michael Marte
 */
final class DomainFinalizer
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    override def run {
        finalizeDomains
    }

    private def finalizeDomains {
        val done = new mutable.HashSet[AnyVariable]
        for ((key, x) <- cc.vars if ! done.contains(x)) {
            val dx = cc.domains(key)
            val tx = dx.valueType
            if (tx == BooleanValueTraits.valueType) {
                finalizeBooleanDomain(IntegerValueTraits.unsafeDowncast(x), dx.asInstanceOf[BooleanDomain])
            } else if (tx == IntegerValueTraits.valueType) {
                finalizeIntegerDomain(IntegerValueTraits.unsafeDowncast(x), dx.asInstanceOf[IntegerDomain])
            } else if (tx == IntegerSetValueTraits.valueType) {
                finalizeIntegerSetDomain(IntegerSetValueTraits.unsafeDowncast(x), dx.asInstanceOf[IntegerPowersetDomain])
            }
            cc.logger.logg("Domain of %s is %s".format(x, x.domain))
            done += x
        }
    }

    private def finalizeBooleanDomain(x: Variable[IntegerValue], dx: BooleanDomain) {
        if (dx.isSingleton) {
            if (cc.space.isChannelVariable(x)) {
                x.turnIntoChannel(IntegerValueTraits.nonNegativeDomain)
                if (dx.singleValue == True) {
                    cc.costVars += x
                } else {
                    val costs = createNonNegativeChannel[IntegerValue]
                    cc.space.post(new NumLe(nextConstraintId, null, One, x, costs))
                    cc.costVars += costs
                }
            } else {
                cc.space.setValue(x, if (dx.singleValue == True) Zero else One)
            }
        } else if (cc.space.isChannelVariable(x)) {
            x.turnIntoChannel(IntegerValueTraits.nonNegativeDomain)
        }
    }

    private def finalizeIntegerDomain(x: Variable[IntegerValue], dx: IntegerDomain) {
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                x.turnIntoChannel(IntegerValueTraits.completeDomain)
                if (! cc.space.definingConstraint(x).get.isInstanceOf[Bool2Int1] || dx.isSingleton) {
                    val costs = createNonNegativeChannel[IntegerValue]
                    cc.space.post(new SetIn(nextConstraintId, null, x, dx, costs))
                    cc.costVars += costs
                }
            } else {
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

    private def finalizeIntegerSetDomain(x: Variable[IntegerSetValue], dx: IntegerPowersetDomain) {
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                x.turnIntoChannel(IntegerSetValueTraits.completeDomain)
                val costs = createNonNegativeChannel[IntegerValue]
                cc.space.post(new SetSubset(nextConstraintId, null, x, dx.base, costs))
                cc.costVars += costs
            } else {
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

}
