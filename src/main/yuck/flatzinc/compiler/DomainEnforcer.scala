package yuck.flatzinc.compiler

import yuck.constraints._
import yuck.core._

/**
 * Enforces domains of FlatZinc variables.
 *
 * Domains of search variables are enforced by reducing their domains.
 *
 * Domains of channel variables are enforced by posting appropriate constraints.
 *
 * @author Michael Marte
 */
final class DomainEnforcer
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    override def run {
        enforceDomains
    }

    private def enforceDomains {
        for ((key, x) <- cc.vars) {
            val dx = cc.domains(key)
            val tx = dx.valueType
            if (tx == BooleanValue.Traits.valueType) {
                enforceBooleanDomain(IntegerValue.Traits.staticCast(x), dx.asInstanceOf[BooleanDomain])
            } else if (tx == IntegerValue.Traits.valueType) {
                enforceIntegerDomain(IntegerValue.Traits.staticCast(x), dx.asInstanceOf[IntegerDomain])
            } else if (tx == IntegerSetValue.Traits.valueType) {
                enforceIntegerSetDomain(IntegerSetValue.Traits.staticCast(x), dx.asInstanceOf[IntegerPowersetDomain])
            }
        }
    }

    private def enforceBooleanDomain(x: Variable[IntegerValue], dx: BooleanDomain) {
        if (dx.isSingleton) {
            val a = if (dx.singleValue == True) Zero else One
            if (cc.space.isChannelVariable(x)) {
                if (a == Zero) {
                    // true
                    cc.costVars += x
                } else {
                    // false
                    val costs = createNonNegativeChannel[IntegerValue]
                    cc.space.post(new NumLe(nextConstraintId, null, One, x, costs))
                    cc.costVars += costs
                }
            } else {
                x.pruneDomain(if (a == Zero) ZeroIntegerDomain else OneIntegerDomain)
                cc.space.setValue(x, a)
            }
        } else if (! cc.space.isChannelVariable(x)) {
            x.pruneDomain(ZeroOneIntegerDomain)
        }
    }

    private def enforceIntegerDomain(x: Variable[IntegerValue], dx: IntegerDomain) {
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                if (! cc.space.definingConstraint(x).get.isInstanceOf[Bool2Int1] || dx.isSingleton) {
                    val costs = createNonNegativeChannel[IntegerValue]
                    cc.space.post(new SetIn(nextConstraintId, null, x, dx, costs))
                    cc.costVars += costs
                }
            } else {
                x.pruneDomain(dx)
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

    private def enforceIntegerSetDomain(x: Variable[IntegerSetValue], dx: IntegerPowersetDomain) {
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                val costs = createNonNegativeChannel[IntegerValue]
                cc.space.post(new SetSubset(nextConstraintId, null, x, dx.base, costs))
                cc.costVars += costs
            } else {
                x.pruneDomain(dx)
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

}
