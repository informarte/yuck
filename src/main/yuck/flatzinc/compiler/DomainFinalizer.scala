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

    override def run = {
        finalizeDomains
    }

    private def finalizeDomains: Unit = {
        val done = new mutable.HashSet[AnyVariable]
        for ((key, x) <- cc.vars if ! done.contains(x)) {
            x.domain match {
                case dx: BooleanDomain =>
                    finalizeBooleanDomain(BooleanValueTraits.safeDowncast(x), dx)
                case dx: IntegerDomain =>
                    finalizeIntegerDomain(IntegerValueTraits.safeDowncast(x), dx)
                case dx: IntegerPowersetDomain =>
                    finalizeIntegerSetDomain(IntegerSetValueTraits.safeDowncast(x), dx)
            }
            cc.logger.logg("Domain of %s is %s".format(x, x.domain))
            done += x
        }
    }

    private def finalizeBooleanDomain(x: BooleanVariable, dx: BooleanDomain): Unit = {
        if (dx.isSingleton) {
            if (cc.space.isChannelVariable(x)) {
                if (dx.singleValue.truthValue) {
                    cc.costVars += x
                } else {
                    val costs = createBoolChannel
                    cc.space.post(new Not(nextConstraintId, null, x, costs))
                    cc.costVars += costs
                }
            } else {
                cc.space.setValue(x, dx.singleValue)
            }
        }
    }

    private def finalizeIntegerDomain(x: IntegerVariable, dx: IntegerDomain): Unit = {
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                if (! cc.space.definingConstraint(x).get.isInstanceOf[Bool2Int1] || dx.isSingleton) {
                    val costs = createBoolChannel
                    cc.space.post(new Contains(nextConstraintId, null, x, dx, costs))
                    cc.costVars += costs
                }
            } else {
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

    private def finalizeIntegerSetDomain(x: IntegerSetVariable, dx: IntegerPowersetDomain): Unit = {
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                val costs = createBoolChannel
                cc.space.post(new Subset(nextConstraintId, null, x, dx.base, costs))
                cc.costVars += costs
            } else {
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

}
