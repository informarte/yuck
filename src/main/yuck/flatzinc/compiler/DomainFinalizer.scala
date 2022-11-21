package yuck.flatzinc.compiler

import scala.collection.*

import yuck.constraints.*
import yuck.core.{given, *}

/**
 * Enforces the domains of channel variables by adding appropriate constraints.
 *
 * @author Michael Marte
 */
final class DomainFinalizer
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    private val intChannels = new mutable.HashSet[IntegerVariable]

    override def run() = {
        finalizeDomains()
    }

    private def finalizeDomains(): Unit = {
        val done = new mutable.HashSet[AnyVariable]
        for (x <- cc.vars.values if ! done.contains(x)) {
            x match {
                case x: BooleanVariable => finalizeBooleanDomain(x)
                case x: IntegerVariable => finalizeIntegerDomain(x)
                case x: IntegerSetVariable => finalizeIntegerSetDomain(x)
            }
            cc.logger.logg("Domain of %s is %s".format(x, x.domain))
            done += x
        }
        if (intChannels.size > 32) {
            // When there are many integer channels, we enforce their domains using a single InDomain constraint.
            // This way we speed up neighbourhood generation and reduce the overhead of goal tracking.
            val costs = createBoolChannel()
            cc.space.post(new InDomain(nextConstraintId(), null, intChannels.to(immutable.ArraySeq), costs))
            cc.costVars += costs
        } else {
            // InDomain comes with more overhead than Contains and, when there are only a few integer channels,
            // a set of Contains constraints is faster than a single InDomain constraint.
            for (x <- intChannels) {
                val costs = createBoolChannel()
                cc.space.post(new Contains(nextConstraintId(), null, x, x.domain, costs))
                cc.costVars += costs
            }
        }
    }

    private def finalizeBooleanDomain(x: BooleanVariable): Unit = {
        val dx = x.domain
        if (dx.isSingleton) {
            if (cc.space.isChannelVariable(x)) {
                if (dx.singleValue.truthValue) {
                    cc.costVars += x
                } else {
                    val costs = createBoolChannel()
                    cc.space.post(new Not(nextConstraintId(), null, x, costs))
                    cc.costVars += costs
                }
            } else {
                cc.space.setValue(x, dx.singleValue)
            }
        }
    }

    private def finalizeIntegerDomain(x: IntegerVariable): Unit = {
        val dx = x.domain
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                val definingConstraint = cc.space.definingConstraint(x)
                if (! definingConstraint.isInstanceOf[Delivery[_]] &&
                    (! definingConstraint.isInstanceOf[Bool2Int1] || dx.isSingleton))
                {
                    intChannels += x
                }
            } else {
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

    private def finalizeIntegerSetDomain(x: IntegerSetVariable): Unit = {
        val dx = x.domain
        if (dx.isBounded) {
            if (cc.space.isChannelVariable(x)) {
                val costs = createBoolChannel()
                dx match {
                    case dx: IntegerPowersetDomain =>
                        cc.space.post(new Subset(nextConstraintId(), null, x, dx.base, costs))
                    case dx: SingletonIntegerSetDomain =>
                        cc.space.post(new Eq(nextConstraintId(), null, x, dx.base, costs))
                }
                cc.costVars += costs
            } else {
                if (dx.isSingleton) {
                    cc.space.setValue(x, dx.singleValue)
                }
            }
        }
    }

}
