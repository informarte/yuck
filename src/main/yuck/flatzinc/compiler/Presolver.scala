package yuck.flatzinc.compiler

import scala.collection.*

import yuck.core.*

/**
 * Reduces variable domains by propagating constraints.
 *
 * Domains in the compilation context do not get updated, so later stages should ask variables for domains!
 *
 * Domains of implicitly constrained search variables are restored after presolving.
 *
 * @author Michael Marte
 */
final class Presolver
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    override def run() = {
        reduceDomains()
    }

    private def reduceDomains(): Unit ={

        // require that all constraints hold
        for (x <- cc.costVars) {
            x.pruneDomain(TrueDomain)
        }
        for (x <- cc.costVarsFromRedundantConstraints) {
            x.pruneDomain(TrueDomain)
        }

        // propagate constraints
        cc.space.propagate()

    }

}
