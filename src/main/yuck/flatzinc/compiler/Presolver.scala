package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.util.arm.Sigint

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

    private val space = cc.space
    private val costVars = cc.costVars

    override def run() = {
        reduceDomains()
    }

    private def reduceDomains(): Unit ={

        // require that all constraints hold
        for (x <- costVars) {
            x.pruneDomain(TrueDomain)
        }

        // propagate constraints
        space.propagate()

    }

}
