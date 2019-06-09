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
    (cc: CompilationContext, randomGenerator: RandomGenerator, sigint: Sigint)
    extends CompilationPhase(cc, randomGenerator)
{

    private val space = cc.space
    private val costVars = cc.costVars
    private val logger = cc.logger

    override def run {
        reduceDomains
    }

    private def reduceDomains {

        // collect domains of implicitly constrained search variables
        val backup = new mutable.ArrayBuffer[(AnyVariable, AnyDomain)]
        for (x <- cc.implicitlyConstrainedVars) {
            backup += x -> x.domain
        }

        // require that all constraints hold
        for (x <- costVars) {
            x.pruneDomain(TrueDomain)
        }

        // propagate constraints
        space.propagate

        // restore domains of implicitly constrained search variables
        for ((x, dx) <- backup) {
            dx match {
                case dx: BooleanDomain => BooleanValueTraits.safeDowncast(x).relaxDomain(dx)
                case dx: IntegerDomain => IntegerValueTraits.safeDowncast(x).relaxDomain(dx)
                case dx: IntegerSetDomain => IntegerSetValueTraits.safeDowncast(x).relaxDomain(dx)
            }
        }

    }

}
