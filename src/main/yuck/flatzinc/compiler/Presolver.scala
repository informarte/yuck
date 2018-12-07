package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.util.arm.Sigint

/**
 * Reduces variable domains by propagating constraints.
 *
 * Domains in the compilation context do not get updated, so later stages should ask variables for domains!
 *
 * Domains of implicitely constrained search variables are restored after presolving.
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

        // collect domains of implicitely constrained search variables
        val backup = new mutable.ArrayBuffer[(AnyVariable, AnyDomain)]
        for (x <- cc.implicitlyConstrainedVars) {
            backup += x -> x.domain
        }

        // require that all constraints hold
        for (x <- costVars) {
            x.pruneDomain(TrueDomain)
        }

        // propagate constraints (fixed-point iteration)
        def pass(i: Integer) {
            if (sigint.isSet) {
                throw new FlatZincCompilerInterruptedException
            }
            if (logger.withTimedLogScope("Pass %d".format(i)) {space.prune}) {
                pass(i + 1)
            }
        }
        try {
            pass(0)
        }
        catch {
            case error: yuck.core.DomainWipeOutException =>
                throw new yuck.flatzinc.compiler.DomainWipeOutException2(error.x)
        }

        // restore domains of implicitely constrained search variables
        for ((x, dx) <- backup) {
            dx match {
                case dx: BooleanDomain => BooleanValueTraits.safeDowncast(x).pruneDomain(dx)
                case dx: IntegerDomain => IntegerValueTraits.safeDowncast(x).pruneDomain(dx)
                case dx: IntegerSetDomain => IntegerSetValueTraits.safeDowncast(x).pruneDomain(dx)
            }
        }

    }

}
