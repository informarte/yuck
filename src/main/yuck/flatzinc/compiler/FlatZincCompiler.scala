package yuck.flatzinc.compiler

import java.util.concurrent.Callable

import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class FlatZincCompiler
    (ast: FlatZincAst,
     cfg: FlatZincSolverConfiguration,
     randomGenerator: RandomGenerator,
     logger: LazyLogger)
    extends Callable[FlatZincCompilerResult]
{

    override def call = {

        val cc =
            if (cfg.preferImplicitSolvingOverDomainPruning) {
                try {
                    preferImplicitSolvingOverDomainPruning
                }
                catch {
                    case error: VariableWithInfiniteDomainException =>
                        logger.log(error.getMessage)
                        logger.log("Trying again, now with pruning before neighbourhood generation")
                        preferDomainPruningOverImplicitSolving
                }
            } else {
                preferDomainPruningOverImplicitSolving
            }

        checkSearchVariableDomains(cc)
        assignValuesToDanglingVariables(cc)

        logger.criticalSection {
            logger.withLogScope("Yuck model statistics") {
                logModelStatistics(cc)
            }
        }

        val vars = (for ((key, x) <- cc.vars) yield key.toString -> x).toMap
        val arrays = (for ((key, array) <- cc.arrays) yield key.toString -> array).toMap
        new FlatZincCompilerResult(cc.ast, cc.space, vars, arrays, cc.costVar, cc.objective, cc.maybeNeighbourhood)

    }

    private def preferDomainPruningOverImplicitSolving: CompilationContext = {
        val cc = new CompilationContext(ast, cfg, logger)
        run(new DomainInitializer(cc, randomGenerator.nextGen))
        run(new DomainPruner(cc, randomGenerator.nextGen))
        finishCompilation(cc)
        cc
    }

    private def preferImplicitSolvingOverDomainPruning: CompilationContext = {

        // The compilation proceeds in three stages:
        // The first stage identifies implicit constraints.
        // The second stage computes domain reductions.
        // The third stage builds the final model where implicit solving takes precedence over domain pruning.
        // The purpose of this approach is to take domain reductions into account as far as possible without
        // inhibiting implicit solving.

        val cc2 = {

            // stage 1: identify implicit constraints
            val cc1 = new CompilationContext(ast, cfg, logger)
            run(new DomainInitializer(cc1, randomGenerator.nextGen))
            finishCompilation(cc1)

            // stage 2: compute domain reductions
            run(new DomainPruner(cc1, randomGenerator.nextGen))

            // stage 3: build final model, taking domain reductions into account as far as possible without
            // inhibiting implicit solving
            val cc2 = new CompilationContext(ast, cfg, logger)
            run(new DomainInitializer(cc2, randomGenerator.nextGen))
            for ((a, domain) <- cc1.domains if ! cc1.implicitlyConstrainedVars.contains(cc1.vars(a))) {
                cc2.domains += a -> domain
            }
            cc2.equalVars ++= cc1.equalVars
            for (constraint <- cc1.impliedConstraints
                 if ast.involvedVariables(constraint).map(cc1.vars).intersect(cc1.implicitlyConstrainedVars).isEmpty)
            {
                cc2.impliedConstraints += constraint
            }

            // From this point on, we don't need cc1 anymore, so let it go out of scope to free up memory.
            cc2

        }

        // continuation of stage 3
        finishCompilation(cc2)

        cc2

    }

    private def finishCompilation(cc: CompilationContext) {
        run(new VariableFactory(cc, randomGenerator.nextGen))
        run(new VariableClassifier(cc, randomGenerator.nextGen))
        run(new ConstraintFactory(cc, randomGenerator.nextGen))
        run(new DomainFinalizer(cc, randomGenerator.nextGen))
        run(new ObjectiveFactory(cc, randomGenerator.nextGen))
        run(new ConstraintDrivenNeighbourhoodFactory(cc, randomGenerator.nextGen))
    }

    // Use the optional root log level to focus on a particular compilation phase.
    private def run(phase: CompilationPhase, rootLogLevel: yuck.util.logging.LogLevel = yuck.util.logging.FineLogLevel) {
        logger.withRootLogLevel(rootLogLevel) {
            logger.withTimedLogScope("Running %s".format(phase.getClass.getSimpleName)) {
                phase.run
            }
        }
    }

    private def checkSearchVariableDomains(cc: CompilationContext): Unit = {
        for (x <- cc.space.searchVariables) {
            if (x.domain.isInfinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
        }
    }

    private def assignValuesToDanglingVariables(cc: CompilationContext) {
        for ((key, x) <- cc.vars
             if cc.space.isDanglingVariable(x) && ! cc.space.searchState.hasValue(x))
        {
            if (x.domain.isInfinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
            cc.logger.logg("Assigning random value to dangling variable %s".format(x))
            x.assignRandomValue(cc.space, randomGenerator)
        }
    }

    private def logModelStatistics(cc: CompilationContext) = {
        val searchVariables = cc.space.searchVariables
        logger.logg("Search variables: %s".format(searchVariables))
        logger.log("%d search variables".format(searchVariables.size))
        val searchVariablesCoveredByNeighbourhood =
            cc.maybeNeighbourhood.map(_.searchVariables).getOrElse(Set[AnyVariable]())
        logger.logg("Search variables covered by neighbourhood: %s".format(searchVariablesCoveredByNeighbourhood))
        logger.log("%d search variables covered by neighbourhood".format(searchVariablesCoveredByNeighbourhood.size))
        logger.log("%d channel variables".format(cc.space.channelVariables.size))
        val danglingVariables = cc.vars.valuesIterator.toSet.filter(cc.space.isDanglingVariable(_))
        logger.logg("Dangling variables: %s".format(danglingVariables))
        logger.log("%d dangling variables".format(danglingVariables.size))
        logger.log("%d constraints".format(cc.space.numberOfConstraints))
        logger.log("%d implicit constraints".format(cc.space.numberOfImplicitConstraints))
    }

}
