package yuck.flatzinc.compiler

import java.util.concurrent.Callable

import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class FlatZincCompiler
    (ast: FlatZincAst,
     cfg: FlatZincSolverConfiguration,
     randomGenerator: RandomGenerator,
     logger: LazyLogger,
     sigint: Sigint)
    extends Callable[FlatZincCompilerResult]
{

    override def call() = {

        val cc = new CompilationContext(ast, cfg, logger, sigint)

        randomGenerator.nextGen()
        run(new DomainInitializer(cc))
        randomGenerator.nextGen()
        run(new VariableFactory(cc))
        randomGenerator.nextGen()
        run(new VariableClassifier(cc))
        randomGenerator.nextGen()
        run(new ConstraintFactory(cc, sigint))
        randomGenerator.nextGen()
        run(new DomainFinalizer(cc))
        randomGenerator.nextGen()
        run(new ObjectiveFactory(cc))
        randomGenerator.nextGen()
        if (cfg.runPresolver) {
            run(new Presolver(cc))
        }
        run(new ConstraintDrivenNeighbourhoodFactory(cc, randomGenerator.nextGen(), sigint))

        checkSearchVariableDomains(cc)
        assignValuesToDanglingVariables(cc)

        logger.criticalSection {
            logger.withLogScope("Yuck model statistics") {
                logYuckModelStatistics(cc)
            }
        }

        val vars = (for ((key, x) <- cc.vars) yield key.toString -> x).toMap
        val arrays = (for ((key, array) <- cc.arrays) yield key.toString -> array).toMap
        new FlatZincCompilerResult(cc.ast, cc.space, vars, arrays, cc.objective, cc.maybeNeighbourhood)

    }

    // Use the optional root log level to focus on a particular compilation phase.
    private def run(phase: CompilationPhase, rootLogLevel: yuck.util.logging.LogLevel = yuck.util.logging.FineLogLevel): Unit = {
        if (sigint.isSet) {
            throw new FlatZincCompilerInterruptedException
        }
        logger.withRootLogLevel(rootLogLevel) {
            logger.withTimedLogScope("Running %s".format(phase.getClass.getSimpleName)) {
                phase.run()
            }
        }
    }

    private def checkSearchVariableDomains(cc: CompilationContext): Unit = {
        for (x <- cc.space.searchVariables) {
            if (! x.domain.isFinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
        }
    }

    private def assignValuesToDanglingVariables(cc: CompilationContext): Unit = {
        for ((key, x) <- cc.vars
             if cc.space.isDanglingVariable(x) && ! cc.space.searchState.hasValue(x))
        {
            if (! x.domain.isFinite) {
                throw new VariableWithInfiniteDomainException(x)
            }
            cc.logger.logg("Assigning random value to dangling variable %s".format(x))
            x.randomMoveEffect(randomGenerator).affect(cc.space)
        }
    }

    private def logYuckModelStatistics(cc: CompilationContext) = {
        lazy val searchVariables = cc.space.searchVariables
        logger.logg("Search variables: %s".format(searchVariables))
        logger.log("%d search variables".format(searchVariables.size))
        lazy val searchVariablesCoveredByNeighbourhood =
            cc.maybeNeighbourhood.map(_.searchVariables).getOrElse(Set[AnyVariable]())
        logger.logg("Search variables covered by neighbourhood: %s".format(searchVariablesCoveredByNeighbourhood))
        logger.log("%d search variables covered by neighbourhood".format(searchVariablesCoveredByNeighbourhood.size))
        logger.log("%d channel variables".format(cc.space.channelVariables.size))
        lazy val danglingVariables = cc.vars.valuesIterator.filter(cc.space.isDanglingVariable(_)).toSet
        logger.logg("Dangling variables: %s".format(danglingVariables))
        logger.log("%d dangling variables".format(danglingVariables.size))
        logger.log("%d constraints".format(cc.space.numberOfConstraints))
        logger.log("%d implicit constraints".format(cc.space.numberOfImplicitConstraints))
    }

}
