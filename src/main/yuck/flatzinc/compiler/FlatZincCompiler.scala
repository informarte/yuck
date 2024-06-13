package yuck.flatzinc.compiler

import java.util.concurrent.Callable

import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * This class orchestrates the various compiler stages.
 *
 * There are a lot of constraints on the execution order of the stages,
 * see doc/design/compiler/compiler-stage-ordering.mzn.
 * (The problem has 5 solutions one of which is implemented here.)
 *
 * @author Michael Marte
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

        val (cc, runtime) = logger.withTimedLogScope("Compiling problem") {
            compile()
        }

        logger.criticalSection {
            logger.withLogScope("Yuck model statistics") {
                logYuckModelStatistics(cc)
            }
        }

        val vars = (for ((key, x) <- cc.vars) yield key.toString -> x).toMap
        val arrays = (for ((key, array) <- cc.arrays) yield key.toString -> array).toMap
        new FlatZincCompilerResult(
            cc.ast, cc.space, vars, arrays, cc.objective, cc.maybeNeighbourhood, ! cc.warmStartAssignment.isEmpty, runtime)

    }

    private def compile(): CompilationContext = {

        val cc = new CompilationContext(ast, cfg, logger, sigint)

        randomGenerator.nextGen()
        run(new DomainInitializer(cc))
        randomGenerator.nextGen()
        run(new VariableFactory(cc))
        randomGenerator.nextGen()
        run(new VariableClassifier(cc))
        randomGenerator.nextGen()
        run(new ConstraintFactory(cc))
        randomGenerator.nextGen()
        randomGenerator.nextGen()
        run(new ObjectiveFactory(cc))
        randomGenerator.nextGen()
        if (cfg.runPresolver) {
            run(new Presolver(cc))
        }
        run(new ConstraintDrivenNeighbourhoodFactory(cc, randomGenerator.nextGen()))
        if (cfg.pruneConstraintNetwork) {
            run(new ConstraintNetworkPruner(cc))
        }
        run(new WarmStartAnnotationParser(cc))

        checkSearchVariableDomains(cc)
        assignValuesToDanglingVariables(cc)

        cc
    }

    // Use the optional root log level to focus on a particular compilation phase.
    private def run(phase: CompilationPhase, rootLogLevel: yuck.util.logging.LogLevel = yuck.util.logging.LogLevel.FineLogLevel): Unit = {
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
        for (x <- cc.vars.values
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
        logger.logg("Search variables: %s".format(searchVariables.toList.sorted.mkString(", ")))
        logger.log("%d search variables".format(searchVariables.size))
        lazy val searchVariablesCoveredByNeighbourhood =
            cc.maybeNeighbourhood.map(_.searchVariables).getOrElse(Set[AnyVariable]())
        logger.logg("Search variables covered by neighbourhood: %s".format(searchVariablesCoveredByNeighbourhood.toList.sorted.mkString(", ")))
        logger.log("%d search variables covered by neighbourhood".format(searchVariablesCoveredByNeighbourhood.size))
        logger.logg("Implicitly constrained search variables: %s".format(cc.space.implicitlyConstrainedSearchVariables.toList.sorted.mkString(", ")))
        logger.log("%d implicitly constrained search variables".format(cc.space.implicitlyConstrainedSearchVariables.size))
        logger.log("%d channel variables".format(cc.space.channelVariables.size))
        lazy val danglingVariables = cc.vars.valuesIterator.filter(cc.space.isDanglingVariable).toSet
        logger.logg("Dangling variables: %s".format(danglingVariables.toList.sorted.mkString(", ")))
        logger.log("%d dangling variables".format(danglingVariables.size))
        logger.log("%d constraints".format(cc.space.numberOfConstraints))
        logger.log("%d implicit constraints".format(cc.space.numberOfImplicitConstraints))
    }

}
