package yuck.flatzinc.compiler

import java.util.concurrent.Callable

import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAST
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class FlatZincCompiler
    (ast: FlatZincAST,
     cfg: FlatZincSolverConfiguration,
     randomGenerator: RandomGenerator,
     logger: LazyLogger)
    extends Callable[FlatZincCompilerResult]
{

    private val cc = new CompilationContext(ast, cfg, logger)

    // Use the optional root log level to focus on a particular compilation phase.
    private def run(phase: CompilationPhase, rootLogLevel: yuck.util.logging.LogLevel = yuck.util.logging.FineLogLevel) {
        logger.withRootLogLevel(rootLogLevel) {
            logger.withTimedLogScope("Running %s".format(phase.getClass.getSimpleName)) {
                phase.run
            }
        }
    }

    override def call = {
        // Each phase gets its own random generator.
        // This way we can change, reorder, or skip phases without changing the outcome of later phases.
        run(new DomainInitializer(cc, randomGenerator.nextGen))
        run(new DomainPruner(cc, randomGenerator.nextGen))
        run(new VariableFactory(cc, randomGenerator.nextGen))
        run(new VariableClassifier(cc, randomGenerator.nextGen))
        run(new ConstraintFactory(cc, randomGenerator.nextGen))
        run(new DomainEnforcer(cc, randomGenerator.nextGen))
        run(new ObjectiveFactory(cc, randomGenerator.nextGen))
        run(new ConstraintDrivenNeighbourhoodFactory(cc, randomGenerator.nextGen))
        assignValuesToDanglingVariables
        logger.criticalSection {
            logger.withLogScope("Yuck model statistics") {
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
        val vars = (for ((key, x) <- cc.vars) yield key.toString -> x).toMap
        val arrays = (for ((key, array) <- cc.arrays) yield key.toString -> array).toMap
        new FlatZincCompilerResult(cc.ast, cc.space, vars, arrays, cc.costVar, cc.objective, cc.maybeNeighbourhood)
    }

    private def assignValuesToDanglingVariables {
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

}
