package yuck.flatzinc.compiler

import java.time.Duration
import java.util.concurrent.Callable

import yuck.SolvingMethod
import yuck.constraints.Delivery
import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.util.arm.Sigint
import yuck.util.logging.{LazyLogger, LogLevel}

/**
 * This class orchestrates the various compiler stages.
 *
 * There are a lot of constraints on the execution order of the stages,
 * see src/doc/design/compiler/compiler-stage-ordering.mzn.
 * (The problem has 5 solutions one of which is implemented here.)
*/
final class FlatZincCompiler
    (ast: FlatZincAst,
     cfg: FlatZincSolverConfiguration,
     randomGenerator: RandomGenerator,
     sharedBound: SharedBound,
     logger: LazyLogger,
     sigint: Sigint)
    extends Callable[FlatZincCompilerResult]
{

    override def call() = {

        val (result, runtime) = logger.withTimedLogScope("Compiling problem") {
            compile()
        }

        val (cc, stageRuntimes) = result

        logger.criticalSection {
            logger.withLogScope("Yuck model metrics") {
                logYuckModelMetrics(cc)
            }
        }

        val vars = (for (key, x) <- cc.vars yield key.toString -> x).toMap
        val arrays = (for (key, array) <- cc.arrays yield key.toString -> array).toMap
        new FlatZincCompilerResult(
            cc.ast, cc.space, vars, arrays, cc.objective, cc.maybeNeighbourhood, ! cc.warmStartAssignment.isEmpty,
            runtime, stageRuntimes)

    }

    private def compile(): (CompilationContext, FlatZincCompilerStageRuntimes) = {

        val cc = new CompilationContext(ast, cfg, sharedBound, logger, sigint)

        randomGenerator.nextGen()
        val domainInitializerRuntime = run(new DomainInitializer(cc))
        randomGenerator.nextGen()
        val variableFactoryRuntime = run(new VariableFactory(cc))
        randomGenerator.nextGen()
        val variableClassifierRuntime = run(new VariableClassifier(cc))
        randomGenerator.nextGen()
        val constraintFactoryRuntime = run(new ConstraintFactory(cc))
        randomGenerator.nextGen()
        randomGenerator.nextGen()
        val objectiveFactoryRuntime = run(new ObjectiveFactory(cc))
        randomGenerator.nextGen()
        val presolverRuntime = if cfg.runPresolver then run(new Presolver(cc)) else Duration.ofMillis(0)
        val objectiveIsSuitableForFj = cc.objective match {
            case _: SatisfactionObjective => true
            case hierarchicalObjective: HierarchicalObjective =>
                hierarchicalObjective.primitiveObjectives match {
                    case List(_: SatisfactionObjective, numericalObjective: NumericalObjective[?, ?, ?]) =>
                        numericalObjective.maybeY.isDefined
                    case _ => false
                }
            case _ => false
        }
        val useAnnealing =
            cfg.maybePreferredSolvingMethod.getOrElse(SolvingMethod.SimulatedAnnealing) == SolvingMethod.SimulatedAnnealing ||
            ! objectiveIsSuitableForFj ||
            cc.space.searchVariables.iterator.exists(_.isInstanceOf[IntegerSetVariable]) ||
            // Delivery requires the circuit to be maintained by a neighbourhood.
            cc.costVars.exists(costs =>
                cc.space.maybeDefiningConstraint(costs).exists(_.isInstanceOf[Delivery[?, ?, ?]]))
        val neighbourhoodFactoryRuntime =
            if useAnnealing
            then run(new AnnealingNeighbourhoodFactory(cc, randomGenerator.nextGen()))
            else run(new FeasibilityJumpNeighbourhoodFactory(cc, randomGenerator.nextGen()))
        val constraintNetworkPrunerRuntime =
            if cfg.pruneConstraintNetwork then run(new ConstraintNetworkPruner(cc)) else Duration.ofMillis(0)
        val arrayAccessOptimizerRuntime =
            if cfg.optimizeArrayAccess then run(new ArrayAccessOptimizer(cc)) else Duration.ofMillis(0)
        val warmStartAnnotationParserRuntime = run(new WarmStartAnnotationParser(cc))

        checkSearchVariableDomains(cc)
        assignValuesToDanglingVariables(cc)

        val stageRuntimes = FlatZincCompilerStageRuntimes(
            domainInitializerRuntime, variableFactoryRuntime, variableClassifierRuntime, constraintFactoryRuntime,
            objectiveFactoryRuntime, presolverRuntime, neighbourhoodFactoryRuntime, constraintNetworkPrunerRuntime,
            arrayAccessOptimizerRuntime, warmStartAnnotationParserRuntime)

        (cc, stageRuntimes)

    }

    // Use the optional root log level to focus on a particular compilation phase.
    private def run(phase: CompilationPhase, rootLogLevel: LogLevel = LogLevel.FineLogLevel): Duration = {
        if sigint.isSet then {
            throw new FlatZincCompilerInterruptedException
        }
        val (_, duration) =
            logger.withRootLogLevel(rootLogLevel) {
                logger.withTimedLogScope("Running %s".format(phase.getClass.getSimpleName)) {
                    phase.run()
                }
            }
        duration
    }

    private def checkSearchVariableDomains(cc: CompilationContext): Unit = {
        for x <- cc.space.searchVariables do {
            if ! x.domain.isFinite then {
                throw new VariableWithInfiniteDomainException(x)
            }
        }
    }

    private def assignValuesToDanglingVariables(cc: CompilationContext): Unit = {
        for x <- cc.vars.values
             if cc.space.isDanglingVariable(x) && ! cc.space.searchState.hasValue(x) do
        {
            if ! x.domain.isFinite then {
                throw new VariableWithInfiniteDomainException(x)
            }
            cc.logger.logg("Assigning random value to dangling variable %s".format(x))
            x.randomMoveEffect(randomGenerator).affect(cc.space)
        }
    }

    private def logYuckModelMetrics(cc: CompilationContext): Unit = {
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
