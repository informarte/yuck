package yuck.flatzinc.compiler

import java.time.Duration
import java.util.concurrent.Callable

import yuck.SolvingMethod
import yuck.constraints.Delivery
import yuck.core.*
import yuck.util.logging.LogLevel

/**
 * This class orchestrates the various compiler stages.
 *
 * There are a lot of constraints on the execution order of the stages,
 * see src/doc/design/compiler/compiler-stage-ordering.mzn.
 * (The problem has 5 solutions, one of which is implemented here.)
 */
final class FlatZincCompiler2
   (cc: CompilationContext, randomGenerator: RandomGenerator)
   extends Callable[FlatZincCompilerResult]
{

    override def call() = {

        val (_, runtime) = cc.logger.withTimedLogScope("Compiling problem") {
            compile()
        }

        cc.compilerRuntime = cc.compilerRuntime.plus(runtime)

        cc.logger.criticalSection {
            cc.logger.withLogScope("Yuck model metrics") {
                logYuckModelMetrics(cc)
            }
        }

        new FlatZincCompilerResult(
            cc.space, cc.outputVars.to(HashMap), cc.outputArrays.to(HashMap),
            cc.objective, cc.maybeNeighbourhood, ! cc.warmStartAssignment.isEmpty,
            cc.compilerRuntime, cc.compilerStageRuntimes)

    }

    private def compile(): Unit = {

        // Mimic behaviour of previous FlatZincCompiler
        for (i <- 1 to 7) {
            randomGenerator.nextGen()
        }

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
            cc.cfg.maybePreferredSolvingMethod.getOrElse(SolvingMethod.SimulatedAnnealing) == SolvingMethod.SimulatedAnnealing ||
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
            if cc.cfg.pruneConstraintNetwork then run(new ConstraintNetworkPruner(cc)) else Duration.ZERO
        val arrayAccessOptimizerRuntime =
            if cc.cfg.optimizeArrayAccess then run(new ArrayAccessOptimizer(cc)) else Duration.ZERO

        checkSearchVariableDomains(cc)
        assignValuesToDanglingVariables(cc)

        cc.compilerStageRuntimes = FlatZincCompilerStageRuntimes(
            cc.compilerStageRuntimes.domainInitializerRuntime, cc.compilerStageRuntimes.variableFactoryRuntime,
            cc.compilerStageRuntimes.variableClassifierRuntime, cc.compilerStageRuntimes.constraintFactoryRuntime,
            cc.compilerStageRuntimes.cycleBreakerRuntime, cc.compilerStageRuntimes.objectiveFactoryRuntime,
            cc.compilerStageRuntimes.presolverRuntime, neighbourhoodFactoryRuntime, constraintNetworkPrunerRuntime,
            arrayAccessOptimizerRuntime, cc.compilerStageRuntimes.warmStartAnnotationParserRuntime)

    }

    // Use the optional root log level to focus on a particular compilation phase.
    private def run(phase: CompilationPhase, rootLogLevel: LogLevel = LogLevel.FineLogLevel): Duration = {
        if cc.sigint.isSet then {
            throw new FlatZincCompilerInterruptedException
        }
        val (_, duration) =
            cc.logger.withRootLogLevel(rootLogLevel) {
                cc.logger.withTimedLogScope("Running %s".format(phase.getClass.getSimpleName)) {
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
        for x <- cc.danglingVars if ! cc.space.searchState.hasValue(x) do {
            if ! x.domain.isFinite then {
                throw new VariableWithInfiniteDomainException(x)
            }
            cc.logger.logg("Assigning random value to dangling output variable %s".format(x))
            x.randomMoveEffect(randomGenerator).affect(cc.space)
        }
    }

    private def logYuckModelMetrics(cc: CompilationContext): Unit = {
        lazy val searchVariables = cc.space.searchVariables
        cc.logger.logg("Search variables: %s".format(searchVariables.toList.sorted.mkString(", ")))
        cc.logger.log("%d search variables".format(searchVariables.size))
        lazy val searchVariablesCoveredByNeighbourhood =
            cc.maybeNeighbourhood.map(_.searchVariables).getOrElse(Set[AnyVariable]())
        cc.logger.logg("Search variables covered by neighbourhood: %s".format(searchVariablesCoveredByNeighbourhood.toList.sorted.mkString(", ")))
        cc.logger.log("%d search variables covered by neighbourhood".format(searchVariablesCoveredByNeighbourhood.size))
        cc.logger.logg("Implicitly constrained search variables: %s".format(cc.space.implicitlyConstrainedSearchVariables.toList.sorted.mkString(", ")))
        cc.logger.log("%d implicitly constrained search variables".format(cc.space.implicitlyConstrainedSearchVariables.size))
        cc.logger.log("%d channel variables".format(cc.space.channelVariables.size))
        lazy val danglingVariables = cc.vars.valuesIterator.filter(cc.space.isDanglingVariable).toSet
        cc.logger.logg("Dangling variables: %s".format(danglingVariables.toList.sorted.mkString(", ")))
        cc.logger.log("%d dangling variables".format(danglingVariables.size))
        cc.logger.log("%d constraints".format(cc.space.numberOfConstraints))
        cc.logger.log("%d implicit constraints".format(cc.space.numberOfImplicitConstraints))
    }

}
