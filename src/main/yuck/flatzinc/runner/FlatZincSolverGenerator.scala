package yuck.flatzinc.runner

import yuck.annealing.*
import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.flatzinc.compiler.{FlatZincCompiler, FlatZincCompilerResult}
import yuck.util.arm.SettableSigint
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class FlatZincSolverGenerator
    (ast: FlatZincAst,
     cfg: FlatZincSolverConfiguration,
     monitor: AnnealingMonitor,
     logger: LazyLogger,
     sigint: SettableSigint)
    extends SolverGenerator
{

    override def solverName = "FZS"

    private final class SolverForProblemWithoutNeighbourhood
        (override val name: String, compilerResult: FlatZincCompilerResult)
        extends Solver
    {
        require(compilerResult.maybeNeighbourhood.isEmpty)
        private val space = compilerResult.space
        private val objective = compilerResult.objective
        private var finished = false
        space.initialize()
        override def hasFinished = finished
        override def call() = {
            require(! finished)
            val result = new AnnealingResult(Some(compilerResult), name, objective, space.searchState, Vector())
            monitor.onSolverLaunched(result)
            monitor.onBetterProposal(result)
            finished = true
            monitor.onSolverFinished(result)
            result
        }
    }

    private final class BaseSolverGenerator
        (solverIndex: Int, randomGenerator: RandomGenerator)
        extends SolverGenerator
    {
        override def solverName = "FZS-%d".format(solverIndex)
        override def call() = {
            val compiler = new FlatZincCompiler(ast, cfg, randomGenerator.nextGen(), logger, sigint)
            val compilerResult = compiler.call()
            val space = compilerResult.space
            // The initializer will respect existing value assignments.
            val initializer = new RandomInitializer(space, randomGenerator.nextGen())
            logger.withTimedLogScope("Running initializer") {
                initializer.run()
            }
            if (compilerResult.maybeNeighbourhood.isEmpty) {
                new SolverForProblemWithoutNeighbourhood(solverName, compilerResult)
            } else {
                val neighbourhood = compilerResult.maybeNeighbourhood.get
                val n = neighbourhood.searchVariables.size
                val scheduleFactory = new StandardAnnealingScheduleFactory(n, randomGenerator.nextGen())
                val schedule = scheduleFactory.createHybridSchedule
                val startTemperature =
                    if compilerResult.performWarmStart then cfg.warmStartTemperature else cfg.startTemperature
                logger.log("Start temperature: %s".format(startTemperature))
                new SimulatedAnnealing(
                    solverName,
                    space,
                    compilerResult.objective,
                    neighbourhood,
                    schedule,
                    startTemperature, cfg.startTemperature,
                    cfg.perturbationProbability,
                    randomGenerator.nextGen(),
                    cfg.maybeRoundLimit,
                    Some(monitor),
                    Some(compilerResult),
                    sigint)
            }
        }
    }

    override def call() = {
        val randomGenerator = new JavaRandomGenerator(cfg.seed)
        val solvers =
            for (i <- 1 to cfg.numberOfSolvers) yield
                new OnDemandGeneratedSolver(
                    new BaseSolverGenerator(i, randomGenerator.nextGen()),
                    logger,
                    sigint)
        val solver =
            if (solvers.size == 1) solvers.head
            else new ParallelSolver(solvers, cfg.numberOfThreads, solverName, logger, sigint)
        solver
    }

}
