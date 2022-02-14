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
     sigint: SettableSigint,
     logger: LazyLogger,
     monitor: AnnealingMonitor)
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
            val result = new AnnealingResult(name, space, objective, Some(compilerResult))
            result.bestProposal = space.searchState
            result.costsOfBestProposal = objective.costs(result.bestProposal)
            monitor.onSolverLaunched(result)
            monitor.onBetterProposal(result)
            finished = true
            monitor.onSolverFinished(result)
            result
        }
    }

    private final class BaseSolverGenerator
        (randomGenerator: RandomGenerator, solverIndex: Int)
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
                schedule.start(DefaultStartTemperature, 0)
                logger.log("Start temperature: %s".format(schedule.temperature))
                new SimulatedAnnealing(
                    solverName,
                    space,
                    schedule,
                    neighbourhood,
                    randomGenerator.nextGen(),
                    compilerResult.objective,
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
            for (i <- 1 to 1 + cfg.restartLimit) yield
                new OnDemandGeneratedSolver(
                    new BaseSolverGenerator(randomGenerator.nextGen(), i),
                    logger,
                    sigint)
        val solver =
            if (solvers.size == 1) solvers.head
            else new ParallelSolver(solvers, cfg.numberOfThreads, solverName, logger, sigint)
        solver
    }

}
