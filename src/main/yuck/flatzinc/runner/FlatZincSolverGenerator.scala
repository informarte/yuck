package yuck.flatzinc.runner

import yuck.SolvingMethod
import yuck.annealing.*
import yuck.core.*
import yuck.fj.{FeasibilityJump, FeasibilityJumpNeighbourhood}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.flatzinc.compiler.{FlatZincCompiler, FlatZincCompilerResult}
import yuck.flatzinc.util.{AnnealingMonitorFromPortfolioSolverMonitor, FeasibilityJumpMonitorFromPortfolioSolverMonitor, PortfolioSolverMonitor}
import yuck.util.arm.SettableSigint
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class FlatZincSolverGenerator
    (ast: FlatZincAst,
     cfg: FlatZincSolverConfiguration,
     sharedBound: SharedBound,
     monitor: PortfolioSolverMonitor,
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
            val result = new AnnealingResult(Some(compilerResult), name, objective, space.searchState, 0, 0, 0, 0, 0, Vector())
            monitor.onSolverLaunched(result)
            monitor.onBetterProposal(result)
            finished = true
            monitor.onSolverFinished(result)
            result
        }
    }

    private final class BaseSolverGenerator
        (cfg: FlatZincSolverConfiguration, solverIndex: Int, randomGenerator: RandomGenerator)
        extends SolverGenerator
    {
        require(solverIndex >= 1)
        override def solverName = "FZS-%d".format(solverIndex)
        override def call() = {
            val cfg =
                if this.cfg.maybePreferredSolvingMethod.isDefined
                then this.cfg
                else this.cfg.copy(
                    maybePreferredSolvingMethod =
                        if solverIndex % 2 == 1
                        then Some(SolvingMethod.SimulatedAnnealing)
                        else Some(SolvingMethod.FeasibilityJump))
            val compiler = new FlatZincCompiler(ast, cfg, randomGenerator.nextGen(), sharedBound, logger, sigint)
            val compilerResult = compiler.call()
            val space = compilerResult.space
            val initializer = new RandomInitializer(space, randomGenerator.nextGen())
            logger.withTimedLogScope("Running initializer") {
                // The initializer will respect existing value assignments, e.g. due to warm starting.
                initializer.run()
            }
            if (compilerResult.maybeNeighbourhood.isEmpty) {
                new SolverForProblemWithoutNeighbourhood(solverName, compilerResult)
            } else if (compilerResult.maybeNeighbourhood.get.isInstanceOf[FeasibilityJumpNeighbourhood]) {
                logger.log("Using feasibility jump method")
                val fjCfg = cfg.feasibilityJumpConfiguration
                val neighbourhood = compilerResult.maybeNeighbourhood.get
                new FeasibilityJump(
                    solverName,
                    space,
                    compilerResult.objective,
                    neighbourhood.asInstanceOf[FeasibilityJumpNeighbourhood],
                    fjCfg.numberOfMovesPerRound(neighbourhood.searchVariables.size),
                    fjCfg.numberOfSuccessiveFutileRoundsUntilPerturbation,
                    fjCfg.perturbationProbability,
                    if cfg.shareBounds then Some(sharedBound) else None,
                    randomGenerator.nextGen(),
                    Some(new FeasibilityJumpMonitorFromPortfolioSolverMonitor(monitor)),
                    Some(compilerResult),
                    sigint)
            } else {
                logger.log("Using simulated annealing")
                val saCfg = cfg.annealingConfiguration
                val neighbourhood = compilerResult.maybeNeighbourhood.get
                val scheduleFactory =
                    new AnnealingScheduleFactory(neighbourhood.searchVariables.size, randomGenerator.nextGen())
                val schedule = scheduleFactory.createHybridSchedule()
                val startTemperature =
                    if compilerResult.performWarmStart then saCfg.warmStartTemperature else saCfg.startTemperature
                logger.log("Start temperature: %s".format(startTemperature))
                new SimulatedAnnealing(
                    solverName,
                    space,
                    compilerResult.objective,
                    neighbourhood,
                    schedule,
                    startTemperature,
                    saCfg.startTemperature,
                    saCfg.perturbationProbability,
                    if cfg.shareBounds then Some(sharedBound) else None,
                    randomGenerator.nextGen(),
                    saCfg.maybeRoundLimit,
                    Some(new AnnealingMonitorFromPortfolioSolverMonitor(monitor)),
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
                    new BaseSolverGenerator(cfg, i, randomGenerator.nextGen()),
                    logger,
                    sigint)
        val solver =
            if (solvers.size == 1) solvers.head
            else new ParallelSolver(solvers, cfg.numberOfThreads, solverName, logger, sigint)
        solver
    }

}
