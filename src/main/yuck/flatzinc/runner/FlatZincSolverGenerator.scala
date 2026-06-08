package yuck.flatzinc.runner

import java.time.Duration

import yuck.SolvingMethod
import yuck.annealing.*
import yuck.core.*
import yuck.fj.{FeasibilityJump, FeasibilityJumpNeighbourhood}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.flatzinc.compiler.{CompilationContext, FlatZincCompiler1, FlatZincCompiler2, FlatZincCompilerResult}
import yuck.flatzinc.util.{AnnealingMonitorFromPortfolioSolverMonitor, FeasibilityJumpMonitorFromPortfolioSolverMonitor, PortfolioSolverMonitor}
import yuck.util.arm.SettableSigint
import yuck.util.logging.LazyLogger
import yuck.util.logging.LogLevel.FineLogLevel

private final class SolverForProblemWithoutNeighbourhood
    (override val name: String, compilerResult: FlatZincCompilerResult, monitor: PortfolioSolverMonitor)
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
        val result = new AnnealingResult(Some(compilerResult), name, objective, space.searchState, 0, 0, 0, Vector())
        monitor.onSolverLaunched(result)
        monitor.onBetterProposal(result)
        finished = true
        monitor.onSolverFinished(result)
        result
    }
}

private final class FlatZincWorkerGenerator
    (private var cc: CompilationContext,
     solverIndex: Int,
     sharedBound: SharedBound,
     randomGenerator: RandomGenerator,
     monitor: PortfolioSolverMonitor,
     logger: LazyLogger,
     sigint: SettableSigint)
    extends SolverGenerator
{
    require(solverIndex >= 1)
    override def solverName = "FZS-%d".format(solverIndex)
    override def call() = {
        val cfg =
            if cc.cfg.maybePreferredSolvingMethod.isDefined
            then cc.cfg
            else cc.cfg.copy(
                maybePreferredSolvingMethod =
                    if solverIndex % 2 == 1
                    then Some(SolvingMethod.SimulatedAnnealing)
                    else Some(SolvingMethod.FeasibilityJump))
        val (cc1, copyingOverhead) =
            if cc.cfg.numberOfSolvers == 1
            then (cc.clone(cfg), Duration.ZERO)
            else cc.logger.withTimedLogScope("Copying compilation context") {
                cc.logger.withRootLogLevel(FineLogLevel) {
                    cc.copy(cfg)
                }
            }
        cc = null
        cc1.compilerRuntime = cc1.compilerRuntime.plus(copyingOverhead)
        val compiler2 = new FlatZincCompiler2(cc1, randomGenerator.nextGen())
        val compilerResult = compiler2.call()
        val space = compilerResult.space
        val initializer = new RandomInitializer(space, randomGenerator.nextGen())
        logger.withTimedLogScope("Running initializer") {
            // The initializer will respect existing value assignments, e.g. due to warm starting.
            initializer.run()
        }
        if compilerResult.maybeNeighbourhood.isEmpty then {
            new SolverForProblemWithoutNeighbourhood(solverName, compilerResult, monitor)
        } else if compilerResult.maybeNeighbourhood.get.isInstanceOf[FeasibilityJumpNeighbourhood] then {
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
            val schedule = scheduleFactory.createHybridSchedule(compilerResult.performWarmStart)
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
                saCfg.restartPerturbationProbability,
                if cfg.shareBounds then Some(sharedBound) else None,
                randomGenerator.nextGen(),
                saCfg.maybeRoundLimit,
                Some(new AnnealingMonitorFromPortfolioSolverMonitor(monitor)),
                Some(compilerResult),
                sigint)
        }
    }
}

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

    override def call() = {
        val randomGenerator = new JavaRandomGenerator(cfg.seed)
        val compiler1 = new FlatZincCompiler1(ast, cfg, sharedBound, logger, sigint)
        val cc = compiler1.call()
        val solvers =
            for i <- 1 to cfg.numberOfSolvers yield
                new OnDemandGeneratedSolver(
                    new FlatZincWorkerGenerator(cc, i, sharedBound, randomGenerator.nextGen(), monitor, logger, sigint),
                    logger,
                    sigint)
        val solver =
            if solvers.size == 1
            then solvers.head
            else new ParallelSolver(solvers, cfg.numberOfThreads, solverName, logger, sigint)
        solver
    }

}
