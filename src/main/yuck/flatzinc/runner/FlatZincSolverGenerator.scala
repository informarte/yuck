package yuck.flatzinc.runner

import java.util.logging._

import scala.math._

import yuck.annealing._
import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.flatzinc.compiler.{FlatZincCompiler, FlatZincCompilerResult}
import yuck.util.arm.{SettableSigint, Sigint}
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
        space.initialize
        override def hasFinished = finished
        override def call = {
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
        override def call = {
            val compiler = new FlatZincCompiler(ast, cfg, randomGenerator.nextGen, sigint, logger)
            val compilerResult = compiler.call
            val space = compilerResult.space
            // The initializer will respect existing value assignments.
            val initializer = new RandomInitializer(space, randomGenerator.nextGen)
            initializer.run
            if (compilerResult.maybeNeighbourhood.isEmpty) {
                new SolverForProblemWithoutNeighbourhood(solverName, compilerResult)
            } else {
                val neighbourhood = compilerResult.maybeNeighbourhood.get
                val n = neighbourhood.searchVariables.size
                val scheduleFactory = new StandardAnnealingScheduleFactory(n, randomGenerator.nextGen)
                val schedule = scheduleFactory.createRandomSchedule
                scheduleFactory.startScheduleWithRandomTemperature(schedule)
                logger.log("Start temperature: %s".format(schedule.temperature))
                new SimulatedAnnealing(
                    solverName,
                    space,
                    schedule,
                    neighbourhood,
                    randomGenerator.nextGen,
                    compilerResult.objective,
                    cfg.maybeRoundLimit,
                    Some(monitor),
                    Some(compilerResult),
                    sigint)
            }
        }
    }

    override def call = {
        logger.withLogScope("FlatZinc model statistics") {
            logFlatZincModelStatistics
        }
        val randomGenerator = new JavaRandomGenerator(cfg.seed)
        val solvers =
            for (i <- 1 to 1 + max(0, cfg.restartLimit)) yield
                new OnDemandGeneratedSolver(
                    new BaseSolverGenerator(randomGenerator.nextGen, i),
                    logger,
                    sigint)
        val numberOfThreads = cfg.numberOfVirtualCores match {
            case 4 | 8 => 4 // use 4 threads on both i5 and i7, also avoids fan noise on i7
            case n => n
        }
        val solver =
            if (solvers.size == 1) solvers.head
            else new ParallelSolver(solvers, numberOfThreads, solverName, logger, sigint)
        solver
    }

    private def logFlatZincModelStatistics = {
        logger.log("%d predicate declarations".format(ast.predDecls.size))
        logger.log("%d parameter declarations".format(ast.paramDecls.size))
        logger.log("%d variable declarations".format(ast.varDecls.size))
        logger.log("%d constraints".format(ast.constraints.size))
    }

}
