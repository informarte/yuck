package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest
import yuck.util.arm.{RevocableSigint, SettableSigint, Sigint}


/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SolverTest extends UnitTest {

    private class GoodSolver(result: Result, sleepTimeInMillis: Int, sigint: Sigint) extends Solver {
        override def name = result.solverName
        private var finished = false
        private def wasInterrupted = sigint.isSet
        override def hasFinished = finished
        override def call() = {
            require(! hasFinished)
            if (wasInterrupted) {
                throw new SolverInterruptedException
            } else {
                Thread.sleep(sleepTimeInMillis)
                if (wasInterrupted) {
                    throw new SolverInterruptedException
                } else {
                    finished = true
                    result
                }
            }
        }
    }

    private class BadSolverException extends RuntimeException

    private class BadSolver extends Solver {
        override def call() = {
            throw new BadSolverException
        }
        override def hasFinished = false
    }

    @Test
    def testTimeboxingWithTimeout(): Unit = {
        val sigint = new RevocableSigint
        val result = new Result("0", null, null, null)
        val solver = new GoodSolver(result, 100, sigint)
        val timebox = new TimeboxedSolver(solver, 0, logger, sigint)
        assertEx(timebox.call(), classOf[SolverInterruptedException])
        assert(! solver.hasFinished)
        assert(timebox.hasFinished)
        assert(sigint.isSet)
        sigint.revoke()
        assertEx(timebox.call(), classOf[SolverInterruptedException])
        assert(! solver.hasFinished)
        assert(timebox.hasFinished)
        assert(sigint.isSet)
    }

    @Test
    def testTimeboxingWithoutTimeout(): Unit = {
        val result = new Result("0", null, null, null)
        val solver = new TimeboxedSolver(new GoodSolver(result, 0, sigint), 1, logger, sigint)
        solver.call()
        assert(solver.hasFinished)
        assert(! sigint.isSet)
        assertEx(solver.call())
        assert(solver.hasFinished)
        assert(! sigint.isSet)
    }

    @Test
    def testTimeboxingWithException(): Unit = {
        val solver = new TimeboxedSolver(new BadSolver, 1, logger, sigint)
        assertEx(solver.call(), classOf[BadSolverException])
        assert(! solver.hasFinished)
        assert(! sigint.isSet)
        assertEx(solver.call(), classOf[BadSolverException])
        assert(! solver.hasFinished)
        assert(! sigint.isSet)
    }

    @Test
    def testOnDemandGenerationWithResult(): Unit = {
        val result = new Result("0", null, null, null)
        val solverGenerator = new SolverGenerator {
            override def solverName = result.solverName
            override def call = new GoodSolver(result, 0, sigint)
        }
        val solver = new OnDemandGeneratedSolver(solverGenerator, logger, sigint)
        assert(! solver.hasFinished)
        assert(! sigint.isSet)
        assertEq(solver.call().solverName, result.solverName)
        assert(solver.hasFinished)
        assert(! sigint.isSet)
        assertEx(solver.call())
        assert(solver.hasFinished)
        assert(! sigint.isSet)
    }

    @Test
    def testOnDemandGenerationWithException(): Unit = {
        val solverGenerator = new SolverGenerator {
            override def solverName = classOf[BadSolver].getSimpleName
            override def call = new BadSolver
        }
        val solver = new OnDemandGeneratedSolver(solverGenerator, logger, sigint)
        assertEx(solver.call(), classOf[BadSolverException])
        assert(! solver.hasFinished)
        assert(! sigint.isSet)
        assertEx(solver.call(), classOf[BadSolverException])
        assert(! solver.hasFinished)
        assert(! sigint.isSet)
    }

    @Test
    def testParallelSolvingWithSolution(): Unit = {
        val sigint = new RevocableSigint
        val space = new Space(logger, sigint)
        val x = new IntegerVariable(space.nextVariableId, "x", NonNegativeIntegerRange)
        val objective = new MinimizationObjective(x, Some(Zero), None)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        results.indices.foreach(i => results(i).costsOfBestProposal = if (i == 8) Zero else One)
        val solvers = results.map(result => new GoodSolver(result, 100, sigint))
        val solver = new ParallelSolver(solvers, 4, "Test", logger, sigint)
        val result = solver.call()
        assertEq(result.solverName, "8")
        assertEq(result.costsOfBestProposal, Zero)
        assert(result.isSolution)
        assert(solvers(8).hasFinished)
        for (i <- 16 until solvers.size) {
            val solver = solvers(i)
            assert(! solver.hasFinished)
        }
        assert(solver.hasFinished)
        assert(sigint.isSet)
        sigint.revoke()
        assertEx(solver.call())
        assert(solver.hasFinished)
        assert(! sigint.isSet)
    }

    @Test
    def testParallelSolvingWithoutSolution(): Unit = {
        val sigint = new RevocableSigint
        val space = new Space(logger, sigint)
        val x = new IntegerVariable(space.nextVariableId, "x", NonNegativeIntegerRange)
        val objective = new MinimizationObjective(x, Some(Zero), None)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        results.indices.foreach(i => results(i).costsOfBestProposal = One)
        val solvers = results.map(result => new GoodSolver(result, 0, sigint))
        val solver = new ParallelSolver(solvers, 4, "Test", logger, sigint)
        val result = solver.call()
        assertEq(result.costsOfBestProposal, One)
        assert(! result.isSolution)
        for (solver <- solvers) {
            assert(solver.hasFinished)
        }
        assert(solver.hasFinished)
        assert(sigint.isSet)
        sigint.revoke()
        assertEx(solver.call())
        assert(solver.hasFinished)
        assert(! sigint.isSet)
    }

    @Test
    def testParallelSolvingWithException(): Unit = {
        val sigint = new RevocableSigint
        val space = new Space(logger, sigint)
        val x = new IntegerVariable(space.nextVariableId, "x", NonNegativeIntegerRange)
        val objective = new MinimizationObjective(x, Some(Zero), None)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        results.indices.foreach(i => results(i).costsOfBestProposal = One)
        val solvers = results.indices.map(i => if (i == 8) new BadSolver else new GoodSolver(results(i), 100, sigint))
        val solver = new ParallelSolver(solvers, 4, "Test", logger, sigint)
        assertEx(solver.call(), classOf[BadSolverException])
        for (i <- 16 until solvers.size) {
            val solver = solvers(i)
            assert(! solver.hasFinished)
        }
        assert(! solver.hasFinished)
        assert(sigint.isSet)
        sigint.revoke()
        assertEx(solver.call(), classOf[BadSolverException])
        assert(! solver.hasFinished)
        assert(sigint.isSet)
    }

    // Tests on-demand solver generation, parallel solving, timeboxing, and resumption after timeout
    @Test
    def testPracticalSetting(): Unit = {
        val sigint = new RevocableSigint
        val space = new Space(logger, sigint)
        val x = new IntegerVariable(space.nextVariableId, "x", NonNegativeIntegerRange)
        val objective = new MinimizationObjective(x, Some(Zero), None)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        results.indices.foreach(i => results(i).costsOfBestProposal = if (i == 8) Zero else One)
        class GoodSolverGenerator(result: Result) extends SolverGenerator {
            override def solverName = result.solverName
            override def call = new GoodSolver(result, 100, sigint)
        }
        val solvers =
            results.map(result => new OnDemandGeneratedSolver(new GoodSolverGenerator(result), logger, sigint))
        val solver = new ParallelSolver(solvers, 4, "Test", logger, sigint)
        assertEx(new TimeboxedSolver(solver, 0, logger, sigint).call(), classOf[SolverInterruptedException])
        assert(! solver.hasFinished)
        assert(sigint.isSet)
        sigint.revoke()
        val result = new TimeboxedSolver(solver, 8, logger, sigint).call()
        assertEq(result.solverName, "8")
        assertEq(result.costsOfBestProposal, Zero)
        assert(result.isSolution)
        assert(solvers(8).hasFinished)
        for (i <- 16 until solvers.size) {
            val solver = solvers(i)
            assert(! solver.hasFinished)
        }
        assert(solver.hasFinished)
        assert(sigint.isSet)
        assertEx(solver.call())
        assert(solver.hasFinished)
        assert(sigint.isSet)
    }

}
