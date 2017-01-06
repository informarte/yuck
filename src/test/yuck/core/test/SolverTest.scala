package yuck.core.test

import org.junit._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class SolverTest extends UnitTest {

    private class GoodSolver(result: Result, sleepTimeInSeconds: Int)
    extends Solver with StandardSolverInterruptionSupport
    {
        override def name = result.solverName
        private var finished = false
        override def hasFinished = finished
        override def call = {
            if (wasInterrupted) {
                None
            } else {
                Thread.sleep(sleepTimeInSeconds * 1000)
                if (wasInterrupted) {
                    None
                } else {
                    finished = true
                    Some(result)
                }
            }
        }
    }

    private class BadSolverException extends RuntimeException

    private class BadSolver extends Solver with StandardSolverInterruptionSupport {
        override def call = {
            throw new BadSolverException
        }
        override def hasFinished = false
    }

    @Test
    def testTimeboxingWithTimeout {
        val result = new Result("0", null, null, null)
        val solver = new TimeboxedSolver(new GoodSolver(result, 1), 0, logger)
        solver.call
        assert(! solver.hasFinished)
        assert(solver.wasInterrupted)
        solver.resume
        assert(! solver.wasInterrupted)
        solver.call
        assert(! solver.hasFinished)
        assert(solver.wasInterrupted)
    }

    @Test
    def testTimeboxingWithoutTimeout {
        val result = new Result("0", null, null, null)
        val solver = new TimeboxedSolver(new GoodSolver(result, 0), 1, logger)
        solver.call
        assert(solver.hasFinished)
        assert(! solver.wasInterrupted)
        assertEx(solver.resume, classOf[IllegalArgumentException])
        assertEx(solver.call, classOf[IllegalArgumentException])
    }

    @Test
    def testTimeboxingWithException {
        val solver = new TimeboxedSolver(new BadSolver, 1, logger)
        assertEx(solver.call, classOf[BadSolverException])
        assert(! solver.hasFinished)
        assert(! solver.wasInterrupted)
        assertEx(solver.resume, classOf[IllegalArgumentException])
        assertEx(solver.call, classOf[BadSolverException])
    }

    @Test
    def testOnDemandGenerationWithResult {
        val result = new Result("0", null, null, null)
        val solverGenerator = new SolverGenerator {
            override def solverName = result.solverName
            override def call = new GoodSolver(result, 0)
        }
        val solver = new OnDemandGeneratedSolver(solverGenerator, logger)
        assert(! solver.hasFinished)
        assert(! solver.wasInterrupted)
        assertEq(solver.call.get.solverName, result.solverName)
        assert(solver.hasFinished)
        assert(! solver.wasInterrupted)
        assertEx(solver.resume, classOf[IllegalArgumentException])
        assertEx(solver.call, classOf[IllegalArgumentException])
    }

    @Test
    def testOnDemandGenerationWithException {
        val solverGenerator = new SolverGenerator {
            override def solverName = classOf[BadSolver].getSimpleName
            override def call = new BadSolver
        }
        val solver = new OnDemandGeneratedSolver(solverGenerator, logger)
        assertEx(solver.call, classOf[BadSolverException])
        assert(! solver.hasFinished)
        assert(! solver.wasInterrupted)
        assertEx(solver.resume, classOf[IllegalArgumentException])
        assertEx(solver.call, classOf[BadSolverException])
    }

    @Test
    def testParallelSolvingWithSolution {
        val objective = new MinimizationObjective(null, Zero)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        (0 until results.size).foreach(i => results(i).costsOfBestProposal = if (i == 8) Zero else One)
        val solvers = results.map(result => new GoodSolver(result, 1))
        val solver = new ParallelSolver(solvers, 4, "Test", logger)
        val maybeResult = solver.call
        assert(maybeResult.isDefined)
        val result = maybeResult.get
        assertEq(result.solverName, "8")
        assertEq(result.costsOfBestProposal, Zero)
        assert(result.isSolution)
        assert(solvers(8).hasFinished)
        for (i <- 16 until solvers.size) {
            val solver = solvers(i)
            assert(solver.wasInterrupted)
            assert(! solver.hasFinished)
        }
        assert(solver.wasInterrupted)
        assert(solver.hasFinished)
        assertEx(solver.resume, classOf[IllegalArgumentException])
        assertEx(solver.call, classOf[IllegalArgumentException])
    }

    @Test
    def testParallelSolvingWithoutSolution {
        val objective = new MinimizationObjective(null, Zero)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        (0 until results.size).foreach(i => results(i).costsOfBestProposal = One)
        val solvers = results.map(result => new GoodSolver(result, 0))
        val solver = new ParallelSolver(solvers, 4, "Test", logger)
        val maybeResult = solver.call
        assert(maybeResult.isDefined)
        val result = maybeResult.get
        assertEq(result.costsOfBestProposal, One)
        assert(! result.isSolution)
        for (solver <- solvers) {
            assert(solver.wasInterrupted)
            assert(solver.hasFinished)
        }
        assert(solver.wasInterrupted)
        assert(solver.hasFinished)
        assertEx(solver.resume, classOf[IllegalArgumentException])
        assertEx(solver.call, classOf[IllegalArgumentException])
    }

    @Test
    def testParallelSolvingWithException {
        val objective = new MinimizationObjective(null, Zero)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        (0 until results.size).foreach(i => results(i).costsOfBestProposal = One)
        val solvers = (0 until results.size).map(i => if (i == 8) new BadSolver else new GoodSolver(results(i), 1))
        val solver = new ParallelSolver(solvers, 4, "Test", logger)
        assertEx(solver.call, classOf[BadSolverException])
        for (i <- 16 until solvers.size) {
            val solver = solvers(i)
            assert(solver.wasInterrupted)
            assert(! solver.hasFinished)
        }
        assert(solver.wasInterrupted)
        assert(! solver.hasFinished)
        solver.resume
        assertEx(solver.call, classOf[BadSolverException])
    }

    // Tests on-demand solver generation, parallel solving, timeboxing, and resumption after timeout
    @Test
    def testPracticalSetting {
        val objective = new MinimizationObjective(null, Zero)
        val results = (0 until 256).map(i => new Result(i.toString, null, objective, null))
        (0 until results.size).foreach(i => results(i).costsOfBestProposal = if (i == 8) Zero else One)
        class GoodSolverGenerator(result: Result) extends SolverGenerator {
            override def solverName = result.solverName
            override def call = new GoodSolver(result, 1)
        }
        val solvers = results.map(result => new OnDemandGeneratedSolver(new GoodSolverGenerator(result), logger))
        val solver = new ParallelSolver(solvers, 4, "Test", logger)
        assertEq(new TimeboxedSolver(solver, 0, logger).call, None)
        assert(solver.wasInterrupted)
        assert(! solver.hasFinished)
        solver.resume
        val maybeResult = new TimeboxedSolver(solver, 8, logger).call
        assert(maybeResult.isDefined)
        val result = maybeResult.get
        assertEq(result.solverName, "8")
        assertEq(result.costsOfBestProposal, Zero)
        assert(result.isSolution)
        assert(solvers(8).hasFinished)
        for (i <- 16 until solvers.size) {
            val solver = solvers(i)
            assert(solver.wasInterrupted)
            assert(! solver.hasFinished)
        }
        assert(solver.wasInterrupted)
        assert(solver.hasFinished)
        assertEx(solver.resume, classOf[IllegalArgumentException])
        assertEx(solver.call, classOf[IllegalArgumentException])
    }

}
