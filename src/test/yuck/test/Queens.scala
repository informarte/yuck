package yuck.test

import org.junit._

import yuck.annealing._
import yuck.constraints._
import yuck.core._
import yuck.util.testing.IntegrationTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class Queens extends IntegrationTest {

    private final class QueensGenerator(n: Int, i: Int, seed: Int) extends SolverGenerator {
        override def solverName = "SA-%d".format(i)
        override def call = {

            // define problem
            val space = new Space(logger, sigint)
            val d = new IntegerRange(Zero, new IntegerValue(n - 1))
            var rows = new Array[IntegerVariable](n)
            var rowsMinusI = new Array[IntegerVariable](n)
            var rowsPlusI = new Array[IntegerVariable](n)
            for (col <- 0 to (n - 1)) {
                rows.update(col, new IntegerVariable(space.nextVariableId, "row[%s]".format(col) , d))
                rowsMinusI.update(col, new IntegerVariable(space.nextVariableId, "row[%s] - %s".format(col, col), CompleteIntegerRange))
                rowsPlusI.update(col, new IntegerVariable(space.nextVariableId, "row[%s] + %s".format(col, col), CompleteIntegerRange))
                val iVal = new IntegerValue(col)
                val iVar = new IntegerVariable(space.nextVariableId, col.toString, new IntegerRange(iVal, iVal))
                space.setValue(iVar, new IntegerValue(col))
                space.post(new Minus(space.nextConstraintId, null, rows.apply(col), iVar, rowsMinusI.apply(col)))
                space.post(new Plus(space.nextConstraintId, null, rows.apply(col), iVar, rowsPlusI.apply(col)))
            }
            val rowConflicts = new BooleanVariable(space.nextVariableId, "rowConflicts", CompleteBooleanDomain)
            space.post(new Alldistinct(space.nextConstraintId, null, rows.toIndexedSeq, rowConflicts))
            val diagonalConflicts1 = new BooleanVariable(space.nextVariableId, "diagonalConflicts1", CompleteBooleanDomain)
            space.post(new Alldistinct(space.nextConstraintId, null, rowsMinusI.toIndexedSeq, diagonalConflicts1))
            val diagonalConflicts2 = new BooleanVariable(space.nextVariableId, "diagonalConflicts2", CompleteBooleanDomain)
            space.post(new Alldistinct(space.nextConstraintId, null, rowsPlusI.toIndexedSeq, diagonalConflicts2))
            val conflicts = new BooleanVariable(space.nextVariableId, "conflicts", CompleteBooleanDomain)
            space.post(
                new Conjunction(
                    space.nextConstraintId,
                    null,
                    List(rowConflicts, diagonalConflicts1, diagonalConflicts2),
                    conflicts))

            // build local-search solver
            val randomGenerator = new JavaRandomGenerator(seed)
            for ((x, a) <- rows.zip(0 to n - 1)) {
                space.setValue(x, IntegerValue.get(a))
            }
            val solver =
                new SimulatedAnnealing(
                    solverName,
                    space,
                    createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen),
                    new RandomCircularSwapGenerator(
                        space, rows.toIndexedSeq, randomGenerator.nextGen, DefaultMoveSizeDistribution, None, None),
                    randomGenerator.nextGen,
                    new SatisfactionObjective(conflicts),
                    None,
                    Some(new StandardAnnealingMonitor(logger)),
                    None,
                    false,
                    sigint)

            solver
        }
    }

    private def queens(n: Int): Unit = {
        val randomGenerator = new JavaRandomGenerator(29071972)
        val solvers =
            (1 to DefaultRestartLimit).map(
                i => new OnDemandGeneratedSolver(new QueensGenerator(n, i, randomGenerator.nextInt), logger, sigint))
        val solver = new ParallelSolver(solvers, Runtime.getRuntime.availableProcessors, "Queens", logger, sigint)
        val result = solver.call
        assert(result.isSolution)
    }

    @Test
    def queens00008: Unit = {
        queens(8)
    }

    @Test
    def queens00016: Unit = {
        queens(16)
    }

    @Test
    def queens00032: Unit = {
        queens(32)
    }

    @Test
    def queens00064: Unit = {
        queens(64)
    }

    @Test
    def queens00128: Unit = {
        queens(128)
    }

    @Test
    def queens00256: Unit = {
        queens(256)
    }

    @Test
    @Ignore
    def queens00512: Unit = {
        queens(512)
    }

    @Test
    @Ignore
    def queens01024: Unit = {
        queens(1024)
    }

    @Test
    @Ignore
    def queens02048: Unit = {
        queens(2048)
    }

    @Test
    @Ignore
    def queens04096: Unit = {
        queens(4096)
    }

    @Test
    @Ignore
    def queens08192: Unit = {
        queens(8192)
    }

    @Test
    @Ignore
    def queens16384: Unit = {
        queens(16384)
    }

    @Test
    @Ignore
    def queens32768: Unit = {
        queens(32768)
    }

}
