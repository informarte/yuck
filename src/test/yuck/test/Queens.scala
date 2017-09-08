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

    private final class QueensGenerator(n: Int, i: Int, seed: Int, sigint: Sigint) extends SolverGenerator {
        override def solverName = "SA-%d".format(i)
        override def call = {
            val space = new Space(logger)
            val d = new IntegerDomain(Zero, new IntegerValue(n - 1))
            var rows = new Array[Variable[IntegerValue]](n)
            var rowsMinusI = new Array[Variable[IntegerValue]](n)
            var rowsPlusI = new Array[Variable[IntegerValue]](n)
            for (col <- 0 to (n - 1)) {
                rows.update(col, space.createVariable("row[%s]".format(col) , d))
                rowsMinusI.update(col, space.createVariable("row[%s] - %s".format(col, col), UnboundedIntegerDomain))
                rowsPlusI.update(col, space.createVariable("row[%s] + %s".format(col, col), UnboundedIntegerDomain))
                val iVal = new IntegerValue(col)
                val iVar = space.createVariable(col.toString, new IntegerDomain(iVal, iVal))
                space.setValue(iVar, new IntegerValue(col))
                space.post(new Minus(space.constraintIdFactory.nextId, null, rows.apply(col), iVar, rowsMinusI.apply(col)))
                space.post(new Plus(space.constraintIdFactory.nextId, null, rows.apply(col), iVar, rowsPlusI.apply(col)))
            }
            val rowConflicts = space.createVariable("rowConflicts", UnboundedIntegerDomain)
            space.post(new Alldistinct(space.constraintIdFactory.nextId, null, rows.toIndexedSeq, rowConflicts))
            val diagonalConflicts1 = space.createVariable("diagonalConflicts1", UnboundedIntegerDomain)
            space.post(new Alldistinct(space.constraintIdFactory.nextId, null, rowsMinusI.toIndexedSeq, diagonalConflicts1))
            val diagonalConflicts2 = space.createVariable("diagonalConflicts2", UnboundedIntegerDomain)
            space.post(new Alldistinct(space.constraintIdFactory.nextId, null, rowsPlusI.toIndexedSeq, diagonalConflicts2))
            val conflicts = space.createVariable("conflicts", UnboundedIntegerDomain)
            space.post(
                new LinearCombination(
                    space.constraintIdFactory.nextId,
                    null,
                    List(rowConflicts, diagonalConflicts1, diagonalConflicts2).map(new AX(One, _)),
                    conflicts))
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
                        space, rows.toIndexedSeq, randomGenerator.nextGen, DEFAULT_MOVE_SIZE_DISTRIBUTION, None, 0),
                    randomGenerator.nextGen,
                    new MinimizationObjective(conflicts, Zero, None),
                    None,
                    None,
                    None,
                    sigint)
            solver
        }
    }

    private def queens(n: Int) {
        val space = new Space(logger)
        val randomGenerator = new JavaRandomGenerator(29071972)
        val sigint = new SettableSigint
        val solvers =
            (1 to DEFAULT_RESTART_LIMIT).map(
                i => new OnDemandGeneratedSolver(new QueensGenerator(n, i, randomGenerator.nextInt, sigint), logger, sigint))
        val solver = new ParallelSolver(solvers, Runtime.getRuntime.availableProcessors, "Queens", logger, sigint)
        val result = solver.call
        assert(result.isSolution)
    }

    @Test
    def queens00008() {
        queens(8)
    }

    @Test
    def queens00016() {
        queens(16)
    }

    @Test
    def queens00032() {
        queens(32)
    }

    @Test
    def queens00064() {
        queens(64)
    }

    @Test
    def queens00128() {
        queens(128)
    }

    @Test
    def queens00256() {
        queens(256)
    }

    @Test
    @Ignore
    def queens00512() {
        queens(512)
    }

    @Test
    @Ignore
    def queens01024() {
        queens(1024)
    }

    @Test
    @Ignore
    def queens02048() {
        queens(2048)
    }

    @Test
    @Ignore
    def queens04096() {
        queens(4096)
    }

    @Test
    @Ignore
    def queens08192() {
        queens(8192)
    }

    @Test
    @Ignore
    def queens16384() {
        queens(16384)
    }

    @Test
    @Ignore
    def queens32768() {
        queens(32768)
    }

}
