package yuck.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.annealing.*
import yuck.constraints.*
import yuck.core.*
import yuck.test.util.{DefaultNumberOfThreads, IntegrationTest}

/**
 * The classic n-queens problem
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[runners.Parameterized])
final class Queens(val n: Int) extends IntegrationTest {

    override protected val logToConsole = false

    private final class QueensGenerator(n: Int, i: Int, seed: Int) extends SolverGenerator {
        override def solverName = "SA-%d".format(i)
        override def call() = {

            // define problem
            val space = new Space(logger, sigint)
            val d = IntegerRange(Zero, new IntegerValue(n - 1))
            val rows = new Array[IntegerVariable](n)
            val rowsMinusI = new Array[IntegerVariable](n)
            val rowsPlusI = new Array[IntegerVariable](n)
            for (col <- 0 to (n - 1)) {
                rows.update(col, new IntegerVariable(space.nextVariableId, "row[%s]".format(col) , d))
                rowsMinusI.update(col, new IntegerVariable(space.nextVariableId, "row[%s] - %s".format(col, col), CompleteIntegerRange))
                rowsPlusI.update(col, new IntegerVariable(space.nextVariableId, "row[%s] + %s".format(col, col), CompleteIntegerRange))
                val iVal = new IntegerValue(col)
                val iVar = new IntegerVariable(space.nextVariableId, col.toString, IntegerRange(iVal, iVal))
                space.setValue(iVar, new IntegerValue(col))
                space.post(new Minus(space.nextConstraintId, null, rows(col), iVar, rowsMinusI(col)))
                space.post(new Plus(space.nextConstraintId, null, rows(col), iVar, rowsPlusI(col)))
            }
            val rowConflicts = new BooleanVariable(space.nextVariableId, "rowConflicts", CompleteBooleanDomain)
            val rowConstraint = new Alldistinct(space.nextConstraintId, null, rows.toIndexedSeq, rowConflicts)
            space.post(rowConstraint)
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
            val Some(neighbourhood) =
                rowConstraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint)
            space.markAsImplicit(rowConstraint)
            val solver =
                new SimulatedAnnealing(
                    solverName,
                    space,
                    createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen()),
                    neighbourhood,
                    randomGenerator.nextGen(),
                    new SatisfactionObjective(conflicts),
                    None,
                    Some(new StandardAnnealingMonitor(logger)),
                    None,
                    sigint)

            solver
        }
    }

    @Test
    def queens: Unit = {
        val randomGenerator = new JavaRandomGenerator(29071972)
        val solvers =
            (1 to DefaultRestartLimit).map(
                i => new OnDemandGeneratedSolver(new QueensGenerator(n, i, randomGenerator.nextInt()), logger, sigint))
        val solver = new ParallelSolver(solvers, DefaultNumberOfThreads, "Queens", logger, sigint)
        val result = solver.call()
        assert(result.isSolution)
    }

}

/**
 * @author Michael Marte
 *
 */
object Queens {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(8, 16, 32, 64, 128).asJava

}
