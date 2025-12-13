package yuck.test


import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.constraints.*
import yuck.core.*
import yuck.test.util.DefaultNumberOfThreads
import yuck.{SolvingMethod, annealing}

/**
 * The classic n-queens problem
 */
@ParameterizedClass
@MethodSource(Array("parameters"))
final class Queens(val n: Int, solvingMethod: SolvingMethod) extends HelloWorldTest {

    private final class QueensGenerator(n: Int, i: Int, seed: Int) extends SolverGenerator {
        override def solverName = "%s-%d".format(solvingMethod, i)
        override def call() = {

            // define problem
            val space = new Space(logger, sigint)
            val d = IntegerRange(0, n - 1)
            val rows = new Array[IntegerVariable](n)
            val rowsMinusI = new Array[IntegerVariable](n)
            val rowsPlusI = new Array[IntegerVariable](n)
            for col <- 0 until n do {
                rows.update(col, new IntegerVariable(space.nextVariableId(), "row[%s]".format(col) , d))
                rowsMinusI.update(col, new IntegerVariable(space.nextVariableId(), "row[%s] - %s".format(col, col), CompleteIntegerRange))
                rowsPlusI.update(col, new IntegerVariable(space.nextVariableId(), "row[%s] + %s".format(col, col), CompleteIntegerRange))
                val iVal = new IntegerValue(col)
                val iVar = new IntegerVariable(space.nextVariableId(), col.toString, IntegerRange(iVal, iVal))
                space.setValue(iVar, new IntegerValue(col))
                space.post(new Minus(space.nextConstraintId(), null, rows(col), iVar, rowsMinusI(col)))
                space.post(new Plus(space.nextConstraintId(), null, rows(col), iVar, rowsPlusI(col)))
            }
            val rowConflicts = new BooleanVariable(space.nextVariableId(), "rowConflicts", CompleteBooleanDomain)
            val rowConstraint = new AllDifferent(space.nextConstraintId(), null, rows.toVector, Set(), rowConflicts, logger)
            space.post(rowConstraint)
            val diagonalConflicts1 = new BooleanVariable(space.nextVariableId(), "diagonalConflicts1", CompleteBooleanDomain)
            space.post(new AllDifferent(space.nextConstraintId(), null, rowsMinusI.toVector, Set(), diagonalConflicts1, logger))
            val diagonalConflicts2 = new BooleanVariable(space.nextVariableId(), "diagonalConflicts2", CompleteBooleanDomain)
            space.post(new AllDifferent(space.nextConstraintId(), null, rowsPlusI.toVector, Set(), diagonalConflicts2, logger))
            val conflicts = new BooleanVariable(space.nextVariableId(), "conflicts", CompleteBooleanDomain)
            space.post(
                new Conjunction(
                    space.nextConstraintId(),
                    null,
                    List(rowConflicts, diagonalConflicts1, diagonalConflicts2),
                    conflicts))
            space.registerObjectiveVariable(conflicts)

            // build local-search solver
            val objective = new SatisfactionObjective(conflicts)
            val randomGenerator = new JavaRandomGenerator(seed)
            val solver = solvingMethod match {
                case SolvingMethod.SimulatedAnnealing =>
                    val Some(neighbourhood) =
                        rowConstraint.createNeighbourhood(
                            space, randomGenerator, annealing.DefaultMoveSizeDistribution): @unchecked
                    space.registerImplicitConstraint(rowConstraint)
                    createSimulatedAnnealingSolver(
                        solverName, space, objective, neighbourhood, randomGenerator.nextGen(), None)
                case SolvingMethod.FeasibilityJump =>
                    val initializer = new RandomInitializer(space, randomGenerator.nextGen())
                    initializer.run()
                    val xs = rows.toVector
                    val cs = Vector(rowConflicts, diagonalConflicts1, diagonalConflicts2)
                    createFeasibilityJumpSolver(solverName, space, xs, cs, objective, randomGenerator.nextGen(), None)
            }

            solver
        }
    }

    @Test
    def queens(): Unit = {
        val randomGenerator = new JavaRandomGenerator(29071972)
        val solvers =
            (1 to DefaultNumberOfThreads).map(
                i => new OnDemandGeneratedSolver(new QueensGenerator(n, i, randomGenerator.nextInt()), logger, sigint))
        val solver = new ParallelSolver(solvers, DefaultNumberOfThreads, "Queens", logger, sigint)
        val result = solver.call()
        assert(sigint.isSet || result.isSolution)
    }

}

object Queens {

    private val configurations =
        for n <- List(8, 16, 32, 64, 128)
            solvingMethod <- SolvingMethod.values
        yield
            Array(n, solvingMethod)

    def parameters = configurations.toArray

}
