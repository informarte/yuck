package yuck.test

import org.junit.*

import yuck.annealing.*
import yuck.constraints.*
import yuck.core.{given, *}
import yuck.test.util.{DefaultNumberOfThreads, IntegrationTest}


/**
 * The classic send-more-money problem
 *
 * @author Michael Marte
 */
final class SendMoreMoney extends IntegrationTest {

    override protected val logToConsole = false

    private val monitor = new StandardAnnealingMonitor(logger)

    private final class ModelData(
        val LHS: List[(Int, IntegerVariable)],
        val RHS: List[(Int, IntegerVariable)])

    private final class SendMoreMoneyGenerator(i: Int, seed: Int) extends SolverGenerator {
        override def solverName = "SA-%d".format(i)
        override def call() = {

            /*
            send_more_money(Vars) :-
                use_module(library(clpfd)),
                Vars = [S,E,N,D,M,O,R,Y],
                Vars ins 0..9,
                LHS #= S * 1000 + E * 100 + N * 10 + D + M * 1000 + O * 100 + R * 10 + E,
                RHS #= M * 10000 + O * 1000 + N * 100 + E * 10 + Y,
                LHS #= RHS,
                M #\= 0, S#\= 0,
                all_different(Vars).
             */

            // define problem
            val space = new Space(logger, sigint)
            val d = IntegerRange(0, 9)
            val d1 = IntegerRange(1, 9)
            val S = new IntegerVariable(space.nextVariableId(), "S", d1)
            val E = new IntegerVariable(space.nextVariableId(), "E", d)
            val N = new IntegerVariable(space.nextVariableId(), "N", d)
            val D = new IntegerVariable(space.nextVariableId(), "D", d)
            val M = new IntegerVariable(space.nextVariableId(), "M", d1)
            val O = new IntegerVariable(space.nextVariableId(), "O", d)
            val R = new IntegerVariable(space.nextVariableId(), "R", d)
            val Y = new IntegerVariable(space.nextVariableId(), "Y", d)
            val vars = Set(S, E, N, D, M, O, R, E, M, O, N, E, Y)
            val numberOfMissingValues =
                new BooleanVariable(space.nextVariableId(), "numberOfMissingValues", CompleteBooleanDomain)
            space.post(
                new AllDifferent(space.nextConstraintId(), null, vars.toVector, Set(), numberOfMissingValues, logger))
            val LHS = List((1000, S), (100, E), (10, N), (1, D), (1000, M), (100, O), (10, R), (1, E))
            val RHS = List((10000, M), (1000, O), (100, N), (10, E), (1, Y))
            val lhs = new IntegerVariable(space.nextVariableId(), "lhs", CompleteIntegerRange)
            space.post(
                new LinearCombination(
                    space.nextConstraintId(),
                    null,
                    AX.normalize(LHS.map((a, x) => new AX(new IntegerValue(a), x))),
                    lhs))
            val rhs = new IntegerVariable(space.nextVariableId(), "rhs", CompleteIntegerRange)
            space.post(
                new LinearCombination(
                    space.nextConstraintId(),
                    null,
                    RHS.map((a, x) => new AX(new IntegerValue(a), x)),
                    rhs))
            val delta = new BooleanVariable(space.nextVariableId(), "delta", CompleteBooleanDomain)
            space.post(new Eq(space.nextConstraintId(), null, lhs, rhs, delta))
            val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)
            space.post(new Conjunction(space.nextConstraintId(), null, List(numberOfMissingValues, delta), costs))
            space.registerObjectiveVariable(costs)
            assertEq(space.searchVariables, vars)

            // propagate constraints
            costs.pruneDomain(TrueDomain)
            space.propagate()
            if (! sigint.isSet) {
                assertEq(space.searchVariables, vars -- Set(M, O, S))
                assertEq(delta.domain, TrueDomain)
                assertEq(numberOfMissingValues.domain, TrueDomain)
                assertEq(S.domain.singleValue, Nine)
                assertEq(E.domain, IntegerRange(2, 8))
                assertEq(N.domain, E.domain)
                assertEq(D.domain, E.domain)
                assertEq(M.domain.singleValue, One)
                assertEq(O.domain.singleValue, Zero)
                assertEq(R.domain, E.domain)
                assertEq(Y.domain, E.domain)
                assertEq(lhs.domain, IntegerRange(10244, 10888))
            }

            // build local-search solver
            for (x <- vars if x.domain.isSingleton) {
                space.setValue(x, x.domain.singleValue)
            }
            val randomGenerator = new JavaRandomGenerator(seed)
            val initializer = new RandomInitializer(space, randomGenerator.nextGen())
            initializer.run()
            val solver =
                new SimulatedAnnealing(
                    "SA",
                    space,
                    createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen()),
                    new SimpleRandomReassignmentGenerator(space, space.searchVariables.toVector, randomGenerator.nextGen()),
                    randomGenerator.nextGen(),
                    new SatisfactionObjective(costs),
                    None,
                    Some(monitor),
                    Some(new ModelData(LHS, RHS)),
                    sigint)

            solver
        }

    }

    @Test
    def sendMoreMoney(): Unit = {
        val randomGenerator = new JavaRandomGenerator(29071972)
        val solvers =
            (1 to DefaultRestartLimit).map(
                i => new OnDemandGeneratedSolver(new SendMoreMoneyGenerator(i, randomGenerator.nextInt()), logger, sigint))
        val solver = new ParallelSolver(solvers, DefaultNumberOfThreads, "SendMoreMoney", logger, sigint)
        val result = solver.call()
        if (result.isSolution) {
            val modelData = result.maybeUserData.get.asInstanceOf[ModelData]
            assertEq(
                modelData.LHS.foldLeft(0){case (y, (a, x)) => y + a * result.bestProposal.value(x).toInt},
                modelData.RHS.foldLeft(0){case (y, (a, x)) => y + a * result.bestProposal.value(x).toInt})
        }
        assert(result.isSolution)
    }

}
