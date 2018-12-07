package yuck.test

import org.junit._

import yuck.annealing._
import yuck.constraints._
import yuck.core._
import yuck.util.arm.{SettableSigint, Sigint}
import yuck.util.testing.IntegrationTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SendMoreMoney extends IntegrationTest {

    private final class ModelData(
        val LHS: List[(Int, Variable[IntegerValue])],
        val RHS: List[(Int, Variable[IntegerValue])])

    private final class SendMoreMoneyGenerator(i: Int, seed: Int, sigint: Sigint) extends SolverGenerator {
        override def solverName = "SA-%d".format(i)
        override def call = {

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
            val space = new Space(logger)
            val sigint = new SettableSigint
            val d = new IntegerRange(Zero, Nine)
            val d1 = new IntegerRange(One, Nine)
            val S = space.createVariable("S", d1)
            val E = space.createVariable("E", d)
            val N = space.createVariable("N", d)
            val D = space.createVariable("D", d)
            val M = space.createVariable("M", d1)
            val O = space.createVariable("O", d)
            val R = space.createVariable("R", d)
            val Y = space.createVariable("Y", d)
            val vars = Set(S, E, N, D, M, O, R, E, M, O, N, E, Y)
            val numberOfMissingValues = space.createVariable("numberOfMissingValues", CompleteBooleanDomain)
            space.post(
                new Alldistinct(
                    space.constraintIdFactory.nextId, null, Vector(S, E, N, D, M, O, R, Y), numberOfMissingValues))
            val LHS = List((1000, S), (100, E), (10, N), (1, D), (1000, M), (100, O), (10, R), (1, E))
            val RHS = List((10000, M), (1000, O), (100, N), (10, E), (1, Y))
            val lhs = space.createVariable("lhs", CompleteIntegerRange)
            space.post(
                new LinearCombination(
                    space.constraintIdFactory.nextId,
                    null,
                    LHS.map { case (a, x) => new AX(new IntegerValue(a), x) },
                    lhs))
            val rhs = space.createVariable("rhs", CompleteIntegerRange)
            space.post(
                new LinearCombination(
                    space.constraintIdFactory.nextId,
                    null,
                    RHS.map { case (a, x) => new AX(new IntegerValue(a), x) },
                    rhs))
            val delta = space.createVariable("delta", CompleteBooleanDomain)
            space.post(new Eq(space.constraintIdFactory.nextId, null, lhs, rhs, delta))
            val costs = space.createVariable("costs", CompleteBooleanDomain)
            space.post(
                new LinearCombination(
                    space.constraintIdFactory.nextId, null,
                    new AX(new BooleanValue(100), numberOfMissingValues) :: new AX(False, delta) :: Nil, costs))
            assertEq(space.searchVariables, vars)

            // propagate constraints
            costs.pruneDomain(TrueDomain)
            do {} while (space.prune)
            assertEq(space.searchVariables, vars -- Set(M, O, S))
            assertEq(delta.domain, TrueDomain)
            assertEq(numberOfMissingValues.domain, TrueDomain)
            assertEq(S.domain.singleValue, Nine)
            assertEq(E.domain, new IntegerRange(Two, Eight))
            assertEq(N.domain, E.domain)
            assertEq(D.domain, E.domain)
            assertEq(M.domain.singleValue, One)
            assertEq(O.domain.singleValue, Zero)
            assertEq(R.domain, E.domain)
            assertEq(Y.domain, E.domain)
            assertEq(lhs.domain, new IntegerRange(IntegerValue.get(10244), IntegerValue.get(10888)))

            // build local-search solver
            for (x <- vars if x.domain.isSingleton) {
                space.setValue(x, x.domain.singleValue)
            }
            val randomGenerator = new JavaRandomGenerator(seed)
            val initializer = new RandomInitializer(space, randomGenerator.nextGen)
            initializer.run
            val solver =
                new SimulatedAnnealing(
                    "SA",
                    space,
                    createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen),
                    new SimpleRandomReassignmentGenerator(space, space.searchVariables.toIndexedSeq, randomGenerator.nextGen),
                    randomGenerator.nextGen,
                    new MinimizationObjective(costs, True, None),
                    None,
                    Some(new StandardAnnealingMonitor(logger)),
                    Some(new ModelData(LHS, RHS)),
                    sigint)

            solver
        }

    }

    @Test
    def sendMoreMoney {
        val randomGenerator = new JavaRandomGenerator(29071972)
        val sigint = new SettableSigint
        val solvers =
            (1 to DEFAULT_RESTART_LIMIT).toList.map(
                i => new OnDemandGeneratedSolver(new SendMoreMoneyGenerator(i, randomGenerator.nextInt, sigint), logger, sigint))
        val solver = new ParallelSolver(solvers, Runtime.getRuntime.availableProcessors, "SendMoreMoney", logger, sigint)
        val result = solver.call
        if (result.isSolution) {
            val modelData = result.maybeUserData.get.asInstanceOf[ModelData]
            assertEq(
                modelData.LHS./:(0){case (y, (a, x)) => y + a * result.bestProposal.value(x).value},
                modelData.RHS./:(0){case (y, (a, x)) => y + a * result.bestProposal.value(x).value})
        }
        assert(result.isSolution)
    }

}
