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
final class SendMostMoney extends IntegrationTest {

    private final class ModelData(
        val LHS: List[(Int, Variable[IntegerValue])],
        val RHS: List[(Int, Variable[IntegerValue])])

    private final class SendMostMoneyGenerator(i: Int, seed: Int, sigint: Sigint) extends SolverGenerator {
        override def solverName = "SA-%d".format(i)
        override def call = {
            val space = new Space(logger)
            val d = new IntegerRange(Zero, Nine)
            val d1 = new IntegerRange(One, Nine)
            val S = space.createVariable("S", d1)
            val E = space.createVariable("E", d)
            val N = space.createVariable("N", d)
            val D = space.createVariable("D", d)
            val M = space.createVariable("M", d1)
            val O = space.createVariable("O", d)
            val T = space.createVariable("T", d)
            val Y = space.createVariable("Y", d)
            val numberOfMissingValues = space.createVariable("numberOfMissingValues", CompleteBooleanDomain)
            space.post(
                new Alldistinct(
                    space.constraintIdFactory.nextId, null,
                    Set(S, E, N, D, M, O, S, T, M, O, N, E, Y).toVector, numberOfMissingValues))
            val LHS = List((1000, S), (100, E), (10, N), (1, D), (1000, M), (100, O), (10, S), (1, T))
            val RHS = List((10000, M), (1000, O), (100, N), (10, E), (1, Y))
            val lhs = space.createVariable("lhs", CompleteIntegerRange)
            space.post(
                new LinearCombination(
                    space.constraintIdFactory.nextId,
                    null,
                    LHS.map{case (a, x) => new AX(new IntegerValue(a), x)},
                    lhs))
            val rhs = space.createVariable("rhs", CompleteIntegerRange)
            space.post(
                new LinearCombination(
                    space.constraintIdFactory.nextId,
                    null,
                    RHS.map{case (a, x) => new AX(new IntegerValue(a), x)},
                    rhs))
            val delta = space.createVariable("delta", CompleteBooleanDomain)
            space.post(new Eq(space.constraintIdFactory.nextId, null, lhs, rhs, delta))
            val costs = space.createVariable("costs", CompleteBooleanDomain)
            space.post(
                new LinearCombination(
                    space.constraintIdFactory.nextId, null,
                    new AX(new BooleanValue(100), numberOfMissingValues) :: new AX(False, delta) :: Nil, costs))
            assertEq(space.searchVariables, Set(S, E, N, D, M, O, S, T, M, O, N, E, Y))
            val randomGenerator = new JavaRandomGenerator(seed)
            val initializer = new RandomInitializer(space, randomGenerator.nextGen)
            initializer.run
            val solver =
                new SimulatedAnnealing(
                    solverName,
                    space,
                    createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen),
                    new SimpleRandomReassignmentGenerator(space, space.searchVariables.toIndexedSeq, randomGenerator.nextGen),
                    randomGenerator.nextGen,
                    // cf. http://gecoder.rubyforge.org/examples/send-most-money.html
                    new HierarchicalObjective(
                        List(new MinimizationObjective(costs, True, None),
                             new MaximizationObjective(rhs, new IntegerValue(10876), None)),
                        false),
                    None,
                    Some(new StandardAnnealingMonitor(logger)),
                    Some(new ModelData(LHS, RHS)),
                    sigint)
            solver
        }
    }

    @Test
    def sendMostMoney {
        val randomGenerator = new JavaRandomGenerator(29071972)
        val sigint = new SettableSigint
        val solvers =
            (1 to DEFAULT_RESTART_LIMIT).toList.map(
                i => new OnDemandGeneratedSolver(new SendMostMoneyGenerator(i, randomGenerator.nextInt, sigint), logger, sigint))
        val solver = new ParallelSolver(solvers, Runtime.getRuntime.availableProcessors, "SendMostMoney", logger, sigint)
        val result = solver.call
        if (result.isSolution) {
            val modelData = result.maybeUserData.get.asInstanceOf[ModelData]
            assertEq(
                modelData.LHS. /:(0){case (y, (a, x)) => y + a * result.bestProposal.value(x).value},
                modelData.RHS. /:(0){case (y, (a, x)) => y + a * result.bestProposal.value(x).value})
        }
        assert(result.isSolution)
    }

}
