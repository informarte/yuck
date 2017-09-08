package yuck.annealing.test

import org.junit._

import yuck.annealing._
import yuck.core._
import yuck.util.testing.IntegrationTest


/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class ProgressiveTighteningTest extends IntegrationTest {

    // To test progressive tightening of objectives, we use:
    // - two variables x and y with d(x) = d(y) = [0, 9]
    // - a dummy constraint over x and y (to turn both into search variables)
    // - a neighbourhood over x (so y's value can only be changed by tightening)
    // - a monitor that counts the number of tightening events
    // Minimizing or maximizing (x, y), we expect one tightening event.

    private val space = new Space(logger)
    private val d = new IntegerDomain(Zero, Nine)
    private val x = space.createVariable("x", d)
    private val y = space.createVariable("y", d)
    private val randomGenerator = new JavaRandomGenerator(DEFAULT_SEED)
    private val tighteningCounter = new TighteningCounter(y)
    space.post(new DummyConstraint(space.constraintIdFactory.nextId, List(x, y), Nil))

    private final class TighteningCounter(val y: AnyVariable) extends StandardAnnealingMonitor(logger) {
        var n = 0
        override def onObjectiveTightened(x: AnyVariable) {
            super.onObjectiveTightened(x)
            assert(x == y)
            n += 1
        }
    }

    private def createSolver(objective: AnyObjective): Solver = {
        new SimulatedAnnealing(
            "SA",
            space,
            createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen),
            new RandomReassignmentGenerator(
                space, Vector(x), randomGenerator.nextGen, DEFAULT_MOVE_SIZE_DISTRIBUTION, None, 0),
            randomGenerator.nextGen,
            objective,
            Some(1),
            Some(tighteningCounter),
            None,
            new SettableSigint)
    }

    @Test
    def testMinimizationWithProgressiveTightening {
        space.setValue(x, Nine).setValue(y, Nine)
        val objective =
            new HierarchicalObjective(
                List(new MinimizationObjective(x, Zero, None), new MinimizationObjective(y, Zero, Some(MinusOne))),
                false)
        val solver = createSolver(objective)
        val result = solver.call
        assert(result.isGoodEnough)
        assertEq(tighteningCounter.n, 1)
    }

    @Test
    def testMaximizationWithProgressiveTightening {
        space.setValue(x, Zero).setValue(y, Zero)
        val objective =
            new HierarchicalObjective(
                List(new MaximizationObjective(x, Nine, None), new MaximizationObjective(y, Nine, Some(One))),
                false)
        val solver = createSolver(objective)
        val result = solver.call
        assert(result.isGoodEnough)
        assertEq(tighteningCounter.n, 1)
    }

}
