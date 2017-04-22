package yuck.annealing.test

import org.junit._

import scala.collection._

import yuck.annealing._
import yuck.core._
import yuck.util.testing.IntegrationTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class ProgressiveTighteningTest extends IntegrationTest {

    // To test progressive tightening of objectives, we use:
    // - two variables x and y with d(x) = d(y) = [0, 9]
    // - a dummy constraint over x and y (to turn both into search variables)
    // - a neighbourhood over x (so y's value can only be changed by tightening)
    // - a monitor that counts the number of tightening events
    // Minimizing or maximizing (x, y), we expect one tightening event.

    val space = new Space(logger)
    val d = new IntegerDomain(Zero, Nine)
    val x = space.createVariable("x", d)
    val y = space.createVariable("y", d)
    val randomGenerator = new JavaRandomGenerator(DEFAULT_SEED)
    val tighteningCounter = new TighteningCounter(y)
    space.post(new DummyConstraint(space.constraintIdFactory.nextId, List(x, y), Nil))

    final class TighteningCounter(val y: AnyVariable) extends StandardAnnealingMonitor(logger) {
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
            None)
    }

    @Test
    def testMinimizationWithProgressiveTightening {
        space.setValue(x, Nine).setValue(y, Nine)
        val objective =
            new HierarchicalObjective(
                List(new MinimizationObjective(x, Zero, None), new MinimizationObjective(y, Zero, Some(MinusOne))),
                false)
        val solver = createSolver(objective)
        val maybeResult = solver.call
        assert(maybeResult.isDefined)
        assert(maybeResult.get.isGoodEnough)
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
        val maybeResult = solver.call
        assert(maybeResult.isDefined)
        assert(maybeResult.get.isGoodEnough)
        assertEq(tighteningCounter.n, 1)
    }

}
