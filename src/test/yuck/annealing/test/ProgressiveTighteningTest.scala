package yuck.annealing.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.annealing.*
import yuck.core.{given, *}
import yuck.test.util.IntegrationTest


/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class ProgressiveTighteningTest
    (mainObjectiveType: OptimizationMode,
     subordinateObjectiveType: OptimizationMode)
    extends IntegrationTest
{

    private final class TighteningCounter(val y: AnyVariable) extends StandardAnnealingMonitor(logger) {
        var n = 0
        override def onObjectiveTightened(x: AnyVariable) = {
            super.onObjectiveTightened(x)
            assert(x == y)
            n += 1
        }
    }

    // To test progressive tightening of objectives, we use:
    // - two variables x and y with d(x) = d(y) = [0, 9]
    // - a dummy constraint over x and y (to turn both into search variables)
    // - a neighbourhood over x (so y's value can only be changed by tightening)
    // - a monitor that counts the number of tightening events
    @Test
    def testProgressiveTightening(): Unit = {
        val space = new Space(logger, sigint)
        val baseDomain = IntegerRange(Zero, Nine)
        val x = new IntegerVariable(space.nextVariableId(), "x", baseDomain)
        val y = new IntegerVariable(space.nextVariableId(), "y", baseDomain)
        space.post(new DummyConstraint(space.nextConstraintId(), List(x, y), Nil))
        val mainObjective = mainObjectiveType match {
            case OptimizationMode.Min =>
                space.setValue(x, Nine)
                new MinimizationObjective(x, Some(Zero), None)
            case OptimizationMode.Max =>
                space.setValue(x, Zero)
                new MaximizationObjective(x, Some(Nine), None)
        }
        val z = new IntegerVariable(space.nextVariableId(), "z", baseDomain)
        space.post(new DummyConstraint(space.nextConstraintId(), List(y, z), Nil))
        val subordinateObjective = subordinateObjectiveType match {
            case OptimizationMode.Min =>
                space.setValue(y, Nine)
                space.setValue(z, Nine)
                new MinimizationObjective(y, Some(Zero), Some(z))
            case OptimizationMode.Max =>
                space.setValue(y, Zero)
                space.setValue(z, Zero)
                new MaximizationObjective(y, Some(Nine), Some(z))
        }
        val randomGenerator = new JavaRandomGenerator
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        val tighteningCounter = new TighteningCounter(z)
        val solver =
            new SimulatedAnnealing(
                "SA",
                space,
                createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen()),
                new RandomReassignmentGenerator(
                    space, Vector(x), randomGenerator.nextGen(), DefaultMoveSizeDistribution, None, None),
                randomGenerator.nextGen(),
                objective,
                Some(1),
                Some(tighteningCounter),
                None,
                sigint)
        val result = solver.call()
        assert(result.isGoodEnough)
        assert(result.isOptimal)
        assertEq(tighteningCounter.n, 1)
        assertEq(space.numberOfPropagations, 0)
    }

}

/**
  * @author Michael Marte
  *
  */
object ProgressiveTighteningTest {

    private def configurations =
        for (mainObjectiveType <- List(OptimizationMode.Min, OptimizationMode.Max);
             subordinateObjectiveType <- List(OptimizationMode.Min, OptimizationMode.Max))
            yield Vector(mainObjectiveType, subordinateObjectiveType)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
