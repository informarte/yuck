package yuck.annealing.test

import org.junit._

import scala.jdk.CollectionConverters._

import yuck.annealing._
import yuck.core._
import yuck.util.testing.IntegrationTest


/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class ProgressiveTighteningTest
    (mainObjectiveType: ProgressiveTighteningTest.ObjectiveType,
     subordinateObjectiveType: ProgressiveTighteningTest.ObjectiveType,
     propagateBound: Boolean)
    extends IntegrationTest
{

    import ProgressiveTighteningTest._

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
    def testProgressiveTightening: Unit = {
        val space = new Space(logger, sigint)
        val baseDomain = new IntegerRange(Zero, Nine)
        val x = new IntegerVariable(space.nextVariableId, "x", baseDomain)
        val y = new IntegerVariable(space.nextVariableId, "y", baseDomain)
        val randomGenerator = new JavaRandomGenerator
        val tighteningCounter = new TighteningCounter(y)
        space.post(new DummyConstraint(space.nextConstraintId, List(x, y), Nil))
        val mainObjective = mainObjectiveType match {
            case Min =>
                space.setValue(x, Nine)
                new MinimizationObjective(x, Some(Zero), None)
            case Max =>
                space.setValue(x, Zero)
                new MaximizationObjective(x, Some(Nine), None)
        }
        val subordinateObjective = subordinateObjectiveType match {
            case Min =>
                space.setValue(y, Nine)
                new MinimizationObjective(y, Some(Zero), Some(MinusOne))
            case Max =>
                space.setValue(y, Zero)
                new MaximizationObjective(y, Some(Nine), Some(One))
        }
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false)
        val solver =
            new SimulatedAnnealing(
                "SA",
                space,
                createAnnealingSchedule(space.searchVariables.size, randomGenerator.nextGen),
                new RandomReassignmentGenerator(
                    space, Vector(x), randomGenerator.nextGen, DefaultMoveSizeDistribution, None, None),
                randomGenerator.nextGen,
                objective,
                Some(1),
                Some(tighteningCounter),
                None,
                propagateBound,
                sigint)
        val result = solver.call
        assert(result.isGoodEnough)
        assertEq(tighteningCounter.n, 1)
        assertEq(space.numberOfPropagations, if (propagateBound) 1 else 0)
    }

}

/**
  * @author Michael Marte
  *
  */
object ProgressiveTighteningTest {

    trait ObjectiveType
    case object Min extends ObjectiveType
    case object Max extends ObjectiveType

    private def configurations =
        for (mainObjectiveType <- List(Min, Max);
             subordinateObjectiveType <- List(Min, Max);
             propagateBound <- List(true, false))
            yield Vector(mainObjectiveType, subordinateObjectiveType, propagateBound)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}, {2}")
    def parameters = configurations.map(_.toArray).asJava

}
