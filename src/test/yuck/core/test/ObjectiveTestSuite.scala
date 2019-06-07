package yuck.core.test

import org.junit._

/**
  * @author Michael Marte
  *
  */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[MinimizationObjectiveTest],
        classOf[MaximizationObjectiveTest],
        classOf[HierarchicalObjectiveTest]))
final class ObjectiveTestSuite {
}
