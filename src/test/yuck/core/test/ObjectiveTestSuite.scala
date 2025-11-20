package yuck.core.test

import org.junit.*

@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[SatisfactionObjectiveTest],
        classOf[MinimizationObjectiveTest],
        classOf[MaximizationObjectiveTest],
        classOf[HierarchicalObjectiveTest]))
final class ObjectiveTestSuite
