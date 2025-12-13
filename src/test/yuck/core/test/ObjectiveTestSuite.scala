package yuck.core.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

@Suite
@SelectClasses(
    Array(
        classOf[SatisfactionObjectiveTest],
        classOf[MinimizationObjectiveTest],
        classOf[MaximizationObjectiveTest],
        classOf[HierarchicalObjectiveTest]))
final class ObjectiveTestSuite
