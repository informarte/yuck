package yuck.test

import org.junit._

import yuck.flatzinc.test.{FlatZincImplementationTest, TractableMiniZincExamples}

@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[UnitTestSuite],
        classOf[FlatZincImplementationTest],
        classOf[TractableMiniZincExamples]))
@Test
final class ContinuousIntegrationTestSuite {
}
