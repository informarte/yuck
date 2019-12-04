package yuck.flatzinc.test

import org.junit._

/**
 * Big integration test suite
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[MiniZincExamples],
        classOf[MiniZincChallenges]))
@Test
class MiniZincTestSuites {
}
