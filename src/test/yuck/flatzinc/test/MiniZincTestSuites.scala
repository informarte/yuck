package yuck.flatzinc.test

import org.junit._

/**
 * @author Michael Marte
 *
 * Big suite for testing the compiler.
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[MiniZincExamples],
        classOf[MiniZincChallenges]))
@Test
class MiniZincTestSuites {
}
