package yuck.flatzinc.test

import org.junit.*

/**
 * Big integration test suite
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[MiniZincExamples],
        classOf[MiniZincChallengeIntakeTests],
        classOf[MiniZincChallenges]))
class MiniZincTestSuites
