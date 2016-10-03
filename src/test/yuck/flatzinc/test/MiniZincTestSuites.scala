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
        classOf[EasyMiniZincExamples],
        classOf[MediumMiniZincExamples],
        classOf[MiniZincChallenge2012],
        classOf[MiniZincChallenge2013],
        classOf[MiniZincChallenge2014],
        classOf[MiniZincChallenge2015]))
@Test
class MiniZincTestSuites {
}
