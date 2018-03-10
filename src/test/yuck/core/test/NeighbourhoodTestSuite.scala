package yuck.core.test

import org.junit._

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[SimpleRandomReassignmentGeneratorTest],
        classOf[RandomReassignmentGeneratorTest],
        classOf[RandomCircularSwapGeneratorTest],
        classOf[NeighbourhoodCollectionTest]))
final class NeighbourhoodTestSuite {
}
