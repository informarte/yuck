package yuck.core.test

import org.junit.*

@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[RandomReassignmentGeneratorTest],
        classOf[NeighbourhoodCollectionTest]))
final class NeighbourhoodTestSuite
