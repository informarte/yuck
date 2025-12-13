package yuck.core.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

@Suite
@SelectClasses(
    Array(
        classOf[RandomReassignmentGeneratorTest],
        classOf[NeighbourhoodCollectionTest]))
final class NeighbourhoodTestSuite
