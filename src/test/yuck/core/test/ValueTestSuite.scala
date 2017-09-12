package yuck.core.test

import org.junit._

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[BooleanValueTest],
        classOf[BooleanValueTraitsTest],
        classOf[IntegerValueTest],
        classOf[IntegerValueTraitsTest],
        classOf[IntegerSetValueTest],
        classOf[IntegerSetValueTraitsTest],
        classOf[PolymorphicListValueTest]))
@Test
final class ValueTestSuite {
}
