package yuck.core.test

import org.junit.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[BooleanValueOrderingTest],
        classOf[BooleanValueOrderingCostModelTest],
        classOf[BooleanValueTest],
        classOf[BooleanValueTraitsTest],
        classOf[IntegerValueOperationsTest],
        classOf[IntegerValueOrderingCostModelTest],
        classOf[IntegerValueTest],
        classOf[IntegerValueTraitsTest],
        classOf[IntegerSetValueOrderingTest],
        classOf[IntegerSetValueOrderingCostModelTest],
        classOf[IntegerSetValueTest],
        classOf[IntegerSetValueTraitsTest],
        classOf[PolymorphicListValueTest]))
final class ValueTestSuite
