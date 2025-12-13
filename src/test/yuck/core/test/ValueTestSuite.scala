package yuck.core.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

@Suite
@SelectClasses(
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
