package yuck.core.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

@Suite
@SelectClasses(
    Array(
        classOf[BooleanValueOrderingTest],
        classOf[BooleanValueOrderingCostModelTest],
        classOf[BooleanValueTest],
        classOf[BooleanTypeTraitsTest],
        classOf[IntegerValueOperationsTest],
        classOf[IntegerValueOrderingCostModelTest],
        classOf[IntegerValueTest],
        classOf[IntegerTypeTraitsTest],
        classOf[IntegerSetValueOrderingTest],
        classOf[IntegerSetValueOrderingCostModelTest],
        classOf[IntegerSetValueTest],
        classOf[IntegerSetTypeTraitsTest],
        classOf[PolymorphicListValueTest]))
final class ValueTestSuite
