package yuck.core.test

import yuck.core._

/**
 * @author Michael Marte
 *
 */
trait IntegerSetValueTestData {

    protected val baseData =
        List(
            EmptyIntegerRange, NonNegativeIntegerRange, CompleteIntegerRange,
            new IntegerRange(Zero, Two), new IntegerRange(One, Three), new IntegerRange(Two, Four),
            new IntegerRange(Zero, Five))
    protected val testData =
        baseData.map(new IntegerSetValue(_))

}
