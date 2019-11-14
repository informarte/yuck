package yuck.constraints.test

import org.junit._

import yuck.constraints.LtRelation
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class LexLessTest extends UnitTest with LexTestTooling[IntegerValue] {

    override protected val valueTraits = IntegerValueTraits
    override protected val space = new  Space(logger, sigint)

    @Test
    def test33: Unit = {
        runScenario(
            TestScenario(
                LtRelation,
                3, 3,
                TestStep(False, (X(1), One), (X(2), One), (X(3), One), (Y(1), One), (Y(2), One), (Y(3), One)),
                TestStep(True, (X(1), Zero)),
                TestStep(True, (X(1), One), (X(2), Zero)),
                TestStep(True, (X(2), One), (X(3), Zero)),
                TestStep(False, (Y(3), Zero)),
                TestStep(False2, (Y(2), Zero)),
                TestStep(False3, (Y(1), Zero))))
    }

    @Test
    def test23: Unit = {
        runScenario(
            TestScenario(
                LtRelation,
                2, 3,
                TestStep(True, (X(1), One), (X(2), One), (Y(1), One), (Y(2), One), (Y(3), One)),
                TestStep(True, (Y(3), Zero)),
                TestStep(False, (Y(2), Zero)),
                TestStep(False2, (Y(1), Zero)),
                TestStep(False, (X(1), Zero)),
                TestStep(True, (X(2), Zero))))
    }

    @Test
    def test32: Unit = {
        runScenario(
            TestScenario(
                LtRelation,
                3, 2,
                TestStep(False, (X(1), One), (X(2), One), (X(3), One), (Y(1), One), (Y(2), One)),
                TestStep(False, (X(3), Zero)),
                TestStep(True, (X(2), Zero)),
                TestStep(True, (X(1), Zero)),
                TestStep(True, (Y(1), Zero)),
                TestStep(False, (Y(2), Zero))))
    }

}
