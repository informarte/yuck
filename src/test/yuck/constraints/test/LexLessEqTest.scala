package yuck.constraints.test

import org.junit._

import yuck.constraints.LeRelation
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class LexLessEqTest extends UnitTest with LexTestTooling[IntegerValue] {

    override protected val valueTraits = IntegerValueTraits
    override protected val space = new  Space(logger, sigint)

    @Test
    def test33: Unit = {
        runScenario(
            TestScenario(
                LeRelation,
                3, 3,
                TestStep(True, (X(1), One), (X(2), One), (X(3), One), (Y(1), One), (Y(2), One), (Y(3), One)),
                TestStep(True, (X(1), Zero), (X(2), Zero), (X(3), Zero)),
                TestStep(True, (Y(1), Zero), (Y(2), Zero), (Y(3), Zero)),
                TestStep(False, (X(3), One)),
                TestStep(False2, (X(2), One)),
                TestStep(False3, (X(1), One))))
    }

    @Test
    def test23: Unit = {
        runScenario(
            TestScenario(
                LeRelation,
                2, 3,
                TestStep(True, (X(1), One), (X(2), One), (Y(1), One), (Y(2), One), (Y(3), One)),
                TestStep(True, (X(1), Zero), (X(2), Zero)),
                TestStep(True, (Y(1), Zero), (Y(2), Zero), (Y(3), Zero)),
                TestStep(False, (X(2), One)),
                TestStep(False2, (X(1), One))))
    }

    @Test
    def test32: Unit = {
        runScenario(
            TestScenario(
                LeRelation,
                3, 2,
                TestStep(False, (X(1), One), (X(2), One), (X(3), One), (Y(1), One), (Y(2), One)),
                TestStep(True, (X(1), Zero), (X(2), Zero), (X(3), Zero)),
                TestStep(False, (Y(1), Zero), (Y(2), Zero)),
                TestStep(False, (X(3), One)),
                TestStep(False, (X(2), One)),
                TestStep(False2, (X(1), One))))
    }

}
