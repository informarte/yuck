package yuck.constraints.test

import org.junit._

import yuck.constraints._
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class NumberOfDistinctValuesTest extends UnitTest with ConstraintTestTooling {

    @Test
    def testNumberOfDistinctValues: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(One, Two)
        val xs = for (i <- 1 to 3) yield space.createVariable("x[%d]".format(i), d)
        val m = new IntegerVariable(space.nextVariableId, "m", NonNegativeIntegerRange)
        space.post(new NumberOfDistinctValues(space.nextConstraintId, null, xs, m))
        assertEq(space.searchVariables, xs.toSet)
        runScenario(
            TestScenario(
                space,
                Initialize("setup", (xs(0), One), (xs(1), One),  (xs(2), One), (m, One)),
                ConsultAndCommit("1", (xs(0), Two), (m, Two)),
                ConsultAndCommit("2", (xs(1), Two), (m, Two)),
                ConsultAndCommit("3", (xs(2), Two), (m, One))))
    }

}
