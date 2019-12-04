package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._

import scala.language.implicitConversions

import yuck.constraints._
import yuck.core._
import yuck.flatzinc.test.util._

/**
 * Tests that cover Yuck's extensions of FlatZinc
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class FlatZincExtensionTest extends FrontEndTest {

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDisjunctiveConstraint]))
    def testBool2CostsFunction: Unit = {
        val result = solveWithResult(task.copy(problemName = "bool2costs_function_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
        assertEq(quality(result), Zero)
    }

}