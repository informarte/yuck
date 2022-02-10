package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._

import scala.language.implicitConversions

import yuck.constraints._
import yuck.core._
import yuck.flatzinc.test.util._
import yuck.test.util.ParallelTestRunner

/**
 * Tests that cover Yuck's extensions of FlatZinc
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[ParallelTestRunner])
final class FlatZincExtensionTest extends FrontEndTest {

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDisjunctiveConstraint]))
    def testBool2CostsFunction: Unit = {
        val result = solveWithResult(task.copy(problemName = "bool2costs_function_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
        assertEq(quality(result), Zero)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testIntDomain: Unit = {
        val result1 = solveWithResult(task.copy(problemName = "int_domain_min_test"))
        assertEq(quality(result1), One)
        val result2 = solveWithResult(task.copy(problemName = "int_domain_max_test"))
        assertEq(quality(result2), Two)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasBinPackingConstraint]))
    def testIntMaxGoal: Unit = {
        val result = solveWithResult(task.copy(problemName = "int_max_goal_test", verifySolution = false))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(quality(result, 0), True)
        assertEq(quality(result, 1), Ten)
        assertEq(quality(result, 2), Eight)
        assertEq(quality(result, 3), Three)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint]))
    def testIntMinGoal: Unit = {
        val result = solveWithResult(task.copy(problemName = "int_min_goal_test", verifySolution = false))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(quality(result, 0), True)
        assertEq(quality(result, 1), Three)
        assertEq(quality(result, 2), Eight)
        assertEq(quality(result, 3), Ten)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testSatGoal: Unit = {
        val result = solveWithResult(task.copy(problemName = "sat_goal_test", verifySolution = false))
        assertEq(numberOfConstraints[Alldistinct[_]](result), 1)
        assertEq(quality(result, 0), True)
        assertEq(quality(result, 1), False)
    }

}
