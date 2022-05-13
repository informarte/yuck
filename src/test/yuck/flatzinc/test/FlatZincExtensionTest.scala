package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*

import scala.language.implicitConversions

import yuck.constraints.*
import yuck.core.*
import yuck.flatzinc.test.util.*
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
    def testBool2CostsFunction(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bool2costs_function_test"))
        assertEq(result.numberOfConstraints[Disjoint2], 1)
        assertEq(result.quality, Zero)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testIntDomain(): Unit = {
        val result1 = solveWithResult(task.copy(problemName = "int_domain_min_test"))
        assertEq(result1.quality, One)
        val result2 = solveWithResult(task.copy(problemName = "int_domain_max_test"))
        assertEq(result2.quality, Two)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasBinPackingConstraint]))
    def testIntMaxGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "int_max_goal_test", verifySolution = false))
        assertEq(result.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), Ten)
        assertEq(result.quality(2), Eight)
        assertEq(result.quality(3), Three)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint]))
    def testIntMinGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "int_min_goal_test", verifySolution = false))
        assertEq(result.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), Three)
        assertEq(result.quality(2), Eight)
        assertEq(result.quality(3), Ten)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testSatGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "sat_goal_test", verifySolution = false))
        assertEq(result.numberOfConstraints[Alldistinct[_]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), False)
    }

}
