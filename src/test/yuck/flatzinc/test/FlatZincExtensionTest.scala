package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*

import scala.language.implicitConversions
import yuck.constraints.*
import yuck.core.{*, given}
import yuck.flatzinc.compiler.Bool2Costs1
import yuck.flatzinc.test.util.*
import yuck.test.*
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
        val result = solveWithResult(task.copy(problemName = "bool2costs_function_test", verificationFrequency = VerifyOnlyLastSolution))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
        assertEq(result.space.numberOfConstraints[Bool2Costs1], 1)
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
        val result = solveWithResult(task.copy(problemName = "int_max_goal_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), Ten)
        assertEq(result.quality(2), Eight)
        assertEq(result.quality(3), Three)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint]))
    def testIntMinGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "int_min_goal_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), Three)
        assertEq(result.quality(2), Eight)
        assertEq(result.quality(3), Ten)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testSatGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "sat_goal_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[Alldistinct[_]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), False)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testWarmStartFromSolution(): Unit = {
        val result = solveWithResult(task.copy(problemName = "warm_start_from_solution_test"))
        assert(result.isSolution)
        assert(result.warmStartWasPerformed)
        assertEq(
            result.space.searchVariables.toSeq.sortBy(_.name).map(result.assignment.value),
            Seq(Three, Two, Two, One, Two, One))
        assert(! result.searchWasPerformed)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testWarmStartFromPartialSolution(): Unit = {
        val result = solveWithResult(task.copy(problemName = "warm_start_from_partial_solution_test"))
        assert(result.isSolution)
        assert(result.warmStartWasPerformed)
        assert(result.searchWasPerformed)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testWarmStartFromInvalidSolution(): Unit = {
        val result = solveWithResult(task.copy(problemName = "warm_start_from_invalid_solution_test"))
        assert(result.isSolution)
        assert(result.warmStartWasPerformed)
        assert(result.searchWasPerformed)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testExtendedWarmStartSyntax(): Unit = {
        val result = solveWithResult(task.copy(problemName = "extended_warm_start_syntax_test"))
        assert(result.isSolution)
        assert(result.warmStartWasPerformed)
        assertEq(
            result.space.searchVariables.toSeq.sortBy(_.name).map(result.assignment.value),
            Seq(Three, Two, Two, One, Two, One))
        assert(! result.searchWasPerformed)
    }

}
