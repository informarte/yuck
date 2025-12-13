package yuck.flatzinc.test

import scala.language.implicitConversions

import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}
import org.junit.jupiter.api.{Tag, Test}

import yuck.constraints.*
import yuck.core.*
import yuck.flatzinc.compiler.Bool2Costs1
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.HasGlobalConstraint.*
import yuck.flatzinc.test.util.ProblemType.*
import yuck.flatzinc.test.util.VerificationFrequency.*
import yuck.test.*

/**
 * Tests that cover Yuck's extensions of FlatZinc
 */
@Execution(ExecutionMode.CONCURRENT)
final class FlatZincExtensionTest extends FrontEndTest {

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasDisjunctiveConstraint)
    def testBool2CostsFunction(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bool2costs_function_test", verificationFrequency = VerifyOnlyLastSolution))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
        assertEq(result.space.numberOfConstraints[Bool2Costs1], 1)
        assertEq(result.quality, Zero)
    }

    @Test
    @Tag(MinimizationProblem)
    def testIntDomain(): Unit = {
        val result1 = solveWithResult(task.copy(problemName = "int_domain_min_test"))
        assertEq(result1.quality, One)
        val result2 = solveWithResult(task.copy(problemName = "int_domain_max_test"))
        assertEq(result2.quality, Two)
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasBinPackingConstraint)
    def testIntMaxGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "int_max_goal_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[BinPacking[?]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), Ten)
        assertEq(result.quality(2), Eight)
        assertEq(result.quality(3), Three)
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasBinPackingConstraint)
    def testIntMinGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "int_min_goal_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[BinPacking[?]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), Three)
        assertEq(result.quality(2), Eight)
        assertEq(result.quality(3), Ten)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentConstraint)
    def testSatGoal(): Unit = {
        val result = solveWithResult(task.copy(problemName = "sat_goal_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[AllDifferent[?]], 1)
        assertEq(result.quality(0), True)
        assertEq(result.quality(1), False)
    }

    @Test
    @Tag(SatisfiabilityProblem)
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
    @Tag(SatisfiabilityProblem)
    def testWarmStartFromPartialSolution(): Unit = {
        val result = solveWithResult(task.copy(problemName = "warm_start_from_partial_solution_test"))
        assert(result.isSolution)
        assert(result.warmStartWasPerformed)
        assert(result.searchWasPerformed)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    def testWarmStartFromInvalidSolution(): Unit = {
        val result = solveWithResult(task.copy(problemName = "warm_start_from_invalid_solution_test"))
        assert(result.isSolution)
        assert(result.warmStartWasPerformed)
        assert(result.searchWasPerformed)
    }

    @Test
    @Tag(SatisfiabilityProblem)
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
