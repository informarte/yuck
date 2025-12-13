package yuck.flatzinc.test

import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}
import org.junit.jupiter.api.{Tag, Test}
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.SolvingMethod
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.HasGlobalConstraint.*
import yuck.flatzinc.test.util.ProblemType.*
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*
import yuck.flatzinc.test.util.VerificationFrequency.*

/**
 * Test cases taken from the MiniZinc challenge submission procedure
 */
@ParameterizedClass
@MethodSource(Array("parameters"))
@Execution(ExecutionMode.CONCURRENT)
class MiniZincChallengeIntakeTests(preferredSolvingMethod: SolvingMethod) extends ZincBasedTest {

    private val task =
        ZincTestTask(
            directoryLayout = NonStandardMiniZincBenchmarksLayout,
            suitePath = "resources/mzn/tests/minizinc-challenge-intake-tests/tests",
            suiteName = "minizinc-challenge-intake-tests",
            solverConfiguration =
                ZincTestTask().solverConfiguration.copy(
                    name = preferredSolvingMethod.toString.toLowerCase,
                    numberOfSolvers = 1,
                    maybePreferredSolvingMethod = Some(preferredSolvingMethod),
                    maybeRuntimeLimitInSeconds = Some(10)),
            throwWhenUnsolved = true,
            verificationFrequency = VerifyEverySolution,
            createDotFile = true)

    @Test
    @Tag(MinimizationProblem)
    def testBasic(): Unit = {
        solve(task.copy(problemName = "test_basic", modelName = "basic", maybeOptimum = Some(1)))
    }

    @Test
    @Tag(MaximizationProblem)
    def testBasic2Large(): Unit = {
        solve(task.copy(directoryLayout = StandardMiniZincBenchmarksLayout, problemName = "test_basic2", modelName = "basic2", instanceName = "large", maybeOptimum = Some(2500)))
    }

    @Test
    @Tag(MaximizationProblem)
    def testBasic2Small(): Unit = {
        solve(task.copy(directoryLayout = StandardMiniZincBenchmarksLayout, problemName = "test_basic2", modelName = "basic2", instanceName = "small", maybeOptimum = Some(19)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasAllDifferentConstraint)
    def testAllDifferent(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_all_different", maybeOptimum = Some(1)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasAllDifferentExceptConstraint)
    def testAllDifferentExcept0(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_all_different_except_0", maybeOptimum = Some(10)))
    }

    @Test
    @Tag(MinimizationProblem)
    def testAllDisjoint(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_all_disjoint", maybeOptimum = Some(0)))
    }

    @Test
    @Tag(MinimizationProblem)
    def testAllEqual(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_all_equal", maybeOptimum = Some(10)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCircuitConstraint)
    def testCircuit(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_circuit"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCumulativeConstraint)
    def testCumulative(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_cumulative"))
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasCumulativeConstraint)
    def testCumulativeOpt(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_cumulative_opt", maybeOptimum = Some(3)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasDecreasingConstraint)
    def testDecreasing(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_decreasing", maybeOptimum = Some(13)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasDiffnConstraint)
    def testDiffn(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_diffn", maybeOptimum = Some(44)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctive(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_disjunctive", maybeOptimum = Some(6)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctiveOpt(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_disjunctive_opt", maybeOptimum = Some(5)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctiveStrict(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_disjunctive_strict", maybeOptimum = Some(7)))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctiveStrictOpt(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_disjunctive_strict_opt", maybeOptimum = Some(6)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinality(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_global_cardinality"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinalityLowUp(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_global_cardinality_low_up"))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasIncreasingConstraint)
    def testIncreasing(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_increasing", maybeOptimum = Some(13)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasInverseConstraint)
    def testInverse(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_inverse"))
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasLexLessEqConstraint)
    def testLexLessEq(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_lex_lesseq", maybeOptimum = Some(0)))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasRegularConstraint)
    def testRegular(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_regular"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasValuePrecedeConstraint)
    def testValuePrecede(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_value_precede"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasValuePrecedeChainConstraint)
    def testValuePrecedeChain(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_value_precede_chain"))
    }

    @Test
    @Tag(MaximizationProblem)
    def testVarSet1(): Unit = {
        solve(task.copy(problemName = "test_var_set", modelName = "var_set_1", maybeOptimum = Some(2)))
    }

    @Test
    @Tag(MaximizationProblem)
    def testVarSet2(): Unit = {
        solve(task.copy(problemName = "test_var_set", modelName = "var_set_2", maybeOptimum = Some(97)))
    }

}

object MiniZincChallengeIntakeTests {

    def parameters = SolvingMethod.values

}
