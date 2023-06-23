package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*
import org.junit.experimental.categories.Categories.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.test.util.*
import yuck.test.util.ParallelTestRunner

/**
 * Test cases taken from the MiniZinc challenge submission procedure
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[ParallelTestRunner])
class MiniZincChallengeIntakeTests extends ZincBasedTest {

    override protected val logToConsole = false

    private val task =
        ZincTestTask(
            directoryLayout = NonStandardMiniZincBenchmarksLayout,
            suitePath = "resources/mzn/tests/minizinc-challenge-intake-tests/tests",
            suiteName = "minizinc-challenge-intake-tests",
            maybeRestartLimit = Some(0),
            maybeRuntimeLimitInSeconds = Some(10),
            throwWhenUnsolved = true,
            reusePreviousTestResult = false,
            verificationFrequency = VerifyEverySolution,
            createDotFile = true)

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testBasic(): Unit = {
        solve(task.copy(problemName = "test_basic", modelName = "basic", maybeOptimum = Some(1)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testBasic2Large(): Unit = {
        solve(task.copy(directoryLayout = StandardMiniZincBenchmarksLayout, problemName = "test_basic2", modelName = "basic2", instanceName = "large", maybeOptimum = Some(2500)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testBasic2Small(): Unit = {
        solve(task.copy(directoryLayout = StandardMiniZincBenchmarksLayout, problemName = "test_basic2", modelName = "basic2", instanceName = "small", maybeOptimum = Some(19)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def testAllDifferent(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_all_different", maybeOptimum = Some(1)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testAllDisjoint(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_all_disjoint", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testAllEqual(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_all_equal", maybeOptimum = Some(10)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint]))
    def testCircuit(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_circuit"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulative(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_cumulative"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint]))
    def testDiffn(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_diffn", maybeOptimum = Some(44)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctive(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_disjunctive", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinality(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_global_cardinality"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUp(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_global_cardinality_low_up"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasIncreasingConstraint]))
    def testIncreasing(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_increasing", maybeOptimum = Some(13)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEq(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_lex_lesseq", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def testRegular(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_regular"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasValuePrecedeConstraint]))
    def testValuePrecede(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_value_precede"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasValuePrecedeChainConstraint]))
    def testValuePrecedeChain(): Unit = {
        solve(task.copy(problemName = "test_globals", modelName = "test_value_precede_chain"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testVarSet1(): Unit = {
        solve(task.copy(problemName = "test_var_set", modelName = "var_set_1", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testVarSet2(): Unit = {
        solve(task.copy(problemName = "test_var_set", modelName = "var_set_2", maybeOptimum = Some(97)))
    }

}
