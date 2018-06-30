package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

import scala.language.implicitConversions
import yuck.annealing.AnnealingResult
import yuck.constraints.{AlldistinctNeighbourhood, GeneralInverseNeighbourhood, SelfInverseNeighbourhood, SimpleInverseNeighbourhood}
import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.{FlatZincCompilerResult, VariableWithInfiniteDomainException}
import yuck.flatzinc.test.util._

/**
 * Additional tests that cover features not exercised by the MiniZinc examples.
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class FlatZincImplementationTest extends MiniZincBasedTest {

    private val task =
        MiniZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests",
            solverConfiguration = new FlatZincSolverConfiguration(restartLimit = 0))

    private val taskWithImplicitSolving =
        task.copy(
            solverConfiguration =
                task.solverConfiguration.copy(
                    preferImplicitSolvingOverDomainPruning = false,
                    maybeRoundLimit = Some(1)))

    private def neighbourhood(result: Result): Neighbourhood =
        result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult].maybeNeighbourhood.get

    private implicit def createTask(problemName: String): MiniZincTestTask = task.copy(problemName = problemName)

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentWithEqualDomains {
        val result = solve(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_equal_domains"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentWithEqualDomainsAndMoreValuesThanVariables {
        val result = solve(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_equal_domains_and_more_values_than_variables"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentWithDifferentDomains {
        val result = solve(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_different_domains"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentWithDifferentDomainsAndMoreValuesThanVariables {
        val result = solve(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_different_domains_and_more_values_than_variables"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExcept0Constraint]))
    def testAlldifferentExcept0 {
        solve(task.copy(problemName = "alldifferent_except_0_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAtLeastConstraint]))
    def testAtLeast {
        solve(task.copy(problemName = "at_least_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAtMostConstraint]))
    def testAtMost {
        solve(task.copy(problemName = "at_most_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPacking {
        solve(task.copy(problemName = "bin_packing_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapa {
        solve(task.copy(problemName = "bin_packing_capa_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithUnboundedLoadVariables {
        solve(task.copy(problemName = "bin_packing_load_test_with_unbounded_load_variables"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithEqualLoadVariables {
        solve(task.copy(problemName = "bin_packing_load_test_with_equal_load_variables"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithDuplicateBinVariables {
        solve(task.copy(problemName = "bin_packing_load_test_with_duplicate_bin_variables"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEq {
        solve(task.copy(problemName = "count_eq_var_test"))
        solve(task.copy(problemName = "count_eq_const_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLeq {
        solve(task.copy(problemName = "count_leq_var_test"))
        solve(task.copy(problemName = "count_leq_const_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLt {
        solve(task.copy(problemName = "count_lt_var_test"))
        solve(task.copy(problemName = "count_lt_const_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGeq {
        solve(task.copy(problemName = "count_geq_var_test"))
        solve(task.copy(problemName = "count_geq_const_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGt {
        solve(task.copy(problemName = "count_gt_var_test"))
        solve(task.copy(problemName = "count_gt_const_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountNeq {
        solve(task.copy(problemName = "count_neq_var_test"))
        solve(task.copy(problemName = "count_neq_const_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulative {
        solve(task.copy(problemName = "cumulative_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctive {
        solve(task.copy(problemName = "disjunctive_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasExactlyConstraint]))
    def testExactly {
        solve(task.copy(problemName = "exactly_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinality {
        solve(task.copy(problemName = "global_cardinality_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithBoundedSearchVariables {
        solve(task.copy(problemName = "inverse_test_with_bounded_search_variables"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithUnboundedSearchVariables {
        solve(task.copy(problemName = "inverse_test_with_unbounded_search_variables"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithChannelVariables {
        solve(task.copy(problemName = "inverse_test_with_channel_variables"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithUnrestrictedPairing {
        val result = solve(taskWithImplicitSolving.copy(problemName = "inverse_test_with_unrestricted_pairing"))
        assert(neighbourhood(result).isInstanceOf[SimpleInverseNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithRestrictedPairing {
        val result = solve(taskWithImplicitSolving.copy(problemName = "inverse_test_with_restricted_pairing"))
        assert(neighbourhood(result).isInstanceOf[GeneralInverseNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithOneFunction {
        val result = solve(taskWithImplicitSolving.copy(problemName = "inverse_test_with_one_function"))
        assert(neighbourhood(result).isInstanceOf[SelfInverseNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseDecomposition {
        val result = solve(taskWithImplicitSolving.copy(problemName = "inverse_decomposition_test"))
        assert(neighbourhood(result).isInstanceOf[NeighbourhoodCollection])
        assert(neighbourhood(result).asInstanceOf[NeighbourhoodCollection].children.forall(_.isInstanceOf[SimpleInverseNeighbourhood]))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessBool {
        solve(task.copy(problemName = "lex_less_bool_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqBool {
        solve(task.copy(problemName = "lex_lesseq_bool_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMaximumConstraint]))
    def testMaximum {
        solve(task.copy(problemName = "maximum_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMinimumConstraint]))
    def testMinimum {
        solve(task.copy(problemName = "minimum_int_test"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
    def testNValue {
        solve(task.copy(problemName = "nvalue_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def testRegular {
        solve(task.copy(problemName = "regular_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableInt {
        solve(task.copy(problemName = "table_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testArrayAccessWhereResultMustEqualIndex {
        solve(task.copy(problemName = "array_access_where_result_must_equal_index"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testArrayAccessWhereIndexIsChannelVariable {
        solve(task.copy(problemName = "array_access_where_index_is_channel_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationOfSumWithNegativeAddends {
        solve(task.copy(problemName = "minimization_of_sum_with_negative_addends"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationProblemWithBoundedDanglingObjectiveVariable {
        solve(task.copy(problemName = "minimization_problem_with_bounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationProblemWithUnboundedDanglingObjectiveVariable {
        solve(task.copy(problemName = "minimization_problem_with_unbounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def testMinimizationProblemWithImplicitlyConstrainedObjectiveVariable {
        val result = solve(task.copy(problemName = "minimization_problem_with_implicitly_constrained_objective_variable"))
        assertEq(result.asInstanceOf[AnnealingResult].consultationsPerMove, 0)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testMaximizationProblemWithBoundedDanglingObjectiveVariable {
        solve(task.copy(problemName = "maximization_problem_with_bounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testMaximizationProblemWithUnboundedDanglingObjectiveVariable {
        solve(task.copy(problemName = "maximization_problem_with_unbounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def testMaximizationProblemWithImplicitlyConstrainedObjectiveVariable {
        val result = solve(task.copy(problemName = "maximization_problem_with_implicitly_constrained_objective_variable"))
        assertEq(result.asInstanceOf[AnnealingResult].consultationsPerMove, 0)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithBoundedDanglingVariable {
        solve(task.copy(problemName = "problem_with_bounded_dangling_variable"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedDanglingVariable {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_dangling_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithBoundedIrrelevantSearchVariable {
        solve(task.copy(problemName = "problem_with_bounded_irrelevant_search_variable"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedIrrelevantSearchVariable {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_irrelevant_search_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedRelevantSearchVariable {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_relevant_search_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

}
