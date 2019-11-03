package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._

import scala.language.implicitConversions

import yuck.annealing.AnnealingResult
import yuck.constraints.{AlldistinctNeighbourhood, GeneralInverseNeighbourhood, SelfInverseNeighbourhood, SimpleInverseNeighbourhood}
import yuck.core._
import yuck.constraints._
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
            solverConfiguration = FlatZincSolverConfiguration(restartLimit = 0),
            maybeRuntimeLimitInSeconds = Some(10),
            assertWhenUnsolved = true,
            reusePreviousTestResult = false)

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
    def testAlldifferentWithEqualDomains: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_equal_domains"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentWithEqualDomainsAndMoreValuesThanVariables: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_equal_domains_and_more_values_than_variables"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentWithDifferentDomains: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_different_domains"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentWithDifferentDomainsAndMoreValuesThanVariables: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test_with_different_domains_and_more_values_than_variables"))
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_int_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Alldistinct[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExcept0Constraint]))
    def testAlldifferentExcept0: Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_0_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[AlldistinctExceptZero[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExcept0Constraint]))
    def testAlldifferentExcept0Reif: Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_0_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[AlldistinctExceptZero[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPacking: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Cumulative]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Cumulative]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapa: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Cumulative]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapaReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Cumulative]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithUnboundedLoadVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_unbounded_load_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithEqualLoadVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_equal_load_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithDuplicateBinVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_duplicate_bin_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEq: Unit = {
        val result = solveWithResult(task.copy(problemName = "count_eq_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountConst[_]]), 2)
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountVar[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulative: Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Cumulative]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulativeReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Cumulative]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnNonstrict: Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnNonstrictReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnStrict: Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnStrictReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveNonstrict: Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveNonstrictReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveStrict: Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveStrictReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Disjoint2]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testEmptyTableInt: Unit = {
        assertEx(
            solve(task.copy(problemName = "empty_table_int_test")),
            classOf[InconsistentProblemException])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinality: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUp: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUpReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithBoundedSearchVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_test_with_bounded_search_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Inverse]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithUnboundedSearchVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_test_with_unbounded_search_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Inverse]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithChannelVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_test_with_channel_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Inverse]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithUnrestrictedPairing: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "inverse_test_with_unrestricted_pairing"))
        assert(neighbourhood(result).isInstanceOf[SimpleInverseNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithRestrictedPairing: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "inverse_test_with_restricted_pairing"))
        assert(neighbourhood(result).isInstanceOf[GeneralInverseNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithOneFunction: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "inverse_test_with_one_function"))
        assert(neighbourhood(result).isInstanceOf[SelfInverseNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseDecomposition: Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "inverse_decomposition_test"))
        assert(neighbourhood(result).isInstanceOf[NeighbourhoodCollection])
        assert(neighbourhood(result).asInstanceOf[NeighbourhoodCollection].children.forall(_.isInstanceOf[SimpleInverseNeighbourhood]))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Inverse]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessBool: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLess[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessBoolReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLess[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqBool: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLessEq[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqBoolReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLessEq[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessInt: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLess[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessIntReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLess[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqInt: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLessEq[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqIntReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[LexLessEq[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMaximumConstraint]))
    def testMaximum: Unit = {
        solve(task.copy(problemName = "maximum_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMinimumConstraint]))
    def testMinimum: Unit = {
        solve(task.copy(problemName = "minimum_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberInt: Unit = {
        val result = solveWithResult(task.copy(problemName = "member_int_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountVar[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberIntReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "member_int_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountVar[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberBool: Unit = {
        val result = solveWithResult(task.copy(problemName = "member_bool_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountVar[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberBoolReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "member_bool_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountVar[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberSet: Unit = {
        val result = solveWithResult(task.copy(problemName = "member_set_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountVar[_]]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberSetReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "member_set_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[CountVar[_]]), 2)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
    def testNValue: Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[NumberOfDistinctValues[_]]), 1)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
    def testNValueReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[NumberOfDistinctValues[_]]), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def testRegular: Unit = {
        val result = solveWithResult(task.copy(problemName = "regular_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Regular]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def testRegularReif: Unit = {
        // The std library does not provide a decomposition for regular_reif,
        // so we cannot verify the solution.
        val result = solveWithResult(task.copy(problemName = "regular_reif_test", verifySolution = false))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Regular]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testRegularWithDuplicateInputVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "regular_test_with_duplicate_input_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[Regular]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableInt: Unit = {
        val result = solveWithResult(task.copy(problemName = "table_int_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[IntegerTable]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableIntReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "table_int_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[IntegerTable]), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testArrayAccessWhereResultMustEqualIndex: Unit = {
        solve(task.copy(problemName = "array_access_where_result_must_equal_index"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testArrayAccessWhereIndexIsChannelVariable: Unit = {
        solve(task.copy(problemName = "array_access_where_index_is_channel_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationOfSumWithNegativeAddends: Unit = {
        solve(task.copy(problemName = "minimization_of_sum_with_negative_addends"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationProblemWithBoundedDanglingObjectiveVariable: Unit = {
        solve(task.copy(problemName = "minimization_problem_with_bounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testMinimizationProblemWithUnboundedDanglingObjectiveVariable: Unit = {
        solve(task.copy(problemName = "minimization_problem_with_unbounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def testMinimizationProblemWithImplicitlyConstrainedObjectiveVariable: Unit = {
        val result = solveWithResult(task.copy(problemName = "minimization_problem_with_implicitly_constrained_objective_variable"))
        assertEq(result.asInstanceOf[AnnealingResult].consultationsPerMove, 0)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testMaximizationProblemWithBoundedDanglingObjectiveVariable: Unit = {
        solve(task.copy(problemName = "maximization_problem_with_bounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testMaximizationProblemWithUnboundedDanglingObjectiveVariable: Unit = {
        solve(task.copy(problemName = "maximization_problem_with_unbounded_dangling_objective_variable"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint]))
    def testMaximizationProblemWithImplicitlyConstrainedObjectiveVariable: Unit = {
        val result = solveWithResult(task.copy(problemName = "maximization_problem_with_implicitly_constrained_objective_variable"))
        assertEq(result.asInstanceOf[AnnealingResult].consultationsPerMove, 0)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithBoundedDanglingVariable: Unit = {
        solve(task.copy(problemName = "problem_with_bounded_dangling_variable"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedDanglingVariable: Unit = {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_dangling_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithBoundedIrrelevantSearchVariable: Unit = {
        solve(task.copy(problemName = "problem_with_bounded_irrelevant_search_variable"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedIrrelevantSearchVariable: Unit = {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_irrelevant_search_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def testProblemWithUnboundedRelevantSearchVariable: Unit = {
        assertEx(
            solve(task.copy(problemName = "problem_with_unbounded_relevant_search_variable")),
            classOf[VariableWithInfiniteDomainException])
    }

}
