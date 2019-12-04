package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._

import scala.language.implicitConversions

import yuck.core._
import yuck.constraints._
import yuck.flatzinc.test.util._

/**
 * Tests to make sure that the global constraints provided by Yuck's library get
 * compiled correctly
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class GlobalConstraintCompilationTest extends FrontEndTest {

    private val taskWithImplicitSolving =
        task.copy(
            solverConfiguration =
                task.solverConfiguration.copy(
                    preferImplicitSolvingOverDomainPruning = false,
                    maybeRoundLimit = Some(1)))

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
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 2)
        assertEq(result.space.searchVariables.size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapa: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapaReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_reif_test"))
        // minizinc eliminates one of the bin_packing_load constraints :-)
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithUnboundedLoadVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_unbounded_load_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithEqualLoadVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_equal_load_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 7)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithDuplicateBinVariables: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_duplicate_bin_variables"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 8)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 2)
        assertEq(result.space.searchVariables.size, 9)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadFn: Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_fn_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 6)
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
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinality: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 4)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 4)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityFn: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_fn_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityClosedFn: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_closed_fn_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUp: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUpReif: Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_reif_test"))
        assertEq(result.space.numberOfConstraints(_.isInstanceOf[BinPacking[_]]), 1)
        assertEq(result.space.searchVariables.size, 3)
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

}
