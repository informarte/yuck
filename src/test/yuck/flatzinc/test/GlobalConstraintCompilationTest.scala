package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*

import scala.language.implicitConversions

import yuck.constraints.*
import yuck.core.*
import yuck.flatzinc.compiler.Bool2Costs2
import yuck.flatzinc.test.util.*
import yuck.test.util.ParallelTestRunner

/**
 * Tests to make sure that the global constraints provided by Yuck's library get
 * compiled correctly
 *
 * @author Michael Marte
 */
@runner.RunWith(classOf[ParallelTestRunner])
final class GlobalConstraintCompilationTest extends FrontEndTest {

    private val taskWithImplicitSolving =
        task.copy(
            solverConfiguration =
                task.solverConfiguration.copy(
                    maybeRoundLimit = Some(1)))

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferent(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test"))
        assertEq(numberOfConstraints[Alldistinct[_]](result), 1)
        assert(neighbourhood(result).isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_int_reif_test"))
        assertEq(numberOfConstraints[Alldistinct[_]](result), 2)
        assert(neighbourhood(result).isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExcept0Constraint]))
    def testAlldifferentExcept0(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_0_test"))
        assertEq(numberOfConstraints[AlldistinctExceptZero[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExcept0Constraint]))
    def testAlldifferentExcept0Reif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_0_reif_test"))
        assertEq(numberOfConstraints[AlldistinctExceptZero[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPacking(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_reif_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 2)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapa(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapaReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_reif_test"))
        // minizinc eliminates one of the bin_packing_load constraints :-)
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithUnboundedLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_unbounded_loads"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithEqualLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_equal_loads"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithSharedLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_shared_loads"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 2)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithEqualBins(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test_with_equal_bins"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 5)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_reif_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 2)
        assertEq(searchVariables(result).size, 9)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_fn_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 6)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint]))
    def testCircuit(): Unit = {
        val result = solveWithResult(task.copy(problemName = "circuit_test"))
        assertEq(numberOfConstraints[Circuit](result), 1)
        assert(neighbourhood(result).isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint]))
    def testCircuitReif(): Unit = {
        // Gecode does not provide a decomposition for circuit_reif, so we cannot verify the solution.
        val result = solveWithResult(task.copy(problemName = "circuit_reif_test", verifySolution = false))
        assertEq(numberOfConstraints[Circuit](result), 2)
        assert(neighbourhood(result).isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEq(): Unit = {
        val result = solveWithResult(task.copy(problemName = "count_eq_test"))
        assertEq(numberOfConstraints[CountConst[_]](result), 2)
        assertEq(numberOfConstraints[CountVar[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulative(): Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_test"))
        assertEq(numberOfConstraints[Cumulative](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulativeReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_reif_test"))
        assertEq(numberOfConstraints[Cumulative](result), 2)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveryWithWaiting(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_test_with_waiting", maybeOptimum = Some(378)))
        assertEq(numberOfConstraints[Circuit](result), 1)
        assertEq(numberOfConstraints[Delivery[_]](result), 1)
        assertEq(numberOfConstraints[Eq[_]](result), 0)
        assert(neighbourhood(result).isInstanceOf[CircuitNeighbourhood])
        assertEq(quality(result).asInstanceOf[IntegerValue].value, 378)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveryWithoutWaiting(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_test_without_waiting", dataAssignments = Map(("MaxKToMinKRatio", "1")), maybeOptimum = Some(669), maybeTargetObjectiveValue = Some(700)))
        assertEq(numberOfConstraints[Circuit](result), 1)
        assertEq(numberOfConstraints[Delivery[_]](result), 2)
        assertEq(numberOfConstraints[Eq[_]](result), 0)
        assert(neighbourhood(result).isInstanceOf[CircuitNeighbourhood])
        assertLe(quality(result).asInstanceOf[IntegerValue].value, 700)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveryReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_reif_test"))
        assertEq(numberOfConstraints[Circuit](result), 1)
        assertEq(numberOfConstraints[Delivery[_]](result), 1)
        assertEq(numberOfConstraints[Eq[_]](result), 0)
        assert(neighbourhood(result).isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveriesWithEqualArrivalTimes(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_test_with_equal_arrival_times", dataAssignments = Map(("MaxKToMinKRatio", "1"))))
        assertEq(numberOfConstraints[Circuit](result), 1)
        assertEq(numberOfConstraints[Delivery[_]](result), 2)
        assertEq(numberOfConstraints[Eq[_]](result), 1)
        assert(neighbourhood(result).isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveriesWithSharedArrivalTimes(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_test_with_shared_arrival_times"))
        assertEq(numberOfConstraints[Circuit](result), 1)
        assertEq(numberOfConstraints[Delivery[_]](result), 2)
        assertEq(numberOfConstraints[Eq[_]](result), 23)
        assert(neighbourhood(result).isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnNonstrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnNonstrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_reif_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnStrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnStrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_reif_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveNonstrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveNonstrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_reif_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveStrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveStrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_reif_test"))
        assertEq(numberOfConstraints[Disjoint2](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinality(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 4)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_reif_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 4)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_fn_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityClosedFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_closed_fn_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUp(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUpReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_reif_test"))
        assertEq(numberOfConstraints[BinPacking[_]](result), 1)
        assertEq(searchVariables(result).size, 3)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverse(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_test"))
        assertEq(numberOfConstraints[Inverse](result), 1)
        assert(neighbourhood(result).isInstanceOf[GeneralInverseNeighbourhood])
    }

    // This test verifies that Yuck's definition of fzn_inverse constrains the codomain
    // of one function to be a subset of the other function's domain.
    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseWithUnboundedSearchVariables(): Unit = {
        val result =
            solveWithResult(
                task.copy(
                    problemName = "inverse_test_with_unbounded_search_variables",
                    solverConfiguration = task.solverConfiguration.copy(runPresolver = false)))
        assertEq(numberOfConstraints[Inverse](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseDecomposition(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "inverse_decomposition_test"))
        assertEq(numberOfConstraints[Inverse](result), 2)
        assert(neighbourhood(result).isInstanceOf[NeighbourhoodCollection])
        assert(neighbourhood(result).asInstanceOf[NeighbourhoodCollection].children.forall(_.isInstanceOf[SimpleInverseNeighbourhood]))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_reif_test"))
        assertEq(numberOfConstraints[Inverse](result), 2)
        assert(neighbourhood(result).isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_test"))
        assertEq(numberOfConstraints[LexLess[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_reif_test"))
        assertEq(numberOfConstraints[LexLess[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_test"))
        assertEq(numberOfConstraints[LexLessEq[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_reif_test"))
        assertEq(numberOfConstraints[LexLessEq[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_test"))
        assertEq(numberOfConstraints[LexLess[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_reif_test"))
        assertEq(numberOfConstraints[LexLess[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_test"))
        assertEq(numberOfConstraints[LexLessEq[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_reif_test"))
        assertEq(numberOfConstraints[LexLessEq[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_set_test"))
        assertEq(numberOfConstraints[LexLess[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_set_reif_test"))
        assertEq(numberOfConstraints[LexLess[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_set_test"))
        assertEq(numberOfConstraints[LexLessEq[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_set_reif_test"))
        assertEq(numberOfConstraints[LexLessEq[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMaximumConstraint]))
    def testMaximum(): Unit = {
        solve(task.copy(problemName = "maximum_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMinimumConstraint]))
    def testMinimum(): Unit = {
        solve(task.copy(problemName = "minimum_int_test"))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "member_int_test"))
        assertEq(numberOfConstraints[CountVar[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "member_int_reif_test"))
        assertEq(numberOfConstraints[CountVar[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "member_bool_test"))
        assertEq(numberOfConstraints[CountVar[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "member_bool_reif_test"))
        assertEq(numberOfConstraints[CountVar[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "member_set_test"))
        assertEq(numberOfConstraints[CountVar[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "member_set_reif_test"))
        assertEq(numberOfConstraints[CountVar[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
    def testNValue(): Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_test"))
        assertEq(numberOfConstraints[NumberOfDistinctValues[_]](result), 1)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
    def testNValueReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_reif_test"))
        assertEq(numberOfConstraints[NumberOfDistinctValues[_]](result), 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def testRegular(): Unit = {
        val result = solveWithResult(task.copy(problemName = "regular_test"))
        assertEq(numberOfConstraints[Regular](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def testRegularReif(): Unit = {
        // Gecode does not provide a decomposition for regular_reif, so we cannot verify the solution.
        val result = solveWithResult(task.copy(problemName = "regular_reif_test", verifySolution = false))
        assertEq(numberOfConstraints[Regular](result), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_bool_test"))
        assertEq(numberOfConstraints[Table[_]](result), 1)
        assert(neighbourhood(result).isInstanceOf[TableNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_bool_reif_test"))
        assertEq(numberOfConstraints[Table[_]](result), 1)
        assert(neighbourhood(result).isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_int_test"))
        assertEq(numberOfConstraints[Table[_]](result), 1)
        assert(neighbourhood(result).isInstanceOf[TableNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_int_reif_test"))
        assertEq(numberOfConstraints[Table[_]](result), 1)
        assert(neighbourhood(result).isInstanceOf[RandomReassignmentGenerator])
    }

}
