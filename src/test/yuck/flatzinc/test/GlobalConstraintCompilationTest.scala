package yuck.flatzinc.test

import scala.language.implicitConversions
import scala.reflect.ClassTag

import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}
import org.junit.jupiter.api.{Tag, Test}

import yuck.constraints.*
import yuck.constraints.OrderingRelation.*
import yuck.core.*
import yuck.flatzinc.compiler.{Bool2Costs2, LevelWeightMaintainer}
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.HasGlobalConstraint.*
import yuck.flatzinc.test.util.ProblemType.*
import yuck.flatzinc.test.util.VerificationFrequency.*

/**
 * Tests to make sure that the global constraints provided by Yuck's library get
 * compiled correctly
 */
@Execution(ExecutionMode.CONCURRENT)
final class GlobalConstraintCompilationTest extends FrontEndTest {

    private val taskWithImplicitSolving =
        task.copy(
            solverConfiguration =
                task.solverConfiguration.copy(
                    name = "with-implicit-solving",
                    annealingConfiguration =
                        task.solverConfiguration.annealingConfiguration.copy(
                            useImplicitSolving = true)))

    private val taskWithoutImplicitSolving =
        task.copy(
            solverConfiguration =
                task.solverConfiguration.copy(
                    name = "without-implicit-solving",
                    annealingConfiguration =
                        task.solverConfiguration.annealingConfiguration.copy(
                            startTemperature = 0.01,
                            useImplicitSolving = false)))

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentConstraint)
    def testAllDifferentIntWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "all_different_int_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[AllDifferentNeighbourhood[?, ?, ?]])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentConstraint)
    def testAllDifferentIntWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "all_different_int_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentConstraint)
    def testAllDifferentIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_int_reif_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentConstraint)
    def testAllDifferentSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_set_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentConstraint)
    def testAllDifferentSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_set_reif_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasAllDifferentExceptConstraint)
    def testAllDifferentExcept0WithImplicitSolving(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_except_0_test", maybeOptimum = Some(14)))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[NeighbourhoodCollection])
        assert(result.neighbourhood.asInstanceOf[NeighbourhoodCollection].children.head.isInstanceOf[AllDifferentNeighbourhood[?, ?, ?]])
        assert(result.searchWasPerformed)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentExceptConstraint)
    def testAllDifferentExcept0WithoutImplicitSolving(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_except_0_with_duplicate_variable_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentExceptConstraint)
    def testAllDifferentExcept0Reif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_except_0_reif_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentExceptConstraint)
    def testAllDifferentExceptWithImplicitSolving(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_except_test", maybeOptimum = Some(26)))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[NeighbourhoodCollection])
        assert(result.neighbourhood.asInstanceOf[NeighbourhoodCollection].children.head.isInstanceOf[AllDifferentNeighbourhood[?, ?, ?]])
        assert(result.searchWasPerformed)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentExceptConstraint)
    def testAllDifferentExceptWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_except_with_duplicate_variable_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasAllDifferentExceptConstraint)
    def testAllDifferentExceptReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "all_different_except_reif_test"))
        assertEq(result.space.numberOfConstraints[AllDifferent[?, ?, ?]], 2)
    }

    // We test the bin_packing constraints very thoroughly because the Yuck bindings,
    // their interplay with the MiniZinc standard library, and the FlatZinc compiler are
    // all quite complex.

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPacking(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_reif_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 9)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        checkBinPackingReifConstraintNetwork[Le[?, ?, ?]](result)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingCapa(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingCapaReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_reif_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 9)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        checkBinPackingReifConstraintNetwork[Le[?, ?, ?]](result)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingLoad(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingLoadWithUnboundedLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_unbounded_loads_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 4)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 1)
        assertEq(result.space.numberOfConstraints, 2)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints(constraint => constraint.isInstanceOf[Conjunction] && constraint.inVariables.isEmpty), 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingLoadWithEqualLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_equal_loads_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        assertEq(result.space.numberOfConstraints, 5)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingLoadWithSharedLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_shared_loads_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 13)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 10)
        assertEq(result.space.numberOfConstraints, 10)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingLoadWithEqualBins(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_equal_bins_test"))
        assertEq(result.space.searchVariables.size, 5)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingLoadReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_reif_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 9)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        checkBinPackingReifConstraintNetwork[Contains](result)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasBinPackingConstraint)
    def testBinPackingLoadFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_fn_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        assertEq(result.space.numberOfConstraints, 5)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    private def checkBinPackingReifConstraintNetwork[T <: Constraint](result: Result)(using classTag: ClassTag[T]): Unit = {
        assertEq(result.space.numberOfConstraints, 8)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Bool2Costs2], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 2)
        assertEq(
            result.space.numberOfConstraints(constraint =>
                constraint.isInstanceOf[Conjunction] &&
                    constraint.inVariables.size == 3 &&
                    constraint.inVariables.forall(x => classTag.runtimeClass.isInstance(result.space.definingConstraint(x))) &&
                    constraint.outVariables.size == 1 &&
                    result.space.directlyAffectedConstraints(constraint.outVariables.head).forall(_.isInstanceOf[Bool2Costs2])),
            1)
        assertEq(
            result.space.numberOfConstraints(constraint =>
                constraint.isInstanceOf[Conjunction] &&
                    constraint.inVariables.size == 1 &&
                    result.space.definingConstraint(constraint.inVariables.head).isInstanceOf[Bool2Costs2] &&
                    constraint.outVariables.toSeq == result.objective.objectiveVariables),
            1)
        assertEq(result.space.numberOfConstraints(classTag.runtimeClass.isInstance), 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCircuitConstraint)
    def testCircuitWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "circuit_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCircuitConstraint)
    def testCircuitWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "circuit_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCircuitConstraint)
    def testCircuitReif(): Unit = {
        // Gecode does not provide a decomposition for circuit_reif, so we cannot verify the solution.
        val result = solveWithResult(task.copy(problemName = "circuit_reif_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[Circuit], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountEqBool(): Unit = {
        testCount("count_eq_bool_test", EqRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountEqBoolReif(): Unit = {
        testCountReif("count_eq_bool_reif_test", EqRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountFnBool(): Unit = {
        testCountFn("count_fn_bool_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountEqInt(): Unit = {
        testCount("count_eq_int_test", EqRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountEqIntReif(): Unit = {
        testCountReif("count_eq_int_reif_test", EqRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountFnInt(): Unit = {
        testCountFn("count_fn_int_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountEqSet(): Unit = {
        testCount("count_eq_set_test", EqRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountEqSetReif(): Unit = {
        testCountReif("count_eq_set_reif_test", EqRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountFnSet(): Unit = {
        testCountFn("count_fn_set_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountNeqBool(): Unit = {
        testCount("count_neq_bool_test", NeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountNeqBoolReif(): Unit = {
        testCountReif("count_neq_bool_reif_test", NeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountNeqInt(): Unit = {
        testCount("count_neq_int_test", NeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountNeqIntReif(): Unit = {
        testCountReif("count_neq_int_reif_test", NeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountNeqSet(): Unit = {
        testCount("count_neq_set_test", NeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountNeqSetReif(): Unit = {
        testCountReif("count_neq_set_reif_test", NeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLeqBool(): Unit = {
        testCount("count_leq_bool_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLeqBoolReif(): Unit = {
        testCountReif("count_leq_bool_reif_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLeqInt(): Unit = {
        testCount("count_leq_int_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLeqIntReif(): Unit = {
        testCountReif("count_leq_int_reif_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLeqSet(): Unit = {
        testCount("count_leq_set_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLeqSetReif(): Unit = {
        testCountReif("count_leq_set_reif_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLtBool(): Unit = {
        testCount("count_lt_bool_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLtBoolReif(): Unit = {
        testCountReif("count_lt_bool_reif_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLtInt(): Unit = {
        testCount("count_lt_int_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLtIntReif(): Unit = {
        testCountReif("count_lt_int_reif_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLtSet(): Unit = {
        testCount("count_lt_set_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountLtSetReif(): Unit = {
        testCountReif("count_lt_set_reif_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGeqBool(): Unit = {
        testCount("count_geq_bool_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGeqBoolReif(): Unit = {
        testCountReif("count_geq_bool_reif_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGeqInt(): Unit = {
        testCount("count_geq_int_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGeqIntReif(): Unit = {
        testCountReif("count_geq_int_reif_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGeqSet(): Unit = {
        testCount("count_geq_set_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGeqSetReif(): Unit = {
        testCountReif("count_geq_set_reif_test", LeRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGtBool(): Unit = {
        testCount("count_gt_bool_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGtBoolReif(): Unit = {
        testCountReif("count_gt_bool_reif_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGtInt(): Unit = {
        testCount("count_gt_int_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGtIntReif(): Unit = {
        testCountReif("count_gt_int_reif_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGtSet(): Unit = {
        testCount("count_gt_set_test", LtRelation)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCountConstraint)
    def testCountGtSetReif(): Unit = {
        testCountReif("count_gt_set_reif_test", LtRelation)
    }

    private def testCount(problemName: String, relation: OrderingRelation): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.channelVariables.size, 5)
        if relation == EqRelation then {
            assertEq(result.space.searchVariables.size, 11)
            assertEq(result.space.searchVariables.map(_.name).filterNot(_.startsWith("x")), Set("y"))
            assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("c"))
            assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        } else {
            assertEq(result.space.searchVariables.size, 12)
            assertEq(result.space.searchVariables.map(_.name).filterNot(_.startsWith("x")), Set("c", "y"))
            assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        }
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[CountConst[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[?, ?, ?]], 1)
        relation match {
            case EqRelation => assertEq(result.space.numberOfConstraints[Contains], 1)
                               assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 1)
            case NeRelation => assertEq(result.space.numberOfConstraints[Ne[?, ?, ?]], 2)
            case LeRelation => assertEq(result.space.numberOfConstraints[Le[?, ?, ?]], 2)
            case LtRelation => assertEq(result.space.numberOfConstraints[Lt[?, ?, ?]], 2)
        }
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    private def testCountReif(problemName: String, relation: OrderingRelation): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.searchVariables.size, 12)
        assertEq(result.space.searchVariables.map(_.name).filterNot(_.startsWith("x")), Set("c[2]", "y[2]"))
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("b[1]", "b[2]"))
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 7)
        assertEq(result.space.numberOfConstraints[CountConst[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[?, ?, ?]], 1)
        relation match {
            case EqRelation => assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 2)
            case NeRelation => assertEq(result.space.numberOfConstraints[Ne[?, ?, ?]], 2)
            case LeRelation => assertEq(result.space.numberOfConstraints[Le[?, ?, ?]], 2)
            case LtRelation => assertEq(result.space.numberOfConstraints[Lt[?, ?, ?]], 2)
        }
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    private def testCountFn(problemName: String): Unit = {
        // The MiniZinc library does not fully support set counting.
        val result = solveWithResult(task.copy(problemName = problemName, verificationFrequency = NoVerification))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 0)
        assertEq(result.space.numberOfConstraints, 9)
        assertEq(result.space.numberOfConstraints[CountConst[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[CountVar[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Contains], 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        assertEq(result.space.numberOfConstraints[SumConstraint[?, ?, ?]], 1)
        val xs = result.outputArray("x")
        val ys = result.outputArray("y")
        val cs = result.outputArray("c")
        assertEq(ys.size, 4)
        assertEq(cs.size, 4)
        val as = xs.map(result.assignment.value)
        val bs = ys.map(result.assignment.value)
        val ns = bs.map(b => as.count(_ == b))
        val ms = cs.map(c => result.assignment.value(c.asInstanceOf[IntegerVariable]).toInt)
        assertEq(ns(0), ms(0))
        assertEq(ns(1), ms(1))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCumulativeConstraint)
    def testCumulative(): Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_test"))
        assertEq(result.space.numberOfConstraints[Cumulative], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCumulativeConstraint)
    def testCumulativeReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_reif_test"))
        assertEq(result.space.numberOfConstraints[Cumulative], 2)
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasCircuitConstraint)
    @Tag(HasDeliveryConstraint)
    def testDeliveryWithWaiting(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_with_waiting_test", maybeOptimum = Some(378)))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 0)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
        assertEq(result.quality.asInstanceOf[IntegerValue].value, 378)
    }

    @Test
    @Tag(MinimizationProblem)
    @Tag(HasCircuitConstraint)
    @Tag(HasDeliveryConstraint)
    def testDeliveryWithoutWaiting(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_without_waiting_test", dataAssignments = Map(("MaxKToMinKRatio", "1")), maybeOptimum = Some(669), maybeTargetObjectiveValue = Some(705)))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 0)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
        assertLe(result.quality.asInstanceOf[IntegerValue].value, 705L)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCircuitConstraint)
    @Tag(HasDeliveryConstraint)
    def testDeliveryReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_reif_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 0)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCircuitConstraint)
    @Tag(HasDeliveryConstraint)
    def testDeliveriesWithEqualArrivalTimes(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_with_equal_arrival_times_test", dataAssignments = Map(("MaxKToMinKRatio", "1"))))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasCircuitConstraint)
    @Tag(HasDeliveryConstraint)
    def testDeliveriesWithSharedArrivalTimes(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_with_shared_arrival_times_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 23)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDiffnConstraint)
    def testDiffnNonstrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDiffnConstraint)
    def testDiffnNonstrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDiffnConstraint)
    def testDiffnStrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDiffnConstraint)
    def testDiffnStrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctiveNonstrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctiveNonstrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctiveStrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasDisjunctiveConstraint)
    def testDisjunctiveStrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    // We test global_cardinality constraints very thoroughly because both the interplay of
    // the Yuck bindings with the MiniZinc standard library and the FlatZinc compiler
    // (mapping to bin_packing) are quite complex.

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinality(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 7)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinalityReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_reif_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 10)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 8)
        checkBinPackingReifConstraintNetwork[Eq[?, ?, ?]](result)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinalityFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_fn_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 7)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinalityClosedFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_closed_fn_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 7)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinalityLowUp(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 2)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasGlobalCardinalityConstraint)
    def testGlobalCardinalityLowUpReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_reif_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 9)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        checkBinPackingReifConstraintNetwork[Contains](result)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasIncreasingConstraint)
    def testIncreasingNonstrictBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_bool_test"))
        assertEq(result.space.numberOfConstraints[Increasing[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[BooleanIncreasingNeighbourhood])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasIncreasingConstraint)
    def testIncreasingNonstrictBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[Increasing[?, ?, ?]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasIncreasingConstraint)
    def testIncreasingNonstrictInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_int_test"))
        assertEq(result.space.numberOfConstraints[Increasing[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[IntegerIncreasingNeighbourhood])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasIncreasingConstraint)
    def testIncreasingNonstrictIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_int_reif_test"))
        assertEq(result.space.numberOfConstraints[Increasing[?, ?, ?]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasIncreasingConstraint)
    def testIncreasingStrictInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_strict_int_test"))
        assertEq(result.space.numberOfConstraints[Increasing[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[IntegerIncreasingNeighbourhood])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasIncreasingConstraint)
    def testIncreasingStrictIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_strict_int_reif_test"))
        assertEq(result.space.numberOfConstraints[Increasing[?, ?, ?]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasInverseConstraint)
    def testInverse(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_test", miniZincCompilerRenamesVariables = true))
        assertEq(result.space.numberOfConstraints[Inverse], 1)
        assert(result.neighbourhood.isInstanceOf[GeneralInverseNeighbourhood])
    }

    // This test verifies that Yuck's definition of fzn_inverse constrains the codomain
    // of one function to be a subset of the other function's domain.
    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasInverseConstraint)
    def testInverseWithUnboundedSearchVariables(): Unit = {
        val result =
            solveWithResult(
                task.copy(
                    problemName = "inverse_with_unbounded_search_variables_test",
                    solverConfiguration = task.solverConfiguration.copy(runPresolver = false),
                    miniZincCompilerRenamesVariables = true))
        assertEq(result.space.numberOfConstraints[Inverse], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasInverseConstraint)
    def testInverseDecomposition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_decomposition_test"))
        assertEq(result.space.numberOfConstraints[Inverse], 2)
        assert(result.neighbourhood.isInstanceOf[NeighbourhoodCollection])
        assert(result.neighbourhood.asInstanceOf[NeighbourhoodCollection].children.forall(_.isInstanceOf[SimpleInverseNeighbourhood]))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasInverseConstraint)
    def testInverseReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_reif_test", miniZincCompilerRenamesVariables = true))
        assertEq(result.space.numberOfConstraints[Inverse], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessConstraint)
    def testLexLessBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_test"))
        assertEq(result.space.numberOfConstraints[LexLess[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessConstraint)
    def testLexLessBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLess[?, ?, ?]], 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessEqConstraint)
    def testLexLessEqBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessEqConstraint)
    def testLexLessEqBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[?, ?, ?]], 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessConstraint)
    def testLexLessInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_test"))
        assertEq(result.space.numberOfConstraints[LexLess[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessConstraint)
    def testLexLessIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLess[?, ?, ?]], 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessEqConstraint)
    def testLexLessEqInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessEqConstraint)
    def testLexLessEqIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[?, ?, ?]], 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessConstraint)
    def testLexLessSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_set_test"))
        assertEq(result.space.numberOfConstraints[LexLess[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessConstraint)
    def testLexLessSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_set_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLess[?, ?, ?]], 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessEqConstraint)
    def testLexLessEqSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_set_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[?, ?, ?]], 1)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasLexLessEqConstraint)
    def testLexLessEqSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_set_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[?, ?, ?]], 2)
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMaximumConstraint)
    def testMaximum(): Unit = {
        solve(task.copy(problemName = "maximum_int_test"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMinimumConstraint)
    def testMinimum(): Unit = {
        solve(task.copy(problemName = "minimum_int_test"))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMemberConstraint)
    def testMemberBool(): Unit = {
        testMember("member_bool_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMemberConstraint)
    def testMemberBoolReif(): Unit = {
        testMemberReif("member_bool_reif_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMemberConstraint)
    def testMemberInt(): Unit = {
        testMember("member_int_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMemberConstraint)
    def testMemberIntReif(): Unit = {
        testMemberReif("member_int_reif_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMemberConstraint)
    def testMemberSet(): Unit = {
        testMember("member_set_test")
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasMemberConstraint)
    def testMemberSetReif(): Unit = {
        testMemberReif("member_set_reif_test")
    }

    private def testMember(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 0)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[CountConst[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Le[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    private def testMemberReif(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 0)
        assertEq(result.space.numberOfConstraints, 7)
        assertEq(result.space.numberOfConstraints[CountConst[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[Le[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasNValueConstraint)
    def testNValue(): Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_test"))
        assertEq(result.space.searchVariables.filterNot(wasIntroducedByYuck).map(_.name), Set("x[1]", "x[2]", "x[3]"))
        assertEq(result.space.channelVariables.size, 3)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("n"))
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 5)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[LevelWeightMaintainer], 1)
        assertEq(result.space.numberOfConstraints[Lt[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[NumberOfDistinctValues[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasNValueConstraint)
    def testNValueReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_reif_test"))
        assertEq(
            result.space.searchVariables.filterNot(wasIntroducedByYuck).map(_.name),
            Set("x[1]", "x[2]", "x[3]"))
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("n"))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        assertEq(result.space.numberOfConstraints, 10)
        assertEq(result.space.numberOfConstraints[Conjunction], 3)
        assertEq(result.space.numberOfConstraints[Eq[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[LevelWeightMaintainer], 1)
        assertEq(result.space.numberOfConstraints[Lt[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[NumberOfDistinctValues[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasNValueConstraint)
    def testNValueFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_fn_test"))
        assertEq(
            result.space.searchVariables.filterNot(wasIntroducedByYuck).map(_.name),
            Set("x[1]", "x[2]", "x[3]", "y[1]", "y[2]", "y[3]"))
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("m", "n"))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 3)
        assertEq(result.space.numberOfConstraints, 8)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(
            result.space.numberOfConstraints(
                constraint => constraint.isInstanceOf[Contains] && constraint.inVariables.count(_.name == "m") == 1),
            1)
        assertEq(result.space.numberOfConstraints[LevelWeightMaintainer], 1)
        assertEq(result.space.numberOfConstraints[Lt[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[NumberOfDistinctValues[?, ?, ?]], 2)
        assertEq(result.space.numberOfConstraints[Plus[?, ?, ?]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasRegularConstraint)
    def testRegularWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "regular_test", maybeOptimum = Some(12)))
        assertEq(result.space.numberOfConstraints[Regular], 1)
        assert(result.neighbourhood.isInstanceOf[RegularNeighbourhood])
        assertEq(result.quality, IntegerValue(12))
    }

    @Test
    @Tag(MaximizationProblem)
    @Tag(HasRegularConstraint)
    def testRegularWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "regular_test", maybeOptimum = Some(12)))
        assertEq(result.space.numberOfConstraints[Regular], 1)
        assert(result.neighbourhood.isInstanceOf[NeighbourhoodCollection])
        assert(result.neighbourhood.children.head.isInstanceOf[RandomReassignmentGenerator])
        assertEq(result.quality, IntegerValue(12))
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasRegularConstraint)
    def testRegularReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "regular_reif_test"))
        assertEq(result.space.numberOfConstraints[Regular], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasTableConstraint)
    def testTableBoolWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "table_bool_test"))
        assertEq(result.space.numberOfConstraints[Table[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[TableNeighbourhood[?, ?, ?]])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasTableConstraint)
    def testTableBoolWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "table_bool_test"))
        assertEq(result.space.numberOfConstraints[Table[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasTableConstraint)
    def testTableBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[Table[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasTableConstraint)
    def testTableIntWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "table_int_test"))
        assertEq(result.space.numberOfConstraints[Table[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[TableNeighbourhood[?, ?, ?]])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasTableConstraint)
    def testTableIntWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "table_int_test"))
        assertEq(result.space.numberOfConstraints[Table[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Tag(SatisfiabilityProblem)
    @Tag(HasTableConstraint)
    def testTableIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_int_reif_test"))
        assertEq(result.space.numberOfConstraints[Table[?, ?, ?]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

}
