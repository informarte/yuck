package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.*

import scala.language.implicitConversions
import scala.reflect.ClassTag

import yuck.constraints.*
import yuck.core.{given, *}
import yuck.flatzinc.compiler.{Bool2Costs2, LevelWeightMaintainer}
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
                task.solverConfiguration.copy(maybeName = Some("with-implicit-solving")))

    private val taskWithoutImplicitSolving =
        task.copy(
            solverConfiguration =
                task.solverConfiguration.copy(maybeName = Some("without-implicit-solving"), useImplicitSolving = false))

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentIntWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "alldifferent_int_test"))
        assertEq(result.space.numberOfConstraints[Alldistinct[_]], 1)
        assert(result.neighbourhood.isInstanceOf[AlldistinctNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentIntWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "alldifferent_int_test"))
        assertEq(result.space.numberOfConstraints[Alldistinct[_]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_int_reif_test"))
        assertEq(result.space.numberOfConstraints[Alldistinct[_]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_set_test"))
        assertEq(result.space.numberOfConstraints[Alldistinct[_]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def testAlldifferentSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_set_reif_test"))
        assertEq(result.space.numberOfConstraints[Alldistinct[_]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExceptConstraint]))
    def testAlldifferentExcept0(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_0_test"))
        assertEq(result.space.numberOfConstraints[AlldistinctExcept[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExceptConstraint]))
    def testAlldifferentExcept0Reif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_0_reif_test"))
        assertEq(result.space.numberOfConstraints[AlldistinctExcept[_]], 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExceptConstraint]))
    def testAlldifferentExcept(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_test"))
        assertEq(result.space.numberOfConstraints[AlldistinctExcept[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentExceptConstraint]))
    def testAlldifferentExceptReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "alldifferent_except_reif_test"))
        assertEq(result.space.numberOfConstraints[AlldistinctExcept[_]], 2)
    }

    // We test the bin_packing constraints very thoroughly because the Yuck bindings,
    // their interplay with the MiniZinc standard library, and the FlatZinc compiler are
    // all quite complex.

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPacking(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_reif_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 9)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        checkBinPackingReifConstraintNetwork[Le[_]](result)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapa(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingCapaReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_capa_reif_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 9)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        checkBinPackingReifConstraintNetwork[Le[_]](result)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoad(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithUnboundedLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_unbounded_loads_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 4)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 1)
        assertEq(result.space.numberOfConstraints, 2)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints(constraint => constraint.isInstanceOf[Conjunction] && constraint.inVariables.isEmpty), 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithEqualLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_equal_loads_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        assertEq(result.space.numberOfConstraints, 5)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[_]], 2)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithSharedLoads(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_shared_loads_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 13)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 10)
        assertEq(result.space.numberOfConstraints, 10)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[Eq[_]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadWithEqualBins(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_with_equal_bins_test"))
        assertEq(result.space.searchVariables.size, 5)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(isUserDefined), 3)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
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
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasBinPackingConstraint]))
    def testBinPackingLoadFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "bin_packing_load_fn_test"))
        assertEq(result.space.searchVariables.size, 6)
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        assertEq(result.space.numberOfConstraints, 5)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[_]], 2)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    private def checkBinPackingReifConstraintNetwork[T <: Constraint](result: Result)(using classTag: ClassTag[T]): Unit = {
        assertEq(result.space.numberOfConstraints, 8)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
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
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint]))
    def testCircuitWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "circuit_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint]))
    def testCircuitWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "circuit_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint]))
    def testCircuitReif(): Unit = {
        // Gecode does not provide a decomposition for circuit_reif, so we cannot verify the solution.
        val result = solveWithResult(task.copy(problemName = "circuit_reif_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[Circuit], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEqBool(): Unit = {
        testCount("count_eq_bool_test", EqRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEqBoolReif(): Unit = {
        testCountReif("count_eq_bool_reif_test", EqRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountFnBool(): Unit = {
        testCountFn("count_fn_bool_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEqInt(): Unit = {
        testCount("count_eq_int_test", EqRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEqIntReif(): Unit = {
        testCountReif("count_eq_int_reif_test", EqRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountFnInt(): Unit = {
        testCountFn("count_fn_int_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEqSet(): Unit = {
        testCount("count_eq_set_test", EqRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountEqSetReif(): Unit = {
        testCountReif("count_eq_set_reif_test", EqRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountFnSet(): Unit = {
        testCountFn("count_fn_set_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountNeqBool(): Unit = {
        testCount("count_neq_bool_test", NeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountNeqBoolReif(): Unit = {
        testCountReif("count_neq_bool_reif_test", NeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountNeqInt(): Unit = {
        testCount("count_neq_int_test", NeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountNeqIntReif(): Unit = {
        testCountReif("count_neq_int_reif_test", NeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountNeqSet(): Unit = {
        testCount("count_neq_set_test", NeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountNeqSetReif(): Unit = {
        testCountReif("count_neq_set_reif_test", NeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLeqBool(): Unit = {
        testCount("count_leq_bool_test", LeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLeqBoolReif(): Unit = {
        testCountReif("count_leq_bool_reif_test", LeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLeqInt(): Unit = {
        testCount("count_leq_int_test", LeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLeqIntReif(): Unit = {
        testCountReif("count_leq_int_reif_test", LeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLeqSet(): Unit = {
        testCount("count_leq_set_test", LeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLeqSetReif(): Unit = {
        testCountReif("count_leq_set_reif_test", LeRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLtBool(): Unit = {
        testCount("count_lt_bool_test", LtRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLtBoolReif(): Unit = {
        testCountReif("count_lt_bool_reif_test", LtRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLtInt(): Unit = {
        testCount("count_lt_int_test", LtRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLtIntReif(): Unit = {
        testCountReif("count_lt_int_reif_test", LtRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLtSet(): Unit = {
        testCount("count_lt_set_test", LtRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountLtSetReif(): Unit = {
        testCountReif("count_lt_set_reif_test", LtRelation)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGeqBool(): Unit = {
        testCount("count_geq_bool_test", LeRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGeqBoolReif(): Unit = {
        testCountReif("count_geq_bool_reif_test", LeRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGeqInt(): Unit = {
        testCount("count_geq_int_test", LeRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGeqIntReif(): Unit = {
        testCountReif("count_geq_int_reif_test", LeRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGeqSet(): Unit = {
        testCount("count_geq_set_test", LeRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGeqSetReif(): Unit = {
        testCountReif("count_geq_set_reif_test", LeRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGtBool(): Unit = {
        testCount("count_gt_bool_test", LtRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGtBoolReif(): Unit = {
        testCountReif("count_gt_bool_reif_test", LtRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGtInt(): Unit = {
        testCount("count_gt_int_test", LtRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGtIntReif(): Unit = {
        testCountReif("count_gt_int_reif_test", LtRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGtSet(): Unit = {
        testCount("count_gt_set_test", LtRelation, true)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCountConstraint]))
    def testCountGtSetReif(): Unit = {
        testCountReif("count_gt_set_reif_test", LtRelation, true)
    }

    private def testCount(problemName: String, relation: OrderingRelation, inverseRelation: Boolean = false): Unit = {
        // The MiniZinc library does not support set counting.
        val result = solveWithResult(task.copy(problemName = problemName, verificationFrequency = NoVerification))
        assertEq(result.space.channelVariables.size, 5)
        if (relation == EqRelation) {
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
        assertEq(result.space.numberOfConstraints[CountConst[_]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[_]], 1)
        relation match {
            case EqRelation => assertEq(result.space.numberOfConstraints[Contains], 1)
                               assertEq(result.space.numberOfConstraints[Eq[_]], 1)
            case NeRelation => assertEq(result.space.numberOfConstraints[Ne[_]], 2)
            case LeRelation => assertEq(result.space.numberOfConstraints[Le[_]], 2)
            case LtRelation => assertEq(result.space.numberOfConstraints[Lt[_]], 2)
        }
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        val xs = result.compilerResult.arrays("x")
        val ys = result.compilerResult.vars("y")
        val c = result.compilerResult.vars("c")
        val as = xs.map(result.assignment.value)
        val b = result.assignment.value(ys)
        val n = as.count(_ == b)
        val m = result.assignment.value(c.asInstanceOf[IntegerVariable]).toInt
        relation match {
            case EqRelation => assertEq(n, m)
            case NeRelation => assertNe(n, m)
            case LeRelation => if (inverseRelation) assertGe(m, n) else assertLe(m, n)
            case LtRelation => if (inverseRelation) assertGt(m, n) else assertLt(m, n)
        }
    }

    private def testCountReif(problemName: String, relation: OrderingRelation, inverseRelation: Boolean = false): Unit = {
        // The MiniZinc library does not support set counting.
        val result = solveWithResult(task.copy(problemName = problemName, verificationFrequency = NoVerification))
        assertEq(result.space.searchVariables.size, 12)
        assertEq(result.space.searchVariables.map(_.name).filterNot(_.startsWith("x")), Set("c[2]", "y[2]"))
        assertEq(result.space.channelVariables.size, 6)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("b[1]", "b[2]"))
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 4)
        assertEq(result.space.numberOfConstraints, 7)
        assertEq(result.space.numberOfConstraints[CountConst[_]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[_]], 1)
        relation match {
            case EqRelation => assertEq(result.space.numberOfConstraints[Eq[_]], 2)
            case NeRelation => assertEq(result.space.numberOfConstraints[Ne[_]], 2)
            case LeRelation => assertEq(result.space.numberOfConstraints[Le[_]], 2)
            case LtRelation => assertEq(result.space.numberOfConstraints[Lt[_]], 2)
        }
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        val xs = result.compilerResult.arrays("x")
        val ys = result.compilerResult.arrays("y")
        val cs = result.compilerResult.arrays("c")
        assertEq(ys.size, 2)
        assertEq(cs.size, 2)
        val as = xs.map(result.assignment.value)
        val bs = ys.map(result.assignment.value)
        val ns = bs.map(b => as.count(_ == b))
        val ms = cs.map(c => result.assignment.value(c.asInstanceOf[IntegerVariable]).toInt)
        relation match {
            case EqRelation =>
                assert(ns(0) == ms(0) || ns(1) == ms(1))
            case NeRelation =>
                assert(ns(0) != ms(0) || ns(1) != ms(1))
            case LeRelation =>
                if (inverseRelation) assert(ms(0) >= ns(0) || ms(1) >= ns(1))
                else assert(ms(0) <= ns(0) || ms(1) <= ns(1))
            case LtRelation =>
                if (inverseRelation) assert(ms(0) > ns(0) || ms(1) > ns(1))
                else assert(ms(0) < ns(0) || ms(1) < ns(1))
        }
    }

    private def testCountFn(problemName: String): Unit = {
        // The MiniZinc library does not support set counting.
        val result = solveWithResult(task.copy(problemName = problemName, verificationFrequency = NoVerification))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 0)
        assertEq(result.space.numberOfConstraints, 9)
        assertEq(result.space.numberOfConstraints[CountConst[_]], 2)
        assertEq(result.space.numberOfConstraints[CountVar[_]], 2)
        assertEq(result.space.numberOfConstraints[Contains], 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
        assertEq(result.space.numberOfConstraints[SumConstraint[_]], 1)
        val xs = result.compilerResult.arrays("x")
        val ys = result.compilerResult.arrays("y")
        val cs = result.compilerResult.arrays("c")
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
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulative(): Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_test"))
        assertEq(result.space.numberOfConstraints[Cumulative], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCumulativeConstraint]))
    def testCumulativeReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "cumulative_reif_test"))
        assertEq(result.space.numberOfConstraints[Cumulative], 2)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveryWithWaiting(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_with_waiting_test", maybeOptimum = Some(378)))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[_]], 1)
        assertEq(result.space.numberOfConstraints[Eq[_]], 0)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
        assertEq(result.quality.asInstanceOf[IntegerValue].value, 378)
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveryWithoutWaiting(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_without_waiting_test", dataAssignments = Map(("MaxKToMinKRatio", "1")), maybeOptimum = Some(669), maybeTargetObjectiveValue = Some(700)))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[_]], 2)
        assertEq(result.space.numberOfConstraints[Eq[_]], 0)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
        assertLe(result.quality.asInstanceOf[IntegerValue].value, 700L)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveryReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_reif_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[_]], 1)
        assertEq(result.space.numberOfConstraints[Eq[_]], 0)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveriesWithEqualArrivalTimes(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_with_equal_arrival_times_test", dataAssignments = Map(("MaxKToMinKRatio", "1"))))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[_]], 2)
        assertEq(result.space.numberOfConstraints[Eq[_]], 1)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasCircuitConstraint], classOf[HasDeliveryConstraint]))
    def testDeliveriesWithSharedArrivalTimes(): Unit = {
        val result = solveWithResult(task.copy(problemName = "delivery_with_shared_arrival_times_test"))
        assertEq(result.space.numberOfConstraints[Circuit], 1)
        assertEq(result.space.numberOfConstraints[Delivery[_]], 2)
        assertEq(result.space.numberOfConstraints[Eq[_]], 23)
        assert(result.neighbourhood.isInstanceOf[CircuitNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnNonstrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnNonstrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_nonstrict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnStrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDiffnConstraint]))
    def testDiffnStrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "diffn_strict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveNonstrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveNonstrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_nonstrict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveStrict(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasDisjunctiveConstraint]))
    def testDisjunctiveStrictReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "disjunctive_strict_reif_test"))
        assertEq(result.space.numberOfConstraints[Disjoint2], 1)
    }

    // We test global_cardinality constraints very thoroughly because both the interplay of
    // the Yuck bindings with the MiniZinc standard library and the FlatZinc compiler
    // (mapping to bin_packing) are quite complex.

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinality(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 7)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[_]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_reif_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 10)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 8)
        checkBinPackingReifConstraintNetwork[Eq[_]](result)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_fn_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 7)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[_]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityClosedFn(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_closed_fn_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 8)
        assertEq(result.space.channelVariables.count(isUserDefined), 1)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 7)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Eq[_]], 3)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUp(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 2)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 5)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[BinPacking[_]], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[Contains], 2)
        assertEq(result.space.numberOfConstraints[Eq[_]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint]))
    def testGlobalCardinalityLowUpReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "global_cardinality_low_up_reif_test"))
        assertEq(result.space.searchVariables.size, 3)
        assertEq(result.space.channelVariables.size, 9)
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 7)
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        checkBinPackingReifConstraintNetwork[Contains](result)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasIncreasingConstraint]))
    def testIncreasingNonstrictBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_bool_test"))
        assertEq(result.space.numberOfConstraints[Increasing[_, _]], 1)
        assert(result.neighbourhood.isInstanceOf[BooleanIncreasingNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasIncreasingConstraint]))
    def testIncreasingNonstrictBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[Increasing[_, _]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasIncreasingConstraint]))
    def testIncreasingNonstrictInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_int_test"))
        assertEq(result.space.numberOfConstraints[Increasing[_, _]], 1)
        assert(result.neighbourhood.isInstanceOf[IntegerIncreasingNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasIncreasingConstraint]))
    def testIncreasingNonstrictIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_nonstrict_int_reif_test"))
        assertEq(result.space.numberOfConstraints[Increasing[_, _]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasIncreasingConstraint]))
    def testIncreasingStrictInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_strict_int_test"))
        assertEq(result.space.numberOfConstraints[Increasing[_, _]], 1)
        assert(result.neighbourhood.isInstanceOf[IntegerIncreasingNeighbourhood])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasIncreasingConstraint]))
    def testIncreasingStrictIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "increasing_strict_int_reif_test"))
        assertEq(result.space.numberOfConstraints[Increasing[_, _]], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverse(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_test", miniZincCompilerRenamesVariables = true))
        assertEq(result.space.numberOfConstraints[Inverse], 1)
        assert(result.neighbourhood.isInstanceOf[GeneralInverseNeighbourhood])
    }

    // This test verifies that Yuck's definition of fzn_inverse constrains the codomain
    // of one function to be a subset of the other function's domain.
    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
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
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseDecomposition(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_decomposition_test"))
        assertEq(result.space.numberOfConstraints[Inverse], 2)
        assert(result.neighbourhood.isInstanceOf[NeighbourhoodCollection])
        assert(result.neighbourhood.asInstanceOf[NeighbourhoodCollection].children.forall(_.isInstanceOf[SimpleInverseNeighbourhood]))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasInverseConstraint]))
    def testInverseReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "inverse_reif_test", miniZincCompilerRenamesVariables = true))
        assertEq(result.space.numberOfConstraints[Inverse], 2)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_test"))
        assertEq(result.space.numberOfConstraints[LexLess[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLess[_]], 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqBool(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[_]], 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_test"))
        assertEq(result.space.numberOfConstraints[LexLess[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_int_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLess[_]], 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqInt(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_int_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[_]], 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_set_test"))
        assertEq(result.space.numberOfConstraints[LexLess[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessConstraint]))
    def testLexLessSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_less_set_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLess[_]], 2)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqSet(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_set_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[_]], 1)
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasLexLessEqConstraint]))
    def testLexLessEqSetReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "lex_lesseq_set_reif_test"))
        assertEq(result.space.numberOfConstraints[LexLessEq[_]], 2)
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
    def testMemberBool(): Unit = {
        testMember("member_bool_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberBoolReif(): Unit = {
        testMemberReif("member_bool_reif_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberInt(): Unit = {
        testMember("member_int_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberIntReif(): Unit = {
        testMemberReif("member_int_reif_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberSet(): Unit = {
        testMember("member_set_test")
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasMemberConstraint]))
    def testMemberSetReif(): Unit = {
        testMemberReif("member_set_reif_test")
    }

    private def testMember(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 0)
        assertEq(result.space.numberOfConstraints, 6)
        assertEq(result.space.numberOfConstraints[CountConst[_]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[_]], 1)
        assertEq(result.space.numberOfConstraints[Le[_]], 2)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    private def testMemberReif(problemName: String): Unit = {
        val result = solveWithResult(task.copy(problemName = problemName))
        assertEq(result.space.channelVariables.count(wasIntroducedByMiniZincCompiler), 0)
        assertEq(result.space.numberOfConstraints, 7)
        assertEq(result.space.numberOfConstraints[CountConst[_]], 1)
        assertEq(result.space.numberOfConstraints[CountVar[_]], 1)
        assertEq(result.space.numberOfConstraints[Le[_]], 2)
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
    def testNValue(): Unit = {
        val result = solveWithResult(task.copy(problemName = "nvalue_test"))
        assertEq(result.space.searchVariables.filterNot(wasIntroducedByYuck).map(_.name), Set("x[1]", "x[2]", "x[3]"))
        assertEq(result.space.channelVariables.size, 3)
        assertEq(result.space.channelVariables.filter(isUserDefined).map(_.name), Set("n"))
        assertEq(result.space.channelVariables.count(wasIntroducedByYuck), 2)
        assertEq(result.space.numberOfConstraints, 5)
        assertEq(result.space.numberOfConstraints[Conjunction], 1)
        assertEq(result.space.numberOfConstraints[LevelWeightMaintainer], 1)
        assertEq(result.space.numberOfConstraints[Lt[_]], 1)
        assertEq(result.space.numberOfConstraints[NumberOfDistinctValues[_]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
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
        assertEq(result.space.numberOfConstraints[Eq[_]], 1)
        assertEq(result.space.numberOfConstraints[LevelWeightMaintainer], 1)
        assertEq(result.space.numberOfConstraints[Lt[_]], 1)
        assertEq(result.space.numberOfConstraints[NumberOfDistinctValues[_]], 2)
        assertEq(result.space.numberOfConstraints[Or], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasNValueConstraint]))
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
        assertEq(result.space.numberOfConstraints[Lt[_]], 1)
        assertEq(result.space.numberOfConstraints[NumberOfDistinctValues[_]], 2)
        assertEq(result.space.numberOfConstraints[Plus[_]], 1)
        assertEq(result.space.numberOfConstraints[SatisfactionGoalTracker], 1)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasRegularConstraint]))
    def testRegularWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "regular_test", maybeOptimum = Some(12)))
        assertEq(result.space.numberOfConstraints[Regular], 1)
        assert(result.neighbourhood.isInstanceOf[RegularNeighbourhood])
        assertEq(result.quality, IntegerValue(12))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasRegularConstraint]))
    def testRegularWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "regular_test", maybeOptimum = Some(12)))
        assertEq(result.space.numberOfConstraints[Regular], 1)
        assert(result.neighbourhood.isInstanceOf[NeighbourhoodCollection])
        assert(result.neighbourhood.children.head.isInstanceOf[RandomReassignmentGenerator])
        assertEq(result.quality, IntegerValue(12))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def testRegularReif(): Unit = {
        // Gecode does not provide a decomposition for regular_reif, so we cannot verify the solution.
        val result = solveWithResult(task.copy(problemName = "regular_reif_test", verificationFrequency = NoVerification))
        assertEq(result.space.numberOfConstraints[Regular], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableBoolWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "table_bool_test"))
        assertEq(result.space.numberOfConstraints[Table[_]], 1)
        assert(result.neighbourhood.isInstanceOf[TableNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableBoolWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "table_bool_test"))
        assertEq(result.space.numberOfConstraints[Table[_]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableBoolReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_bool_reif_test"))
        assertEq(result.space.numberOfConstraints[Table[_]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableIntWithImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithImplicitSolving.copy(problemName = "table_int_test"))
        assertEq(result.space.numberOfConstraints[Table[_]], 1)
        assert(result.neighbourhood.isInstanceOf[TableNeighbourhood[_]])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableIntWithoutImplicitSolving(): Unit = {
        val result = solveWithResult(taskWithoutImplicitSolving.copy(problemName = "table_int_test"))
        assertEq(result.space.numberOfConstraints[Table[_]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasTableConstraint]))
    def testTableIntReif(): Unit = {
        val result = solveWithResult(task.copy(problemName = "table_int_reif_test"))
        assertEq(result.space.numberOfConstraints[Table[_]], 1)
        assert(result.neighbourhood.isInstanceOf[RandomReassignmentGenerator])
    }

}
