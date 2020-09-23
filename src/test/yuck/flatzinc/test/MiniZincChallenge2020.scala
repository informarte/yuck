package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2020
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenge2020 extends MiniZincBasedTest {

    private val task =
        MiniZincTestTask(
            directoryLayout = StandardMiniZincBenchmarksLayout,
            suitePath = "resources/mzn/tests/minizinc-benchmarks",
            suiteName = "mznc20")

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def bnn_planner_sysadmin_4_2s: Unit = {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "bnn-planner", instanceName = "sysadmin_4_2s", maybeOptimum = Some(0)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint]))
    def cable_tree_wiring_A031: Unit = {
        solve(task.copy(problemName = "cable-tree-wiring", modelName = "ctw", instanceName = "A031", maybeOptimum = Some(24104)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCumulativeConstraint], classOf[HasDecreasingConstraint], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint], classOf[HasMinimumConstraint], classOf[HasTableConstraint]))
    def code_generator_mips_gcc_cfgbuild_control_flow_insn_p: Unit = {
        solve(task.copy(problemName = "code-generator", modelName = "unison", instanceName = "mips_gcc.cfgbuild.control_flow_insn_p", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(274663947)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def collaborative_construction_37: Unit = {
        solve(task.copy(problemName = "collaborative-construction", modelName = "macc", instanceName = "37", maybeMaximumNumberOfThreads = Some(1), maybeOptimum = Some(9)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasGlobalCardinalityConstraint]))
    def gbac_reduced_UD2: Unit = {
        solve(task.copy(problemName = "gbac", instanceName = "reduced_UD2-gbac", maybeOptimum = Some(401)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def hoist_scheduling_PU_1_2_2: Unit = {
        solve(task.copy(problemName = "hoist-scheduling", modelName = "hoist-benchmark", instanceName = "PU_1_2_2", maybeOptimum = Some(221)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasTableConstraint]))
    def is_v1HjuSBQMb: Unit = {
        solve(task.copy(problemName = "is", modelName = "model", instanceName = "v1HjuSBQMb", maybeOptimum = Some(454656), maybeRuntimeLimitInSeconds = Some(420)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasAtLeastConstraint], classOf[HasAtMostConstraint], classOf[HasGlobalCardinalityConstraint]))
    def lot_sizing_pigment15b_psp: Unit = {
        solve(task.copy(problemName = "lot-sizing", modelName = "lot_sizing_cp", instanceName = "pigment15b.psp", maybeOptimum = Some(1123)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def minimal_decision_sets_backache_train4: Unit = {
        solve(task.copy(problemName = "minimal-decision-sets", modelName = "sparse_mds", instanceName = "backache_train4", maybeOptimum = Some(34)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasCircuitConstraint], classOf[HasInverseConstraint]))
    def p1f_10: Unit = {
        solve(task.copy(problemName = "p1f", instanceName = "10", maybeOptimum = Some(300)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def pentominoes_int_06: Unit = {
        solve(task.copy(problemName = "pentominoes", modelName = "pentominoes-int", instanceName = "06"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint]))
    def pillars_and_planks: Unit = {
        solve(task.copy(problemName = "pillars-and-planks", modelName = "pillars-planks-solution", instanceName = "p-d12", maybeOptimum = Some(5)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def racp_j30_26_2_1_0: Unit = {
        solve(task.copy(problemName = "racp", instanceName = "j30_26_2_1.0", maybeOptimum = Some(704)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def radiation_i6_9: Unit = {
        solve(task.copy(problemName = "radiation", instanceName = "i6-9", maybeOptimum = Some(338)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def sdn_chain_d10n780_1: Unit = {
        solve(task.copy(problemName = "sdn-chain", modelName = "model", instanceName = "d10n780-1", maybeOptimum = Some(10)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def skill_allocation_1m_1: Unit = {
        solve(task.copy(problemName = "skill-allocation", modelName = "skill_allocation_only", instanceName = "skill_allocation_mzn_1m_1", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def soccer_computational_xIGData_24_24_16_1: Unit = {
        solve(task.copy(problemName = "soccer-computational", modelName = "ecp", instanceName = "xIGData_24_24_16_1"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def stable_goods_s_d6: Unit = {
        solve(task.copy(problemName = "stable-goods", modelName = "stable-goods-solution", instanceName = "s-d6", maybeOptimum = Some(6264)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def tower_challenge_070_070_15_070_04: Unit = {
        solve(task.copy(problemName = "tower-challenge", modelName = "tower", instanceName = "tower_070_070_15_070-04", maybeOptimum = Some(61)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def whirlpool_d8: Unit = {
        solve(task.copy(problemName = "whirlpool", modelName = "whirlpool-x", instanceName = "d8"))
    }

}
