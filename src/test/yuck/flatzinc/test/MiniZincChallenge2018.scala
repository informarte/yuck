package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2018
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenge2018 extends MiniZincBasedTest {

    private val task = MiniZincTestTask(directoryLayout = StandardMiniZincBenchmarksLayout, suitePath = "resources/mzn/benchmarks", suiteName = "mznc18")

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def cargo_challenge_22 {
        solve(task.copy(problemName = "cargo", modelName = "cargo_coarsePiles", instanceName = "challenge22", maybeOptimum = Some(884)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def concert_hall_cap_mznc2018_02 {
        solve(task.copy(problemName = "concert-hall-cap", instanceName = "concert-cap.mznc2018.02", maybeOptimum = Some(48278)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentConstraint], classOf[HasInverseConstraint], classOf[HasMemberConstraint], classOf[HasRegularConstraint]))
    def elitserien_handball1 {
        solve(task.copy(problemName = "elitserien", modelName = "handball", instanceName = "handball1", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAtMostConstraint], classOf[HasCumulativeConstraint], classOf[HasNValueConstraint]))
    def gfd_schedule_n65f2d50m20k10 {
        solve(task.copy(problemName = "gfd-schedule", modelName = "gfd-schedule2", instanceName = "n65f2d50m20k10", maybeOptimum = Some(19)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def largescheduling_instance_0100_4 {
        solve(task.copy(problemName = "largescheduling", modelName = "largecumulative", instanceName = "instance-0400-4", maybeHighScore = Some(220428)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasBinPackingConstraint], classOf[HasCountConstraint], classOf[HasMaximumConstraint], classOf[HasNetworkFlowCostConstraint]))
    def mapping_full2x2_mp3 {
        solve(task.copy(problemName = "mapping", instanceName = "full2x2_mp3", maybeOptimum = Some(1100)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def neighbours_1 {
        solve(task.copy(problemName = "neighbours", modelName = "neighbours-rect", instanceName = "neighbours1", maybeOptimum = Some(63)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def on_call_rostering_2s_200d {
        solve(task.copy(problemName = "on-call-rostering", modelName = "oc-roster", instanceName = "2s-200d", maybeOptimum = Some(63)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasIncreasingConstraint]))
    def oocsp_racks_050_r1 {
        solve(task.copy(problemName = "oocsp_racks", instanceName = "oocsp_racks_050_r1", maybeMaximumNumberOfThreads = Some(1)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def opt_cryptanalysis_r10 {
        solve(task.copy(problemName = "opt-cryptanalysis", modelName = "mznc2017_aes_opt", instanceName = "r10", maybeOptimum = Some(41)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasTableConstraint]))
    def proteindesign12_1PGB_11p_9aa_usingEref_self_x {
        solve(task.copy(problemName = "proteindesign12", modelName = "wcsp", instanceName = "1PGB.11p.9aa.usingEref_self_x", maybeOptimum = Some(4151)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def racp_j30_13_6_1_25 {
        solve(task.copy(problemName = "racp", instanceName = "j30_13_6_1.25", maybeOptimum = Some(401)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasGlobalCardinalityConstraint], classOf[HasRegularConstraint]))
    def rotating_workforce_Example103 {
        solve(task.copy(problemName = "rotating-workforce", instanceName = "Example103"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasAlldifferentExcept0Constraint]))
    def seat_moving_10_20_05 {
        solve(task.copy(problemName = "seat-moving", instanceName = "sm-10-20-05", maybeOptimum = Some(90)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasAlldifferentConstraint]))
    def soccer_computational_xIGData_22_12_22_5 {
        solve(task.copy(problemName = "soccer-computational", modelName = "ecp", instanceName = "xIGData_22_12_22_5"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def steiner_tree_es10fst03_stp {
        solve(task.copy(problemName = "steiner-tree", instanceName = "es10fst03.stp", maybeOptimum = Some(26003678)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem], classOf[HasAlldifferentConstraint], classOf[HasBinPackingConstraint]))
    def team_assignment_data2_5_6 {
        solve(task.copy(problemName = "team-assignment", modelName = "model", instanceName = "data2_5_6", maybeOptimum = Some(974)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDisjunctiveConstraint]))
    def test_scheduling_t30m10r10_5 {
        solve(task.copy(problemName = "test-scheduling", instanceName = "t30m10r10-5", maybeOptimum = Some(2785)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def train_16 {
        solve(task.copy(problemName = "train", instanceName = "instance.16", maybeOptimum = Some(93925)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCircuitConstraint], classOf[HasCumulativeConstraint]))
    def vrplc_9_5_10_s1 {
        solve(task.copy(problemName = "vrplc", modelName = "vrplc_service", instanceName = "vrplc9_5_10_s1", maybeOptimum = Some(351)))
    }

}
