package yuck.flatzinc.test

import org.junit._
import org.junit.experimental.categories._
import org.junit.experimental.categories.Categories._

import yuck.flatzinc.test.util._

/**
 * The smallest problems from the MiniZinc challenge 2012
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MiniZincChallenge2012 extends MiniZincBasedTest {

    private val task = MiniZincTestTask(directoryLayout = StandardMiniZincBenchmarksLayout, suitePath = "resources/mzn/benchmarks", suiteName = "mznc12")

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCountConstraint]))
    def amaze_2012_03_19 {
        solve(task.copy(problemName = "amaze", instanceName = "2012-03-19", maybeOptimum = Some(447)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def amaze2_2012_06_28 {
        solve(task.copy(problemName = "amaze", modelName = "amaze2", instanceName = "2012-06-28"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint], classOf[HasDiffnConstraint]))
    def carpet_cutting_05 {
        solve(task.copy(problemName = "carpet-cutting", modelName = "cc_base", instanceName = "mzn_rnd_test.05", maybeHighScore = Some(1192)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def fast_food_3 {
        solve(task.copy(problemName = "fast-food", modelName = "fastfood", instanceName = "ff3", maybeOptimum = Some(1330)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasDiffnConstraint], classOf[HasMaximumConstraint]))
    def filter_fir_1_1 {
        solve(task.copy(problemName = "filters", modelName = "filter", instanceName = "fir_1_1", maybeOptimum = Some(18)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def league_20_3_5 {
        solve(task.copy(problemName = "league", instanceName = "model20-3-5", maybeOptimum = Some(49984)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasCumulativeConstraint]))
    def mspsp_easy_1 {
        solve(task.copy(problemName = "mspsp", instanceName = "easy_01", maybeOptimum = Some(26)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem], classOf[HasRegularConstraint]))
    def nonogram_fast_4 {
        solve(task.copy(problemName = "nonogram", modelName = "non", instanceName = "non_fast_4"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def parity_learning_44_22_5_3 {
        solve(task.copy(problemName = "parity-learning", instanceName = "44_22_5.3", maybeOptimum = Some(2)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def pattern_set_mining_k1_ionosphere {
        solve(task.copy(problemName = "pattern-set-mining", modelName = "pattern_set_mining_k1", instanceName = "ionosphere", maybeOptimum = Some(195)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def pattern_set_mining_k2_audiology {
        solve(task.copy(problemName = "pattern-set-mining", modelName = "pattern_set_mining_k2", instanceName = "audiology", maybeOptimum = Some(54)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def project_planning_12_8 {
        solve(task.copy(directoryLayout = NonStandardMiniZincBenchmarksLayout, problemName = "project-planning", instanceName = "ProjectPlannertest_12_8"))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def radiation_07_07_20 {
        solve(task.copy(problemName = "radiation", instanceName = "m07_07_20", maybeOptimum = Some(856)))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def ship_schedule_5 {
        solve(task.copy(problemName = "ship-schedule", modelName = "ship-schedule.cp", instanceName = "5Ships", maybeOptimum = Some(483650)))
    }

    @Test
    @Category(Array(classOf[SatisfiabilityProblem]))
    def solbat_12_12_5_1 {
        solve(task.copy(problemName = "solbat", modelName = "sb", instanceName = "sb_12_12_5_1"))
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def still_life_wastage_09 {
        solve(task.copy(problemName = "still-life-wastage", modelName = "still-life", instanceName = "09", maybeOptimum = Some(43)))
    }

    @Test
    @Category(Array(classOf[MinimizationProblem], classOf[HasSubcircuitConstraint]))
    def tpp_3_3_30_1 {
        solve(task.copy(problemName = "tpp", instanceName = "tpp_3_3_30_1", maybeOptimum = Some(190)))
    }

    // Has int_eq constraints which are used to eliminate variables.
    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def train_6 {
        solve(task.copy(problemName = "train", instanceName = "instance.6", maybeOptimum = Some(28290)))
    }

    // Has some simple linear combinations (e.g. 100 * x <= 87 with x <=1) that are propagated up-front.
    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def vrp_A_n38_k5 {
        solve(task.copy(problemName = "vrp", instanceName = "A-n38-k5.vrp", maybeHighScore = Some(737)))
    }

}
