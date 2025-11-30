package yuck.flatzinc.test

import scala.language.implicitConversions

import org.junit.*
import org.junit.experimental.categories.*
import org.junit.experimental.categories.Categories.*
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

import yuck.SolvingMethod
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.VariableWithInfiniteDomainException
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*

/**
 * Test cases taken from the MiniZinc 1.6 distribution
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class MiniZincExamples(maybePreferredSolvingMethod: Option[SolvingMethod]) extends ZincBasedTest {

    override protected val logToConsole = false

    private val task =
        ZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests/minizinc-examples",
            solverConfiguration =
                ZincTestTask().solverConfiguration.copy(
                    name = maybePreferredSolvingMethod.map(_.toString.toLowerCase).getOrElse("hybrid"),
                    maybePreferredSolvingMethod = maybePreferredSolvingMethod,
                    maybeRuntimeLimitInSeconds = Some(5)),
            throwWhenUnsolved = true)

    private implicit def createTask(problemName: String): ZincTestTask = task.copy(problemName = problemName)

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def test2DPacking(): Unit = {
        solve(task.copy(problemName = "2DPacking", maybeOptimum = Some(1)))
    }

    // Tough puzzle with tight all_different constraint, seemingly has only one solution!
    // (http://www.mathematik.uni-bielefeld.de/~sillke/PUZZLES/ALPHAMETIC/alphacipher)
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testAlpha(): Unit = {
        solve(task.copy(problemName = "alpha", maybeRuntimeLimitInSeconds = Some(10)))
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem]))
    def testBattleships(): Unit = {
        solve("battleships_1")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testBlocksworld1(): Unit = {
        solve("blocksworld_instance_1")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testBlocksworld2(): Unit = {
        solve("blocksworld_instance_2")
    }

    // Search variables item[] have infinite domains but pruning saves the day.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testCutstock(): Unit = {
        solve(task.copy(problemName = "cutstock", maybeOptimum = Some(4)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testEq20(): Unit = {
        solve("eq20")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testFactoryPlanning(): Unit = {
        solve("factory_planning_instance")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem], classOf[HasAllDifferentConstraint]))
    def testGolomb(): Unit = {
        solve(task.copy(problemName = "golomb", maybeOptimum = Some(6)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testJobshop(): Unit = {
        solve(task.copy(problemName = "jobshop2x2", maybeOptimum = Some(11)))
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testKnights(): Unit = {
        solve("knights")
    }

    // Uses redundant constraints in the form of a dual model that implies the need for
    // a lot of channeling constraints.
    // Hence this model is bad for local search and we test langford2 instead.
    @Test
    @Category(Array(classOf[UnsuitableProblem], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testLangford(): Unit = {
        solve("langford")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testLangford2(): Unit = {
        solve("langford2")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testLatinSquares(): Unit = {
        solve("latin_squares_fd")
    }

    // Magic sequence instances have tight all_different constraints.
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    @Test
    def testMagicSequence3(): Unit = {
        solve("magicsq_3")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testMagicSequence4(): Unit = {
        solve("magicsq_4")
    }

    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    @Test
    def testMagicSequence5(): Unit = {
        solve("magicsq_5")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MaximizationProblem]))
    def testMultiDimKnapsack(): Unit = {
        solve(task.copy(problemName = "multidimknapsack_simple", maybeOptimum = Some(17)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testOss(): Unit = {
        solve(task.copy(problemName = "oss", maybeOptimum = Some(1168), maybeTargetObjectiveValue = Some(1250)))
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem]))
    def testPacking(): Unit = {
        solve("packing")
    }

    // This one is hard for FJ because FJ does not support implicit constraints.
    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testPartition(): Unit = {
        solve("partition")
    }

    // The original formulation contains symmetry breaking and the optimization part is based on that.
    // The improved model uses all_different_except_0 and count :-)
    // This formulation maximizes the number of squares in the sum.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem], classOf[HasAllDifferentExceptConstraint]))
    def testPerfectSquares(): Unit = {
        solve(task.copy(problemName = "perfsq_ls", maybeOptimum = Some(5)))
    }

    // Another formulation for the perfect square problem that maximizes the sum of squares.
    // FJ finds good solutions very fast but cannot find the optimum.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MaximizationProblem]))
    def testPerfectSquares2(): Unit = {
        solve(task.copy(problemName = "perfsq2", maybeOptimum = Some(337561), maybeTargetObjectiveValue = Some(300000)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MaximizationProblem], classOf[HasAllDifferentConstraint]))
    def testPhoto(): Unit = {
        solve(task.copy(problemName = "photo", maybeOptimum = Some(8)))
    }

    // Big domains!
    // The solver finds good solutions fast but takes too long to find the optimum.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testProductFd(): Unit = {
        solve(task.copy(problemName = "product_fd", maybeOptimum = Some(37200), maybeTargetObjectiveValue = Some(37400)))
    }

    // Variables inside and outside do not have infinite domains and pruning cannot make them finite.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testProductLp(): Unit = {
        assertEx(
            solve(task.copy(problemName = "product_lp", maybeOptimum = Some(37200))),
            classOf[VariableWithInfiniteDomainException])
    }

    // Solved, but not to optimality.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testRadiation(): Unit = {
        solve(task.copy(problemName = "radiation", maybeOptimum = Some(553), maybeTargetObjectiveValue = Some(2000)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testQuasigroup(): Unit = {
        solve("quasigroup_qg5")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testQueensCp(): Unit = {
        solve("queen_cp2")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testQueensIp(): Unit = {
        solve("queen_ip")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testSimpleSat(): Unit = {
        solve("simple_sat")
    }

    // Uses set decision variables.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testSteinerTriples(): Unit = {
        solve(task.copy(problemName = "steiner-triples"))
    }

    // Has a lot of constants in all_different constraints that are used to prune domains up-front.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testSudoku(): Unit = {
        solve("sudoku")
    }

    // A lot of (allegedly) redundant constraints in the original definition.
    // Annotating them leads to a solution better than the optimum proven by CP (439 vs 442).
    // So it seems that the redundant constraints are not that redundant ...
    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testTemplateDesign(): Unit = {
        solve(task.copy(problemName = "template_design", maybeOptimum = Some(442)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki1(): Unit = {
        solve("tenpenki_1")
    }

    @Test
    @Category(Array(classOf[HardInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki2(): Unit = {
        solve("tenpenki_2")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki3(): Unit = {
        solve("tenpenki_3")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki4(): Unit = {
        val result = solveWithResult(task.copy(problemName = "tenpenki_4"))
        assertEq(result.space.searchVariables, Set())
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki5(): Unit = {
        solve("tenpenki_5")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testTenpenki6(): Unit = {
        solve("tenpenki_6")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testTimetabling(): Unit = {
        solve("timetabling")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testTrucking(): Unit = {
        solve(task.copy(problemName = "trucking", maybeOptimum = Some(224)))
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[MinimizationProblem]))
    def testWarehouses(): Unit = {
        solve(task.copy(problemName = "warehouses", maybeOptimum = Some(383)))
    }

    // Has six variables declared as parameters by bool_eq constraints.
    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem]))
    def testWolfGoatCabbage(): Unit = {
        solve("wolf_goat_cabbage")
    }

    @Test
    @Category(Array(classOf[EasyInstance], classOf[SatisfiabilityProblem], classOf[HasAllDifferentConstraint]))
    def testZebra(): Unit = {
        solve("zebra")
    }

}

object MiniZincExamples {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = Array(None, Some(SolvingMethod.SimulatedAnnealing), Some(SolvingMethod.FeasibilityJump))

}

/**
 * Test cases for running after every change
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[EasyInstance]))
@SuiteClasses(Array(classOf[MiniZincExamples]))
class EasyMiniZincExamples

/**
 * Test cases for running after every change
 */
@Ignore("Avoid pointless NoTestsRemainException")
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[MediumInstance]))
@SuiteClasses(Array(classOf[MiniZincExamples]))
final class MediumMiniZincExamples

/**
 * Hard and unsolved problems
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HardInstance]))
@SuiteClasses(Array(classOf[MiniZincExamples]))
final class HardMiniZincExamples

/**
 * Test cases for running after every change
 */
@RunWith(classOf[runners.Suite])
@SuiteClasses(
    Array(
        classOf[EasyMiniZincExamples],
        classOf[MediumMiniZincExamples]))
class TractableMiniZincExamples

/**
 * Tractable satisfiability examples
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[SatisfiabilityProblem]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableSatisfiabilityExamples

/**
 * Tractable minimization examples
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[MinimizationProblem]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableMinimizationExamples

/**
 * Tractable maximization examples
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[MaximizationProblem]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableMaximizationExamples

/**
 * Tractable examples with integer all_different constraints
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasAllDifferentConstraint]))
@SuiteClasses(Array(classOf[TractableMiniZincExamples]))
class TractableAllDifferentExamples
