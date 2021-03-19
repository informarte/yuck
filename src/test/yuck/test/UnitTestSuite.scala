package yuck.test

import org.junit._

import yuck.annealing.test._
import yuck.constraints.test._
import yuck.core.test._
import yuck.flatzinc.ast.test._
import yuck.flatzinc.parser.test._
import yuck.util.alg.rtree.test._
import yuck.util.testing.ParallelTestSuiteRunner

@runner.RunWith(classOf[ParallelTestSuiteRunner])
@runners.Suite.SuiteClasses(
    Array(
        classOf[ProbabilityTest],
        classOf[RandomGeneratorTest],
        classOf[ValueTestSuite],
        classOf[DomainTestSuite],
        classOf[VariableTest],
        classOf[PropagationEffectsTest],
        classOf[SpaceTest],
        classOf[ObjectiveTestSuite],
        classOf[SolverTest],
        classOf[FenwickTreeTest],
        classOf[DistributionTest],
        classOf[ConstraintTestSuite],
        classOf[NeighbourhoodTestSuite],
        classOf[ProgressiveTighteningTest],
        classOf[RTreeIntakeTest],
        classOf[RTreeTransactionTest],
        classOf[FlatZincAstTest],
        classOf[FlatZincParserTest]))
final class UnitTestSuite
