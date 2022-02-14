package yuck.test

import org.junit.*

import yuck.annealing.test.*
import yuck.constraints.test.*
import yuck.core.test.*
import yuck.flatzinc.ast.test.*
import yuck.flatzinc.parser.test.*
import yuck.test.util.ParallelTestSuiteRunner
import yuck.util.alg.rtree.test.*

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
