package yuck.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

import yuck.constraints.test.*
import yuck.core.test.*
import yuck.flatzinc.ast.test.*
import yuck.flatzinc.parser.test.*
import yuck.util.CollectionsTest
import yuck.util.alg.rtree.test.*

@Suite
@SelectClasses(
    Array(
        classOf[CollectionsTest],
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
        classOf[RTreeIntakeTest],
        classOf[RTreeTransactionTest],
        classOf[FlatZincAstTest],
        classOf[FlatZincParserTest]))
final class UnitTestSuite
