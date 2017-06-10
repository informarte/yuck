package yuck.test

import org.junit._

import yuck.annealing.test._
import yuck.constraints.test._
import yuck.core.test._
import yuck.flatzinc.ast.test._
import yuck.flatzinc.parser.test._

@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[RandomGeneratorTest],
        classOf[ValueTest],
        classOf[DomainTest],
        classOf[VariableTest],
        classOf[SpaceTest],
        classOf[ObjectiveTest],
        classOf[SolverTest],
        classOf[FenwickTreeTest],
        classOf[DistributionTest],
        classOf[ConstraintTest],
        classOf[NeighbourhoodTest],
        classOf[ProgressiveTighteningTest],
        classOf[Queens],
        classOf[SendMoreMoney],
        classOf[SendMostMoney],
        classOf[FlatZincAstTest],
        classOf[FlatZincParserTest]))
@Test
class UnitTestSuite {
}
