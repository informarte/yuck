package yuck.constraints.test

import org.junit._

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[BasicNumericalConstraintsTest],
        classOf[AlldistinctTest],
        classOf[BinPackingTest],
        classOf[BooleanTableTest],
        classOf[CountConstTest],
        classOf[CountVarTest],
        classOf[CumulativeTest],
        classOf[Disjoint2Test],
        classOf[DisjunctionTest],
        classOf[DistributionMaintainerTest],
        classOf[ElementTest],
        classOf[IntegerTableTest],
        classOf[InverseTest],
        classOf[LexLessEqTest],
        classOf[LexLessTest],
        classOf[LinearCombinationTest],
        classOf[LinearConstraintTest],
        classOf[MaximumTest],
        classOf[MinimumTest],
        classOf[NumberOfDistinctValuesTest],
        classOf[RegularTest],
        classOf[SumConstraintTest],
        classOf[SumTest]))
@Test
final class ConstraintTestSuite {
}
