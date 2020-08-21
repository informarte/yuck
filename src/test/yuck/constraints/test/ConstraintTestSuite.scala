package yuck.constraints.test

import org.junit._

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[PropagationTestToolingTest],
        classOf[BasicNumericalConstraintsTest],
        classOf[AlldistinctTest],
        classOf[BinPackingTest],
        classOf[BooleanTableTest],
        classOf[ConjunctionTest],
        classOf[CountConstTest],
        classOf[CountVarTest],
        classOf[CumulativeTest],
        classOf[Disjoint2Test],
        classOf[DisjunctionTest],
        classOf[ElementConstTest],
        classOf[ElementVarTest],
        classOf[IntegerTableTest],
        classOf[InverseTest],
        classOf[LexLessEqTest],
        classOf[LexLessTest],
        classOf[LinearCombinationTest],
        classOf[LinearConstraintTest],
        classOf[MaximumTest],
        classOf[MinimumTest],
        classOf[NumberOfDistinctValuesTest],
        classOf[OptimizationGoalTrackerTest],
        classOf[RegularTest],
        classOf[SatisfactionGoalTrackerTest],
        classOf[SumConstraintTest],
        classOf[SumTest]))
@Test
final class ConstraintTestSuite {
}
