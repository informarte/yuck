package yuck.constraints.test

import org.junit._

import yuck.constraints.test.util.test.ConstraintTestToolingTest

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[ConstraintTestToolingTest],
        classOf[BasicNumericalConstraintsTest],
        classOf[AlldistinctTest],
        classOf[BinPackingTest],
        classOf[CircuitTest],
        classOf[CircuitNeighbourhoodTest],
        classOf[ConjunctionTest],
        classOf[CountConstTest],
        classOf[CountVarTest],
        classOf[CumulativeTest],
        classOf[DeliveryTest],
        classOf[Disjoint2Test],
        classOf[DisjunctionTest],
        classOf[ElementConstTest],
        classOf[ElementVarTest],
        classOf[IfThenElseTest],
        classOf[TableTest],
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
        classOf[SumTest],
        classOf[TableNeighbourhoodTest]))
final class ConstraintTestSuite
