package yuck.constraints.test

import org.junit.*

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
        classOf[BinPackingTest],
        classOf[BooleanIncreasingTest],
        classOf[BooleanIncreasingNeighbourhoodTest],
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
        classOf[GeneralInverseNeighbourhoodTest],
        classOf[IfThenElseTest],
        classOf[AllDifferentTest],
        classOf[AllDifferentExceptTest],
        classOf[AllDifferentNeighbourhoodTest],
        classOf[IntegerIncreasingTest],
        classOf[IntegerIncreasingNeighbourhoodTest],
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
        classOf[RegularNeighbourhoodTest],
        classOf[SatisfactionGoalTrackerTest],
        classOf[SelfInverseNeighbourhoodTest],
        classOf[SimpleInverseNeighbourhoodTest],
        classOf[SumConstraintTest],
        classOf[SumTest],
        classOf[TableNeighbourhoodTest]))
final class ConstraintTestSuite
