package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.{AllDifferent, AllDifferentNeighbourhood}
import yuck.core.*
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
final class AllDifferentTest(withException: Boolean) extends UnitTest with ConstraintTestTooling {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(0, 9))
    private val Seq(x1, x2, x3) = xs
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    private val exceptedValues = if withException then Set(Zero) else Set[IntegerValue]()

    @Test
    def testBasics(): Unit = {
        val constraint = new AllDifferent(space.nextConstraintId(), null, xs, exceptedValues, costs, logger)
        if (withException) {
            assertEq(constraint.toString, "all_different_except([x1, x2, x3], {0}, costs)")
        } else {
            assertEq(constraint.toString, "all_different([x1, x2, x3], costs)")
        }
        assertEq(constraint.inVariables.size, 3)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(new AllDifferent(space.nextConstraintId(), null, xs, exceptedValues, costs, logger))
        runScenario(
            TestScenario(
                space,
                Propagate("do not propagate soft constraint", Nil, Nil),
                PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                PropagateAndRollback(
                    "fix value of x1",
                    List(x1 << IntegerDomain(0)),
                    if withException then Nil else List(x2, x3).map(x => x << x.domain.diff(IntegerDomain(0)))),
                Propagate(
                    "fix value of x2",
                    List(x2 << IntegerDomain(1)),
                    List(x1, x3).map(x => x << x.domain.diff(IntegerDomain(1))))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInPropagation(): Unit = {
        space.post(new AllDifferent(space.nextConstraintId(), null, Vector(x1, x2, x2), exceptedValues, costs, logger))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                PropagateAndRollback(
                    "fix value of x1",
                    List(x1 << IntegerDomain(1)),
                    List(x2 << x2.domain.diff(IntegerDomain(1)))),
                Propagate(
                    "fix value of duplicate variable x2",
                    List(x2 << IntegerDomain(0)),
                    if withException then Nil else List(x1 << x1.domain.diff(IntegerDomain(0))))))
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new AllDifferent(space.nextConstraintId(), null, xs, exceptedValues, costs, logger))
        if (withException) {
            runScenario(
                TestScenario(
                    space,
                    Initialize("no conflict", x1 << 1, x2 << 2, x3 << 3, costs << True),
                    Initialize("no conflict", x1 << 0, x2 << 0, x3 << 3, costs << True),
                    Initialize("one conflict", x1 << 1, x2 << 1, x3 << 3, costs << False),
                    Initialize("two conflicts", x1 << 1, x2 << 1, x3 << 1, costs << False2),
                    Consult("fix a conflict", x3 << 3, costs << False),
                    Consult("fix both conflicts", x2 << 2, x3 << 3, costs << True),
                    Consult("fix both conflicts", x2 << 0, x3 << 0, costs << True),
                    ConsultAndCommit("fix both conflicts", x1 << 0, x2 << 2, x3 << 0, costs << True),
                    ConsultAndCommit("cause a conflict", x1 << 2, costs << False)))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Initialize("no conflict", x1 << 1, x2 << 2, x3 << 3, costs << True),
                    Initialize("one conflict", x1 << 0, x2 << 0, x3 << 3, costs << False),
                    Initialize("two conflicts", x1 << 0, x2 << 0, x3 << 0, costs << False2),
                    Consult("fix a conflict", x3 << 3, costs << False),
                    Consult("fix both conflicts", x2 << 2, x3 << 3, costs << True),
                    ConsultAndCommit("fix both conflicts", x2 << 2, x3 << 3, costs << True),
                    ConsultAndCommit("cause a conflict", x1 << 2, costs << False)))
        }
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation(): Unit = {
        space.post(new AllDifferent(space.nextConstraintId(), null, Vector(x1, x2, x2), exceptedValues, costs, logger))
        if (withException) {
            runScenario(
                TestScenario(
                    space,
                    Initialize("no conflict", x1 << 1, x2 << 0, costs << True),
                    Initialize("one conflict", x1 << 1, x2 << 2, costs << False),
                    Initialize("two conflicts", x1 << 1, x2 << 1, costs << False2),
                    Consult("fix a conflict", x2 << 2, costs << False),
                    Consult("fix both conflicts", x2 << 0, costs << True),
                    ConsultAndCommit("fix a conflict", x2 << 2, costs << False),
                    ConsultAndCommit("fix both conflicts", x2 << 0, costs << True)))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Initialize("one conflict", x1 << 1, x2 << 2, costs << False),
                    Initialize("two conflicts", x1 << 1, x2 << 1, costs << False2),
                    Consult("fix a conflict", x2 << 2, costs << False),
                    Consult("move sidewards", x1 << 2, x2 << 2, costs << False2),
                    ConsultAndCommit("fix a conflict", x1 << 2, costs << False),
                    ConsultAndCommit("cause a conflict", x2 << 2, costs << False2)))
        }
    }

    @Test
    def testNeighbourhoodGeneration(): Unit = {
        assertNeighbourhood(xs)
    }

    @Test
    def testNeighbourhoodGenerationWithEqualDomainsAndJustEnoughValues(): Unit = {
        xs.foreach(_.pruneDomain(IntegerRange(0, xs.size - 1)))
        assertNeighbourhood(xs)
    }

    @Test
    def testNeighbourhoodGenerationWithEqualDomainsAndMoreValuesThanVariables(): Unit = {
        assertNeighbourhood(xs)
    }

    @Test
    def testNeighbourhoodGenerationWithDifferentDomainsAndJustEnoughValues(): Unit = {
        xs.indices.foreach(i => xs(i).pruneDomain(IntegerRange(0, xs.size - 1).diff(IntegerDomain(i))))
        assertNeighbourhood(xs)
    }

    @Test
    def testNeighbourhoodGenerationWithDifferentDomainsAndMoreValuesThanVariables(): Unit = {
        xs.indices.foreach(i => xs(i).pruneDomain(xs(i).domain.diff(IntegerDomain(i))))
        assertNeighbourhood(xs)
    }

    @Test
    def testHandlingOfDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        assertNoNeighbourhood(Vector(x1, x2, x2))
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration(): Unit = {
        import yuck.constraints.Plus
        space.post(new Plus(space.nextConstraintId(), null, x1, x2, x3))
        assertNoNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration(): Unit = {
        x1.pruneDomain(IntegerDomain(0))
        space.setValue(x1, Zero)
        assertNoNeighbourhood(xs)
    }

    private def assertNeighbourhood(xs: IndexedSeq[IntegerVariable]): Unit = {
        val constraint = new AllDifferent(space.nextConstraintId(), null, xs, exceptedValues, costs, logger)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution).get
        assertEq(neighbourhood.getClass, classOf[AllDifferentNeighbourhood[?]])
        val now = space.searchState
        assert(xs.forall(x => x.domain.contains(now.value(x))))
        if (withException) {
            assertEq(
                xs.view.map(now.value).filter(_ != Zero).toSet.size +
                    xs.view.map(now.value).count(_ == Zero),
                xs.size)
        } else {
            assertEq(xs.map(now.value).toSet.size, xs.size)
        }
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, xs.filterNot(_.domain.isSingleton).toSet)
    }

    private def assertNoNeighbourhood(xs: IndexedSeq[IntegerVariable], isCandidate: Boolean = false): Unit = {
        val constraint = new AllDifferent(space.nextConstraintId(), null, xs, exceptedValues, costs, logger)
        space.post(constraint)
        assertEq(constraint.isCandidateForImplicitSolving(space), isCandidate)
        assertEq(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution), None)
    }

}

/**
 * @author Michael Marte
 *
 */
object AllDifferentTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(true, false).asJava

}
