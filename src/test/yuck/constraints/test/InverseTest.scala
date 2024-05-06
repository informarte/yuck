package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.*
import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class InverseTest(fOffset: Int, gOffset: Int) extends UnitTest with ConstraintTestTooling {

    private val BaseDomain = CompleteIntegerRange

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), BaseDomain)
    private val Seq(x1, x2, x3) = xs
    private val ys = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "y%d".format(i), BaseDomain)
    private val Seq(y1, y2, y3) = ys
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)
    private val f = new InverseFunction(xs, fOffset)
    private val g = new InverseFunction(ys, gOffset)

    @Test
    def testBasics(): Unit = {
        val constraint = new Inverse(space.nextConstraintId(), null, f, g, costs, logger)
        assertEq(constraint.toString, "inverse([x1, x2, x3], %d, [y1, y2, y3], %d, costs)".format(fOffset, gOffset))
        assertEq(constraint.inVariables.size, 6)
        assertEq(constraint.inVariables.toSet, xs.toSet.union(ys.toSet))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(new Inverse(space.nextConstraintId(), null, f, g, costs, logger))
        runScenario(
            TestScenario(
                space,
                Propagate("do not propagate soft constraint", Nil, Nil),
                PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                Propagate(
                    "root-node propagation",
                    List(costs << TrueDomain),
                    f.xs.map(_ << g.indexDomain).concat(g.xs.map(_ << f.indexDomain))),
                PropagateAndRollback(
                    "x2 -> y1",
                    List(x2 << IntegerDomain(List(gOffset))),
                    List(x1, x3)
                        .map(_ << g.indexDomain.diff(IntegerDomain(gOffset)))
                        .appended(y1 << IntegerDomain(fOffset + 1))
                        .concat(List(y2, y3).map(_ << f.indexDomain.diff(IntegerDomain(fOffset + 1))))),
                Propagate(
                    "y1 -> x2",
                    List(y1 << IntegerDomain(List(fOffset + 1))),
                    List(y2, y3)
                        .map(_ << f.indexDomain.diff(IntegerDomain(fOffset + 1)))
                        .appended(x2 << IntegerDomain(gOffset))
                        .concat(List(x1, x3).map(_ << g.indexDomain.diff(IntegerDomain(gOffset)))))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInPropagation(): Unit = {
        val f = new InverseFunction(Vector(x1, x1, x3), fOffset)
        val g = new InverseFunction(Vector(y1, y3, y3), gOffset)
        space.post(new Inverse(space.nextConstraintId(), null, f, g, costs, logger))
        runScenario(
            TestScenario(
                space,
                Propagate(
                    "root-node propagation",
                    List(costs << TrueDomain),
                    f.xs.map(_ << g.indexDomain).concat(g.xs.map(_ << f.indexDomain))),
                PropagateAndRollback(
                    "x1 -> y1",
                    List(x1 << List(gOffset)),
                    List(y1 << EmptyIntegerRange, y3 << List(fOffset + 2))),
                Propagate(
                    "y3 -> x3",
                    List(y3 << List(fOffset + 2)),
                    List(x1 << List(gOffset), x3 << EmptyIntegerRange))))
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new Inverse(space.nextConstraintId(), null, f, g, costs, logger))
        runScenario(
            TestScenario(
                space,
                // 3 2 1 (assuming both offsets are 1)
                // 3 2 1
                Initialize(
                    "inverse functions",
                    x1 << gOffset + 2, x2 << gOffset + 1, x3 << gOffset,
                    y1 << fOffset + 2, y2 << fOffset + 1, y3 << fOffset,
                    costs << True),
                // 1 2 1
                // 3 2 1
                Consult("1", x1 << gOffset, costs << False4),
                // 3 2 1
                // 3 2 3
                Consult("2", y3 << fOffset + 2, costs << False4),
                // 3 1 2
                // 3 1 2
                Initialize(
                    "non-inverse functions",
                    x1 << gOffset + 2, x2 << gOffset, x3 << gOffset + 1,
                    y1 << fOffset + 2, y2 << fOffset, y3 << fOffset + 1,
                    costs << False8),
                // swap values of y1 and y3:
                // 3 1 2
                // 2 1 3
                // This move checks the functioning of the logic that avoids revisiting the same node twice
                // when computing the cost delta.
                // Here x2 is the node in question because it is referenced by before(y3) and after(y1).
                ConsultAndCommit("1", y1 << fOffset + 1, y3 << fOffset + 2, costs << False6),
                // swap values of x1 and x3:
                // 2 1 3
                // 2 1 3
                ConsultAndCommit("2", x1 << gOffset + 1, x3 << gOffset + 2, costs << True),
                // swap values of x2 and y1:
                // 2 2 3
                // 1 1 3
                ConsultAndCommit("3", x2 << gOffset + 1, y1 << fOffset, costs << False2),
                // reverting previous move in two steps, this is step one:
                // 2 1 3
                // 1 1 3
                ConsultAndCommit("4a", x2 << gOffset, costs << False2),
                // ... this is step two:
                // 2 1 3
                // 2 1 3
                ConsultAndCommit("4b", y1 << fOffset + 1, costs << True)))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation(): Unit = {
        val f = new InverseFunction(Vector(x1, x1, x3), fOffset)
        val g = new InverseFunction(Vector(y1, y3, y3), gOffset)
        space.post(new Inverse(space.nextConstraintId(), null, f, g, costs, logger))
        runScenario(
            TestScenario(
                space,
                // 3 3 1 (assuming both offsets are 1)
                // 3 1 1
                Initialize(
                    "non-inverse functions",
                    x1 << gOffset + 2, x3 << gOffset,
                    y1 << fOffset + 2, y3 << fOffset,
                    costs << False2),
                // 1 1 1
                // 3 3 3
                Consult("1", x1 << gOffset, y3 << fOffset + 2, costs << False6),
                // 2 2 1
                // 3 2 2
                ConsultAndCommit("1", x1 << gOffset + 1, y3 << fOffset + 1, costs << False2)
           ))
    }

    @Test
    def testInverseFunctionTest(): Unit = {
        for (i <- xs.indices) {
            space.setValue(xs(i), IntegerValue(gOffset + i))
            space.setValue(ys(i), IntegerValue(fOffset + i))
        }
        assert(Inverse.areInverseFunctionsOfEachOther(f, g, now))
        assert(Inverse.areInverseFunctionsOfEachOther(g, f, now))
        space.setValue(x1, IntegerValue(gOffset - 1))
        assert(! Inverse.areInverseFunctionsOfEachOther(f, g, now))
        assert(! Inverse.areInverseFunctionsOfEachOther(g, f, now))
        space.setValue(x1, IntegerValue(gOffset)).setValue(y1, IntegerValue(fOffset + 1))
        assert(! Inverse.areInverseFunctionsOfEachOther(f, g, now))
        assert(! Inverse.areInverseFunctionsOfEachOther(g, f, now))
        space.setValue(y1, IntegerValue(fOffset)).setValue(x3, IntegerValue(gOffset + 1))
        assert(! Inverse.areInverseFunctionsOfEachOther(f, g, now))
        assert(! Inverse.areInverseFunctionsOfEachOther(g, f, now))
    }

    @Test
    def testNeighbourhoodGenerationWithUnrestrictedPairing(): Unit = {
        xs.foreach(_.pruneDomain(g.indexDomain))
        ys.foreach(_.pruneDomain(f.indexDomain))
        assertNeighbourhood(f, g, classOf[SimpleInverseNeighbourhood])
    }

    @Test
    def testNeighbourhoodGenerationWithRestrictedPairing(): Unit = {
        xs.foreach(_.pruneDomain(g.indexDomain))
        ys.foreach(_.pruneDomain(f.indexDomain))
        x1.pruneDomain(IntegerDomain(List(gOffset)))
        y3.pruneDomain(IntegerDomain(List(fOffset + xs.size - 1)))
        assertNeighbourhood(f, g, classOf[GeneralInverseNeighbourhood])
    }

    @Test
    def testNeighbourhoodGenerationWithOneFunction(): Unit = {
        val f = new InverseFunction(xs ++ ys, fOffset)
        f.xs.foreach(_.pruneDomain(IntegerRange(fOffset, fOffset + 5)))
        assertNeighbourhood(f, f, classOf[SelfInverseNeighbourhood])
    }

    @Test
    def testHandlingOfDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        xs.foreach(_.pruneDomain(g.indexDomain))
        ys.foreach(_.pruneDomain(f.indexDomain))
        assertNoNeighbourhood(new InverseFunction(Vector(x1, x1, x3), fOffset), g)
        assertNoNeighbourhood(f, new InverseFunction(Vector(y1, y2, y2), gOffset))
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration(): Unit = {
        xs.foreach(_.pruneDomain(g.indexDomain))
        ys.foreach(_.pruneDomain(f.indexDomain))
        import yuck.constraints.Plus
        space.post(new Plus(space.nextConstraintId(), null, x1, x2, y1))
        assertNoNeighbourhood(f, g)
    }

    @Test
    def testHandlingOfInfiniteDomainsInNeighbourhoodGeneration1(): Unit = {
        xs.foreach(_.pruneDomain(g.indexDomain))
        assertNoNeighbourhood(f, g)
    }

    @Test
    def testHandlingOfInfiniteDomainsInNeighbourhoodGeneration2(): Unit = {
        ys.foreach(_.pruneDomain(f.indexDomain))
        assertNoNeighbourhood(f, g)
    }

    @Test
    def testHandlingOfInvalidIndicesInNeighbourhoodGeneration1(): Unit = {
        if (f.offset != g.offset) {
            xs.foreach(_.pruneDomain(f.indexDomain))
            ys.foreach(_.pruneDomain(f.indexDomain))
            assertNoNeighbourhood(f, g)
        }
    }

    @Test
    def testHandlingOfInvalidIndicesInNeighbourhoodGeneration2(): Unit = {
        if (f.offset != g.offset) {
            xs.foreach(_.pruneDomain(g.indexDomain))
            ys.foreach(_.pruneDomain(g.indexDomain))
            assertNoNeighbourhood(f, g)
        }
    }

    private def assertNeighbourhood
        (f: InverseFunction, g: InverseFunction, expectedNeighbourhoodClass: Class[? <: InverseNeighbourhood]): Unit =
    {
        require(f.xs.forall(_.domain.isFinite))
        require(g.xs.forall(_.domain.isFinite))
        val constraint = new Inverse(space.nextConstraintId(), null, f, g, costs, logger)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution).get
        assertEq(neighbourhood.getClass, expectedNeighbourhoodClass)
        assert(f.xs.forall(x => x.domain.contains(now.value(x))))
        assert(g.xs.forall(x => x.domain.contains(now.value(x))))
        assert(Inverse.areInverseFunctionsOfEachOther(f, g, now))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, f.xs.view.concat(g.xs).filterNot(_.domain.isSingleton).toSet)
    }

    private def assertNoNeighbourhood(f: InverseFunction, g: InverseFunction, isCandidate: Boolean = false): Unit = {
        val constraint = new Inverse(space.nextConstraintId(), null, f, g, costs, logger)
        assertEq(constraint.isCandidateForImplicitSolving(space), isCandidate)
        assertEq(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution), None)
    }

}

/**
 * @author Michael Marte
 *
 */
object InverseTest {

    private def offsets = List(-1, 0, 1).map(Integer.valueOf)
    private def configurations = for (fOffset <- offsets; gOffset <- offsets) yield Vector(fOffset, gOffset)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
