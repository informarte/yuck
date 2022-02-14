package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.ref.WeakReference

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.{Circuit, Delivery}
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class DeliveryTest(offset: Int, withTimeWindows: Boolean, withWaiting: Boolean) extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val numberOfCities = 15
    private val n = numberOfCities
    private val numberOfVehicles = 3
    private val k = numberOfVehicles
    private val cityNodes = Range.inclusive(0, n - 1)
    private val startNodes = Range.inclusive(n, n + k - 1)
    private val endNodes = Range.inclusive(n + k, n + 2 * k - 1)
    private val nodes = Range.inclusive(cityNodes.start, endNodes.end)
    private implicit def zeroBasedScalaRangeToOffsetBasedIntegerRange(range: Range): IntegerRange = {
        require(range.step == 1)
        if (range.isEmpty) EmptyIntegerRange
        else if (range.isInclusive) IntegerRange(offset + range.start, offset + range.end)
        else IntegerRange(offset + range.start, offset + range.end - 1)
    }
    private val succ = nodes.map(i => new IntegerVariable(space.nextVariableId, "x%d".format(i + 1), nodes))
    for (i <- endNodes) {
        val j = if (i == endNodes.end) startNodes.start else startNodes.start + (i - endNodes.start) + 1
        succ(i).pruneDomain(Range.inclusive(j, j))
    }
    private val circuitCosts = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
    private val circuit = new Circuit(space.nextConstraintId, null, succ, offset, circuitCosts)
    private val serviceTimes = nodes.map(_ => IntegerValue(randomGenerator.nextInt(numberOfCities)))
    private val travelTimes = nodes.map(_ => nodes.map(_ => IntegerValue(randomGenerator.nextInt(numberOfCities) + 1)))
    private val timeRange = IntegerRange(0, nodes.map(i => nodes.map(j => travelTimes(i)(j).value).max).sum)
    private val arrivalTimes =
        for (i <- nodes) yield
            new IntegerVariable(
                space.nextVariableId, "x%d".format(i + 1),
                if (withTimeWindows) timeRange.randomSubrange(randomGenerator) else timeRange)
    for (i <- startNodes) {
        space.setValue(arrivalTimes(i), arrivalTimes(i).domain.lb)
    }
    private val totalTravelTime =
        new IntegerVariable(
            space.nextVariableId, "totalTravelTime",
            if (randomGenerator.nextDecision()) IntegerRange(timeRange.ub, timeRange.ub)
            else IntegerRange(timeRange.lb, timeRange.lb))
    private val deliveryCosts =
        new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
    private val delivery =
        new Delivery(
            WeakReference(space), space.nextConstraintId, null, startNodes, endNodes, succ, offset,
            arrivalTimes, serviceTimes.apply, (i, j) => travelTimes(i)(j), withWaiting, totalTravelTime, deliveryCosts)

    private def createNeighbourhood: Neighbourhood = {
        space.post(circuit).markAsImplicit(circuit).post(delivery)
        circuit.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
    }

    private def checkArrivalTimes(searchState: SearchState): Unit = {
        for (i <- nodes if ! endNodes.contains(i)) {
            val j = searchState.value(succ(i)).value - offset
            val (x, y) = (arrivalTimes(i), arrivalTimes(j))
            val (a, b) = (searchState.value(x), searchState.value(y))
            val c = a + serviceTimes(i) + travelTimes(i)(j)
            assertEq(b, if (withWaiting) IntegerValue.max(y.domain.lb, c) else c)
        }
    }

    private def checkTotalTravelTime(searchState: SearchState): Unit = {
        var expectedTotalTravelTime = Zero
        for (i <- nodes if ! endNodes.contains(i)) {
            val j = searchState.value(succ(i)).value - offset
            expectedTotalTravelTime += travelTimes(i)(j)
        }
        assertEq(searchState.value(totalTravelTime), expectedTotalTravelTime)
    }

    private def checkCosts(searchState: SearchState): Unit = {
        assertEq(searchState.value(circuitCosts), True)
        val expectedViolation =
            ((if (withTimeWindows) arrivalTimes else Nil).view :+ totalTravelTime)
                .map(x => x.domain.distanceTo(searchState.value(x)).value).sum
        assertEq(searchState.value(deliveryCosts), BooleanValue(expectedViolation))
    }

    @Test
    def testBasics: Unit = {
        assertEq(
            delivery.toString,
            "delivery(%s, %s, [%s], [%s], ..., %s, %s, %s)"
                .format(
                    zeroBasedScalaRangeToOffsetBasedIntegerRange(startNodes),
                    zeroBasedScalaRangeToOffsetBasedIntegerRange(endNodes),
                    succ.mkString(", "), arrivalTimes.mkString(", "), withWaiting, totalTravelTime, deliveryCosts))
        val arrivalTimesAtStartNodes = startNodes.map(arrivalTimes(_)).toSet
        assertEq(delivery.inVariables.toSet, succ.toSet ++ arrivalTimesAtStartNodes)
        assertEq(
            delivery.outVariables.toSet,
            arrivalTimes.toSet -- arrivalTimesAtStartNodes ++ Set(totalTravelTime, deliveryCosts))
    }

    @Test
    def testInitialize: Unit = {
        val neighbourhood = createNeighbourhood
        val sampleSize = 1000
        for (i <- 1 to sampleSize) {
            space.initialize()
            checkArrivalTimes(now)
            checkTotalTravelTime(now)
            checkCosts(now)
            val move = neighbourhood.nextMove
            for (effect <- move.effects) {
                effect.affect(space)
            }
            neighbourhood.commit(move)
        }
    }

    @Test
    def testConsultAndCommit: Unit = {
        val neighbourhood = createNeighbourhood
        space.initialize()
        val sampleSize = 1000
        for (i <- 1 to sampleSize) {
            checkArrivalTimes(now)
            checkTotalTravelTime(now)
            checkCosts(now)
            val move = neighbourhood.nextMove
            space.consult(move)
            if (randomGenerator.nextDecision()) {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

}

/**
 * @author Michael Marte
 *
 */
object DeliveryTest {

    private def configurations =
        for (offset <- List(-1, 0, 1);
             withTimeWindows <- List(true, false);
             withWaiting <- List(true, false);
            if ! withWaiting || withTimeWindows)
            yield Vector(offset, withTimeWindows, withWaiting)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}, {2}")
    def parameters = configurations.map(_.toArray).asJava

}
