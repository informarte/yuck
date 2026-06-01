package yuck.constraints.test

import scala.collection.*
import scala.language.implicitConversions
import scala.ref.WeakReference

import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.{Circuit, Delivery}
import yuck.core.*
import yuck.test.util.UnitTest

@ParameterizedClass
@MethodSource(Array("parameters"))
final class DeliveryTest(offset: Int, withTimeWindows: Boolean, withWaiting: Boolean) extends UnitTest {

    private val numberOfCities = 15
    private val numberOfVehicles = 3

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val cityNodes = Range.inclusive(0, numberOfCities - 1)
    private val startNodes = Range.inclusive(numberOfCities, numberOfCities + numberOfVehicles - 1)
    private val endNodes = Range.inclusive(numberOfCities + numberOfVehicles, numberOfCities + 2 * numberOfVehicles - 1)
    private val nodes = Range.inclusive(cityNodes.start, endNodes.end)
    private implicit def zeroBasedScalaRangeToOffsetBasedIntegerRange(range: Range): IntegerRange = {
        require(range.step == 1)
        if range.isEmpty
        then EmptyIntegerRange
        else if range.isInclusive
        then IntegerRange(offset + range.start, offset + range.end)
        else IntegerRange(offset + range.start, offset + range.end - 1)
    }
    private val succ = nodes.map(i => new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), nodes))
    for i <- endNodes do {
        val j = if i == endNodes.end then startNodes.start else startNodes.start + (i - endNodes.start) + 1
        succ(i).pruneDomain(Range.inclusive(j, j))
    }
    private val circuitCosts = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)
    private val circuit = new Circuit(space.nextConstraintId(), succ, offset, circuitCosts)
    private val serviceTimes0 = nodes.map(_ => IntegerValue(randomGenerator.nextInt(numberOfCities)))
    private val serviceTimes = serviceTimes0.apply
    private val travelTimes0 = nodes.map(_ => nodes.map(_ => IntegerValue(randomGenerator.nextInt(numberOfCities) + 1)))
    private val travelTimes = (i: Int, j: Int) => travelTimes0(i)(j)
    private val timeRange = IntegerRange(0, nodes.map(i => nodes.map(j => travelTimes(i, j).value).max).sum)
    private val arrivalTimes =
        for i <- nodes yield
            new IntegerVariable(
                space.nextVariableId(), "x%d".format(i + 1),
                if withTimeWindows then timeRange.randomSubrange(randomGenerator) else timeRange)
    for i <- startNodes do {
        space.setValue(arrivalTimes(i), arrivalTimes(i).domain.lb)
    }
    private val totalTravelTime =
        new IntegerVariable(
            space.nextVariableId(),
            "totalTravelTime",
            if randomGenerator.nextDecision()
            then IntegerRange(timeRange.ub, timeRange.ub)
            else IntegerRange(timeRange.lb, timeRange.lb))
    private val deliveryCosts =
        new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)
    private val delivery =
        new Delivery(
            WeakReference(space), space.nextConstraintId(), startNodes, endNodes, succ, offset,
            arrivalTimes, serviceTimes, travelTimes, withWaiting, totalTravelTime, deliveryCosts)

    private def createNeighbourhood() = {
        space.post(circuit).registerImplicitConstraint(circuit).post(delivery)
        circuit.createNeighbourhood(space, randomGenerator, logger, sigint, DefaultMoveSizeDistribution).get
    }

    private def checkArrivalTimes(searchState: SearchState): Unit = {
        for i <- nodes if ! endNodes.contains(i) do {
            val j = searchState.value(succ(i)).toInt - offset
            val (x, y) = (arrivalTimes(i), arrivalTimes(j))
            val (a, b) = (searchState.value(x), searchState.value(y))
            val c = a + serviceTimes(i) + travelTimes(i, j)
            assertEq(b, if withWaiting then IntegerValue.max(y.domain.lb, c) else c)
        }
    }

    private def checkTotalTravelTime(searchState: SearchState): Unit = {
        var expectedTotalTravelTime = Zero
        for i <- nodes if ! endNodes.contains(i) do {
            val j = searchState.value(succ(i)).toInt - offset
            expectedTotalTravelTime += travelTimes(i, j)
        }
        assertEq(searchState.value(totalTravelTime), expectedTotalTravelTime)
    }

    private def checkCosts(searchState: SearchState): Unit = {
        assertEq(searchState.value(circuitCosts), True)
        val expectedViolation =
            ((if withTimeWindows then arrivalTimes else Nil).view :+ totalTravelTime)
                .map(x => x.domain.distanceTo(searchState.value(x)).value).sum
        assertEq(searchState.value(deliveryCosts), BooleanValue(expectedViolation))
    }

    @Test
    def testBasics(): Unit = {
        assertEq(
            delivery.toString,
            "delivery(%s, %s, [%s], [%s], ..., %s, %s, %s)".format(
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
    def testCopyingWithoutReplacement(): Unit = {
        val copy = delivery.copy(Map.empty).asInstanceOf[Delivery[?, ?, ?]]
        assert(! copy.eq(delivery))
        testConfiguration(copy, Map.empty)
    }

    @Test
    def testCopyingWithReplacement(): Unit = {
        val deliveryCosts1 = BooleanTypeTraits.createChannel(space)
        val totalTravelTime1 = IntegerTypeTraits.createChannel(space)
        val replacements: Map[AnyVariable, AnyVariable] =
            Map(totalTravelTime -> totalTravelTime1, deliveryCosts -> deliveryCosts1)
        val copy = delivery.copy(replacements).asInstanceOf[Delivery[?, ?, ?]]
        testConfiguration(copy, replacements)
    }

    private def testConfiguration(
        constraint: Delivery[?, ?, ?],
        replacements: Map[AnyVariable, AnyVariable]):
        Unit =
    {
        assertEq(constraint.startNodes, delivery.startNodes)
        assertEq(constraint.endNodes, delivery.endNodes)
        assertEq(constraint.succ, succ)
        assertEq(constraint.offset, offset)
        assertEq(constraint.arrivalTimes, arrivalTimes)
        assertEq(constraint.serviceTimes, serviceTimes)
        assertEq(constraint.travelTimes, travelTimes)
        assertEq(constraint.withWaiting, withWaiting)
        assertEq(constraint.totalTravelTime, replacements.getOrElse(totalTravelTime, totalTravelTime))
        assertEq(constraint.costs, replacements.getOrElse(deliveryCosts, deliveryCosts))
    }

    @Test
    def testInitialize(): Unit = {
        val neighbourhood = createNeighbourhood()
        val sampleSize = 1000
        for i <- 1 to sampleSize do {
            space.initialize()
            checkArrivalTimes(now)
            checkTotalTravelTime(now)
            checkCosts(now)
            val move = neighbourhood.nextMove()
            for effect <- move.effects do {
                effect.affect(space)
            }
            neighbourhood.commit(move)
        }
    }

    @Test
    def testConsultAndCommit(): Unit = {
        val neighbourhood = createNeighbourhood()
        space.initialize()
        val sampleSize = 1000
        for i <- 1 to sampleSize do {
            checkArrivalTimes(now)
            checkTotalTravelTime(now)
            checkCosts(now)
            val move = neighbourhood.nextMove()
            space.consult(move)
            if randomGenerator.nextDecision() then {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

}

object DeliveryTest {

    private def configurations =
        for offset <- List(-1, 0, 1)
            withTimeWindows <- List(true, false)
            withWaiting <- List(true, false)
            if ! withWaiting || withTimeWindows
        yield Array(offset, withTimeWindows, withWaiting)

    def parameters = configurations.toArray

}
