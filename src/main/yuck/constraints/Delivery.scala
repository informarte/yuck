package yuck.constraints

import scala.collection.*
import scala.ref.WeakReference
import scala.reflect.ClassTag

import yuck.core.*

/**
 * Helps with solving single-depot vehicle-routing problems with time windows.
 *
 * Maintains the arrival times, the total travel time, and the sum of time-window violations.
 *
 * The time windows are defined by the domains of the arrival-time variables.
 *
 * The implementation makes two assumptions:
 *
 *  - The variables (succ_i), offset <= i < offset + n, represent a Hamiltonian circuit at any time.
 *  - The model is based on a giant-tour representation where, in addition to the city nodes,
 *    there is a pair of start and end nodes for each vehicle.
 *
 * The implementation supports waiting.
 *
 * When computing the constraint violation, the implementation ignores the time windows
 * of the start nodes; the arrival-time variables of these nodes are considered as input.
 *
 * @see [[yuck.constraints.Circuit Circuit]]
 *
 * @author Michael Marte
 */
final class Delivery
    [Time <: NumericalValue[Time]]
    (space: WeakReference[Space],
     id: Id[Constraint], override val maybeGoal: Option[Goal],
     startNodes: IntegerDomain,
     endNodes: IntegerDomain,
     succ: immutable.IndexedSeq[IntegerVariable], offset: Int,
     arrivalTimes: immutable.IndexedSeq[NumericalVariable[Time]],
     serviceTimes: Function1[Int, Time],
     travelTimes: Function2[Int, Int, Time],
     withWaiting: Boolean,
     totalTravelTime: NumericalVariable[Time],
     costs: BooleanVariable)
    (implicit timeTraits: NumericalValueTraits[Time])
    extends Constraint(id)
{

    private val timeOps = timeTraits.numericalOperations
    private implicit val timeClassTag: ClassTag[Time] = ClassTag[Time](timeTraits.valueType)

    private val nodes = IntegerRange(offset, offset + succ.size - 1)
    require(! startNodes.isEmpty)
    require(startNodes.intersect(endNodes).isEmpty)
    require(startNodes.size == endNodes.size)
    require(startNodes.isSubsetOf(nodes))
    require(endNodes.isSubsetOf(nodes))
    require(endNodes.values.forall(i => succ(i.value - offset).domain.isSingleton))
    require(
        endNodes.values.foldLeft(IntegerValueTraits.emptyDomain){
            case (acc, i) => acc.union(succ(i.value - offset).domain)} == startNodes)
    require(succ.forall(_.domain.isSubsetOf(nodes)))
    require(arrivalTimes.size == nodes.size)
    require(arrivalTimes.toSet.size == arrivalTimes.size)

    override def toString =
        "delivery(%s, %s, [%s], [%s], ..., %s, %s, %s)"
            .format(
                startNodes, endNodes, succ.mkString(", "), arrivalTimes.mkString(", "),
                withWaiting, totalTravelTime, costs)

    override def inVariables =
        startNodes.values.view.map(i => arrivalTimes(i.value - offset)) ++ succ
    override def outVariables =
        nodes.diff(startNodes).values.view.map(i => arrivalTimes(i.value - offset)) ++ Seq(totalTravelTime, costs)

    private def checkSetup(): Unit = {
        val space = this.space.get.get
        val maybeCircuit =
            space.directlyAffectedConstraints(succ(0)).iterator
                 .filter(_.isInstanceOf[Circuit]).filter(space.isImplicitConstraint).nextOption()
        require(maybeCircuit.isDefined)
        val circuit = maybeCircuit.get
        require(circuit.inVariables.toSet == succ.toSet)
    }

    // variable -> tour (in terms of 0-based tour index)
    private val x2Tour = new mutable.AnyRefMap[AnyVariable, Int]

    private var currentTourTravelTimes: Array[Time] = null // for each tour
    private var futureTourTravelTimes: Array[Time] = null // for each tour

    private var currentTotalTravelTime = timeTraits.zero
    private var futureTotalTravelTime = timeTraits.zero

    private var currentCosts = 0L
    private var futureCosts = 0L

    private val effects = new mutable.ArrayBuffer[AnyMoveEffect] {
        override def clear() = {
            // No need to clear the underlying array!
            size0 = 0
        }
    }

    override def initialize(now: SearchState) = {
        checkSetup()
        x2Tour.clear()
        effects.clear()
        val numberOfTours = startNodes.size
        currentTourTravelTimes = new Array[Time](numberOfTours)
        futureTourTravelTimes = new Array[Time](numberOfTours)
        currentTotalTravelTime = timeTraits.zero
        currentCosts = 0
        for (i <- 0 until numberOfTours) {
            var k = startNodes.lb.value + i - offset
            x2Tour.update(arrivalTimes(k), i)
            x2Tour.update(succ(k), i)
            var time = now.value(arrivalTimes(k))
            var j = now.value(succ(k))
            var tourTravelTime = timeTraits.zero
            while (! startNodes.contains(j)) {
                val l = j.value - offset
                x2Tour.update(succ(l), i)
                val x = arrivalTimes(l)
                val dx = x.domain
                time += serviceTimes(k)
                val travelTime = travelTimes(k, l)
                time += travelTime
                tourTravelTime += travelTime
                currentTotalTravelTime += travelTime
                if (withWaiting && dx.hasLb) {
                    time = timeOps.max(dx.lb, time)
                }
                val effect = x.reuseableEffect
                effect.a = time
                effects += effect
                currentCosts = safeAdd(currentCosts, dx.distanceTo(time).toLong)
                k = l
                j = now.value(succ(k))
            }
            currentTourTravelTimes.update(i, tourTravelTime)
        }
        currentCosts = safeAdd(currentCosts, totalTravelTime.domain.distanceTo(currentTotalTravelTime).toLong)
        if (true) {
            val effect = totalTravelTime.reuseableEffect
            effect.a = currentTotalTravelTime
            effects += effect
        }
        if (true) {
            val effect = costs.reuseableEffect
            effect.a = BooleanValue(currentCosts)
            effects += effect
        }
        effects
    }

    // tours affected by the last move examined by consult (in terms of 0-based tour indices)
    private val affectedTours = new mutable.HashSet[Int]

    @inline private def distanceDelta
        (dx: NumericalDomain[Time], beforeValue: NumericalValue[Time], afterValue: NumericalValue[Time]): Long =
        safeSub(dx.distanceTo(afterValue).toLong, dx.distanceTo(beforeValue).toLong)

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effects.clear()
        Array.copy(currentTourTravelTimes, 0, futureTourTravelTimes, 0, startNodes.size)
        futureTotalTravelTime = currentTotalTravelTime
        futureCosts = currentCosts
        affectedTours.clear()
        affectedTours ++= move.involvedVariables.map(x2Tour)
        for (i <- affectedTours) {
            var k = startNodes.lb.value + i - offset
            var time = after.value(arrivalTimes(k))
            var j = after.value(succ(k))
            var tourTravelTime = timeTraits.zero
            while (! startNodes.contains(j)) {
                val l = j.value - offset
                val x = arrivalTimes(l)
                val dx = x.domain
                time += serviceTimes(k)
                val travelTime = travelTimes(k, l)
                time += travelTime
                tourTravelTime += travelTime
                if (withWaiting && dx.hasLb) {
                    time = timeOps.max(dx.lb, time)
                }
                val previousArrivalTime = before.value(x)
                if (time != previousArrivalTime) {
                    val effect = x.reuseableEffect
                    effect.a = time
                    effects += effect
                    futureCosts = safeAdd(futureCosts, distanceDelta(dx, previousArrivalTime, time))
                }
                k = l
                j = after.value(succ(k))
            }
            futureTourTravelTimes.update(i, tourTravelTime)
            futureTotalTravelTime += tourTravelTime - currentTourTravelTimes(i)
        }
        futureCosts =
            safeAdd(futureCosts, distanceDelta(totalTravelTime.domain, currentTotalTravelTime, futureTotalTravelTime))
        if (futureTotalTravelTime != currentTotalTravelTime) {
            val effect = totalTravelTime.reuseableEffect
            effect.a = futureTotalTravelTime
            effects += effect
        }
        if (futureCosts != currentCosts) {
            val effect = costs.reuseableEffect
            effect.a = BooleanValue(futureCosts)
            effects += effect
        }
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        if (true) {
            val tmp = currentTourTravelTimes
            currentTourTravelTimes = futureTourTravelTimes
            futureTourTravelTimes = tmp
        }
        currentTotalTravelTime = futureTotalTravelTime
        currentCosts = futureCosts
        for (i <- affectedTours) {
            var k = startNodes.lb.value + i - offset
            var j = after.value(succ(k))
            while (! startNodes.contains(j)) {
                val l = j.value - offset
                x2Tour.update(succ(l), i)
                k = l
                j = after.value(succ(k))
            }
        }
        effects
    }

}
