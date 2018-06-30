package yuck.constraints

import scala.collection._
import scala.math.max

import yuck.core._

/**
 * A data structure to provide a single task to a [[yuck.constraints.Cumulative Cumulative]] constraint.
 *
 * @author Michael Marte
 *
 */
final class CumulativeTask(
    val s: Variable[IntegerValue], val d: Variable[IntegerValue], val c: Variable[IntegerValue])
{
    override def toString = "(%s, %s, %s)".format(s, d, c)
}

/**
 * Implements the ''cumulative'' constraint as specified by MiniZinc.
 *
 * Keeps track of resource consumption for each time slot in order to provide the amount
 * of unsatisfied requirements (summed up over time) as measure of constraint violation.
 *
 * @author Michael Marte
 */
final class Cumulative
    (id: Id[Constraint], goal: Goal,
     tasks: immutable.Seq[CumulativeTask], ub: Variable[IntegerValue],
     costs: Variable[BooleanValue])
    extends Constraint(id, goal)
{

    override def toString = "cumulative([%s], %s, %s)".format(tasks.mkString(", "), ub, costs)
    override def inVariables = var2Task.keysIterator ++ List(ub).toIterator
    override def outVariables = List(costs)

    private val var2Task =
        new immutable.HashMap[AnyVariable, CumulativeTask] ++
        (tasks.map(_.s).zip(tasks)) ++ (tasks.map(_.d).zip(tasks)) ++ (tasks.map(_.c).zip(tasks))
    private val effects = List(new ReusableEffectWithFixedVariable[BooleanValue](costs))
    private val effect = effects.head
    private type Profile = immutable.HashMap[Int, Int] // time slot -> resource consumption
    private var currentProfile: Profile = null
    private var futureProfile: Profile = null
    private var currentCosts = 0
    private var futureCosts = 0
    private def computeCosts(profile: Profile, ub: Int): Int =
        profile.toIterator.map{case (_, c) => computeLocalCosts(c, ub)}.foldLeft(0)(safeAdd)
    @inline private def computeLocalCosts(c: Int, ub: Int): Int = max(safeSub(c, ub), 0)

    override def initialize(now: SearchState) = {
        currentProfile = new Profile
        for (t <- tasks) {
            val s = now.value(t.s).value
            val d = now.value(t.d).value
            val c = now.value(t.c).value
            for (i <- s until safeAdd(s, d)) {
                currentProfile = currentProfile.updated(i, safeAdd(currentProfile.getOrElse(i, 0), c))
            }
        }
        currentCosts = computeCosts(currentProfile, now.value(ub).value)
        assert(currentCosts >= 0)
        effect.a = BooleanValue.get(currentCosts)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureProfile = currentProfile
        futureCosts = currentCosts
        val ubChanged = move.involves(ub)
        def processTask(t: CumulativeTask) {
            val s0 = before.value(t.s).value
            val d0 = before.value(t.d).value
            val c0 = before.value(t.c).value
            val ub0 = before.value(ub).value
            val s1 = after.value(t.s).value
            val d1 = after.value(t.d).value
            val c1 = after.value(t.c).value
            val ub1 = after.value(ub).value
            def subtractTasks(from: Int, to: Int) {
                var i = from
                while (i <= to) {
                    val r0 = futureProfile(i)
                    val r1 = safeSub(r0, c0)
                    futureProfile = futureProfile.updated(i, r1)
                    if (! ubChanged) {
                        futureCosts = safeSub(futureCosts, computeLocalCosts(r0, ub0))
                        futureCosts = safeAdd(futureCosts, computeLocalCosts(r1, ub0))
                    }
                    i = safeInc(i)
                }
            }
            def addTasks(from: Int, to: Int) {
                var i = from
                while (i <= to) {
                    val r0 = futureProfile.getOrElse(i, 0)
                    val r1 = safeAdd(r0, c1)
                    futureProfile = futureProfile.updated(i, r1)
                    if (! ubChanged) {
                        futureCosts = safeSub(futureCosts, computeLocalCosts(r0, ub1))
                        futureCosts = safeAdd(futureCosts, computeLocalCosts(r1, ub1))
                    }
                    i = safeInc(i)
                }
            }
            // When only the task start changes, the old and the new rectangle are expected to overlap
            // by about 50% on average.
            if (d0 == d1 && c0 == c1 && s1 > s0 && s1 < safeAdd(s0, d0)) {
                // duration and resource consumption did not change, old and new rectangles overlap
                // s0 ********** s0 + d0
                //       s1 ********** s1 + d1 (== d0)
                //          ^^^^
                //          Nothing changes where the rectangles overlap!
                subtractTasks(s0, safeDec(s1))
                addTasks(safeAdd(s0, d0), safeDec(safeAdd(s1, d1)))
            }
            else if (d0 == d1 && c0 == c1 && s0 > s1 && s0 < safeAdd(s1, d1)) {
                // symmetrical case
                //       s0 ********** s0 + d0
                // s1 ********** s1 + d1 (== d0)
                //          ^^^^
                //          Nothing changes where the rectangles overlap!
                addTasks(s1, safeDec(s0))
                subtractTasks(safeAdd(s1, d1), safeDec(safeAdd(s0, d0)))
            }
            else {
                subtractTasks(s0, safeDec(safeAdd(s0, d0)))
                addTasks(s1, safeDec(safeAdd(s1, d1)))
            }
        }
        val visited = new mutable.HashSet[Variable[IntegerValue]]
        for (x <- move.involvedVariables) {
            if (x != ub) {
                val t = var2Task(x)
                if (! visited.contains(t.s)) {
                    visited += t.s
                    processTask(t)
                }
            }
        }
        if (ubChanged) {
            futureCosts = computeCosts(futureProfile, after.value(ub).value)
        }
        assert(futureCosts >= 0)
        effect.a = BooleanValue.get(futureCosts)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        currentProfile = futureProfile
        currentCosts = futureCosts
        effects
    }

}
