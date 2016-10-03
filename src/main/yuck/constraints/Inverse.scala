package yuck.constraints

import scala.collection._

import yuck.core._

// This class is an implementation detail of Inverse, but we need it in the primary
// constructor of Inverse.
final private class InverseFunction(
    val xs: immutable.IndexedSeq[Variable[IntegerValue]],
    val offset: Int)
{
    val range = offset until offset + xs.size
    val x2i = new immutable.HashMap[AnyVariable, Int] ++ xs.zip(range)
    val refs = new Array[mutable.HashSet[Int]](xs.size)
    val visited = new Array[Int](xs.size)
    @inline def size = xs.size
}

/**
 * Used to implement the ''inverse'' constraint as specified by MiniZinc.
 *
 * Given variables f[fOffset], ..., f[fOffset + n - 1] and g[gOffset], ..., g[gOffset + m - 1],
 * the constraint computes
 *
 *  - |s(g[s(f[i])]) - i| for fOffset <= i < fOffset + n and
 *  - |s(f[s(g[j])]) - j| for gOffset <= j < gOffset + m,
 *
 * and provides the sum of these terms as measure of constraint violation.
 *
 * Out-of-bounds indices are ignored and hence both f and g may contain channel variables.
 * (In this case, additional constraints are required that force the channel variables
 * to take valid values.)
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Inverse private
    (id: Id[Constraint], goal: Goal,
     f: InverseFunction,
     g: InverseFunction,
     costs: Variable[IntegerValue])
    extends Constraint(id, goal)
{

    def this(
        id: Id[Constraint], goal: Goal,
        f: immutable.IndexedSeq[Variable[IntegerValue]], fOffset: Int,
        g: immutable.IndexedSeq[Variable[IntegerValue]], gOffset: Int,
        costs: Variable[IntegerValue]) =
        this(id, goal, new InverseFunction(f, fOffset), new InverseFunction(g, gOffset), costs)

    override def toString =
        "inverse([%s], %d, [%s], %d, %s)".format(
            f.xs.mkString(", "), f.offset, g.xs.mkString(", "), g.offset, costs)
    override def inVariables = f.xs.toIterator ++ g.xs.toIterator
    override def outVariables = List(costs)

    private val effects = List(new ReusableEffectWithFixedVariable[IntegerValue](costs))
    private val effect = effects.head
    private var currentCosts = 0
    private var futureCosts = 0
    private val debug = false

    private def computeCosts(
        f: InverseFunction, g: InverseFunction, i: Int, searchState: SearchState): Int =
    {
        val j = searchState.value(f.xs(i - f.offset)).value
        if (g.range.contains(j)) scala.math.abs(searchState.value(g.xs(j - g.offset)).value - i)
        else 0
    }

    override def initialize(now: SearchState) = {
        currentCosts = 0
        for (i <- 0 until f.size) {
            f.visited(i) = -1
            f.refs(i) = new mutable.HashSet[Int]
        }
        for (j <- 0 until g.size) {
            g.visited(j) = -1
            g.refs(j) = new mutable.HashSet[Int]
        }
        for (i <- f.offset until f.offset + f.size) {
            currentCosts += computeCosts(f, g, i, now)
            val j = now.value(f.xs(i - f.offset)).value
            if (g.range.contains(j)) {
                g.refs(j - g.offset) += i
            }
        }
        for (j <- g.offset until g.offset + g.size) {
            currentCosts += computeCosts(g, f, j, now)
            val i = now.value(g.xs(j - g.offset)).value
            if (f.range.contains(i)) {
                f.refs(i - f.offset) += j
            }
        }
        assert(currentCosts >= 0)
        effect.a = IntegerValue.get(currentCosts)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        def computeCostDelta(f: InverseFunction, g: InverseFunction, i: Int, visited: Array[Int]): Int = {
            if (! f.range.contains(i) || visited(i - f.offset) == move.id.rawId) {
                0
            } else {
                visited(i - f.offset) = move.id.rawId
                val delta = computeCosts(f, g, i, after) - computeCosts(f, g, i, before)
                delta
            }
        }
        futureCosts = currentCosts
        for (x <- move.involvedVariables) {
            val maybeI = f.x2i.get(x)
            if (maybeI.isDefined) {
                val i = maybeI.get
                val x = f.xs(i - f.offset)
                futureCosts += computeCostDelta(f, g, i, f.visited)
                for (j <- f.refs(i - f.offset)) {
                    futureCosts += computeCostDelta(g, f, j, g.visited)
                }
                futureCosts += computeCostDelta(g, f, after.value(x).value, g.visited)
            }
            val maybeJ = g.x2i.get(x)
            if (maybeJ.isDefined) {
                val j = maybeJ.get
                val y = g.xs(j - g.offset)
                futureCosts += computeCostDelta(g, f, j, g.visited)
                for (i <- g.refs(j - g.offset)) {
                    futureCosts += computeCostDelta(f, g, i, f.visited)
                }
                futureCosts += computeCostDelta(f, g, after.value(y).value, f.visited)
            }
        }
        if (debug) {
            for (i <- 0 until f.visited.size if f.visited(i) != move.id.rawId) {
                assert(computeCostDelta(f, g, i + f.offset, f.visited) == 0)
            }
            for (j <- 0 until g.visited.size if g.visited(j) != move.id.rawId) {
                assert(computeCostDelta(g, f, j + g.offset, g.visited) == 0)
            }
        }
        assert(futureCosts >= 0)
        effect.a = IntegerValue.get(futureCosts)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for (x <- move.involvedVariables) {
            val maybeI = f.x2i.get(x)
            if (maybeI.isDefined) {
                val i = maybeI.get
                val x = f.xs(i - f.offset)
                val jBefore = before.value(x).value
                if (g.range.contains(jBefore)) {
                    g.refs(jBefore - g.offset) -= i
                }
                val jAfter = after.value(x).value
                if (g.range.contains(jAfter)) {
                    g.refs(jAfter - g.offset) += i
                }
            }
            val maybeJ = g.x2i.get(x)
            if (maybeJ.isDefined) {
                val j = maybeJ.get
                val y = g.xs(j - g.offset)
                val iBefore = before.value(y).value
                if (f.range.contains(iBefore)) {
                    f.refs(iBefore - f.offset) -= j
                }
                val iAfter = after.value(y).value
                if (f.range.contains(iAfter)) {
                    f.refs(iAfter - f.offset) += j
                }
            }
        }
        currentCosts = futureCosts
        effects
    }

}
