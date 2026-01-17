package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Used to implement the family of array_*_element constraints as required by FlatZinc.
 *
 * When an index is out-of-bounds, some value from the array is returned.
 * (When such a case may happen, an additional constraint is required that forces the index variable
 * to take a valid value.)
 */
final class ElementsVar
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Constraint],
     xs: immutable.IndexedSeq[X],
     is: immutable.IndexedSeq[IntegerVariable],
     ys: immutable.IndexedSeq[X],
     offset: Int)
    (using typeTraits: TypeTraits[A, D, X])
    extends Constraint(id)
{

    private val n = xs.size
    private val m = is.size

    require(n > 0)
    require(
        xs.asInstanceOf[immutable.IndexedSeq[AnyVariable]].toSet
            .diff(is.asInstanceOf[immutable.IndexedSeq[AnyVariable]].toSet) == xs.toSet)
    require(ys.size == m)
    require(ys.toSet.size == m)

    override def toString =
        "[%s] = elements([%s], [%s], %d)".format(ys.mkString(", "), xs.mkString(", "), is.mkString(", "), offset)

    override def inVariables = xs.view.appendedAll(is)
    override def outVariables = ys

    private val i2ys: HashMap[AnyVariable, Vector[X]] =
        is.view.zip(ys).groupBy(_._1).view.mapValues(_.map(_._2).toVector).to(HashMap)
    private val y2Effect = new mutable.TreeMap[X, AnyMoveEffect]
    private val effects = y2Effect.values
    private val x2ys: HashMap[AnyVariable, mutable.TreeSet[X]] =
        HashMap.newBuilder.addAll(xs.view.map((_, new mutable.TreeSet[X]))).result()

    // When i is the value of a channel variable, i may be out-of-bounds!
    // Nevertheless, we have to provide some valid index.
    inline private def safeIndex(i: IntegerValue): Int = min(max(0, safeSub(i.toInt, offset)), n - 1)

    inline private def addEffect(y: X, a: A): Unit = {
        val effect = y.reuseableEffect
        effect.a = a
        y2Effect.addOne(y, effect)
    }

    override def propagate() = {
        if typeTraits.domainCapabilities.union
        then is.view.zip(ys).foldLeft(NoPropagationOccurred: PropagationEffects)(propagate)
        else NoPropagationOccurred
    }

    private def propagate(effects: PropagationEffects, iy: (IntegerVariable, X)) = {
        val i = iy._1
        val y = iy._2
        val di1 =
            i.domain.intersect(IntegerRange(offset, safeDec(safeAdd(xs.size, offset))))
        val dy1 =
            y.domain.intersect(
                di1.valuesIterator.foldLeft(typeTraits.emptyDomain)((u, i) => u.union(xs(i.toInt - offset).domain)))
        val di2 =
            IntegerDomain(
                di1.valuesIterator.filter(i => xs(i.toInt - offset).domain.intersects(dy1)).toSet)
        effects.pruneDomains(i, di2, y, dy1)
    }

    override def initialize(now: SearchState) = {
        x2ys.keysIterator.foreach(x => x2ys(x).clear())
        y2Effect.clear()
        for (i, y) <- is.view.zip(ys) do {
            val x = xs(safeIndex(now.value(i)))
            addEffect(y, now.value(x))
            x2ys(x).addOne(y)
        }
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        y2Effect.clear()
        for effect <- move.effectsIterator do {
            val ys = i2ys.getOrElse(effect.x, Vector.empty)
            if ys.isEmpty then {
                for y <- x2ys(effect.x.asInstanceOf[X]) do {
                    if ! y2Effect.contains(y) then {
                        addEffect(y, effect.a.asInstanceOf[A])
                    }
                }
            } else {
                val a = after.value(xs(safeIndex(effect.a.asInstanceOf[IntegerValue])))
                var i = ys.size - 1
                while i >= 0 do {
                    addEffect(ys(i), a)
                    i -= 1
                }
            }
        }
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for effect <- move.effectsIterator do {
            val ys = i2ys.getOrElse(effect.x, Vector.empty)
            if ys.nonEmpty then {
                x2ys(xs(safeIndex(before.value(effect.x.asInstanceOf[IntegerVariable])))).subtractAll(ys)
                x2ys(xs(safeIndex(effect.a.asInstanceOf[IntegerValue]))).addAll(ys)
            }
        }
        effects
    }

}
