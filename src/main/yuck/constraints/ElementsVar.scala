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

    private val effects = ys.map(y => new ReusableMoveEffectWithFixedVariable(y))

    // Maps each i to the ys it affects.
    // Formally, i2Effects(i) = {(i, {effect | (j, effect) <- is zip effects, j = i}) | i <- is}.
    // (If i occurs n times in is, then i2Effects(i) has size n.)
    private val i2Effects: HashMap[AnyVariable, Vector[ReusableMoveEffectWithFixedVariable[A, D, X]]] =
        is.view.zip(effects).groupBy(_._1).view.mapValues(_.map(_._2).toVector).to(HashMap)

    // Maps each x to the ys it currently affects.
    // (Maintained by initialize and commit, used by consult.)
    private val x2Effects: HashMap[X, mutable.HashSet[ReusableMoveEffectWithFixedVariable[A, D, X]]] =
        HashMap.newBuilder
            .addAll(xs.view.map((_, new mutable.HashSet[ReusableMoveEffectWithFixedVariable[A, D, X]])))
            .result()

    private val result = new mutable.HashSet[MoveEffect[A, D, X]]

    // When i is the value of a channel variable, i may be out-of-bounds!
    // Nevertheless, we have to provide some valid index.
    inline private def safeIndex(i: IntegerValue): Int = min(max(0, safeSub(i.toInt, offset)), n - 1)

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
        for (effects <- x2Effects.values) {
            effects.clear()
        }
        for (i, effect) <- is.view.zip(effects) do {
            val x = xs(safeIndex(now.value(i)))
            effect.a = now.value(x)
            x2Effects(x).addOne(effect)
        }
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        result.clear()
        for inEffect <- move.effectsIterator do {
            val outEffects = i2Effects.getOrElse(inEffect.x, Vector.empty)
            if outEffects.isEmpty then {
                // inEffect affects some x in xs.
                val x = inEffect.x.asInstanceOf[X]
                val a = inEffect.a.asInstanceOf[A]
                for outEffect <- x2Effects(x) do {
                    // Now we have to be careful.
                    // If outEffect was already updated, there is no need to do it again.
                    // Moreover, such an update can only have happened by processing an index change,
                    // implying that outEffect is not affected by inEffect in the after state.
                    if ! result.contains(outEffect) then {
                        outEffect.a = a
                        result += outEffect
                    }
                }
            } else {
                // inEffect affects some i in is.
                val a = after.value(xs(safeIndex(inEffect.a.asInstanceOf[IntegerValue])))
                for outEffect <- outEffects do {
                    // Notice that outEffect might already have been updated by processing a change
                    // to some x in xs. However, such an update would have happened under the assumption
                    // that x2Effects(x) still applies, but this is not the case in the after state due
                    // to the change to the index variable inEffect.x which we are processing here.
                    // So we have to update outEffect in any case.
                    outEffect.a = a
                    result += outEffect
                }
            }
        }
        result
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for inEffect <- move.effectsIterator do {
            val outEffects = i2Effects.getOrElse(inEffect.x, Vector.empty)
            if outEffects.nonEmpty then {
                // inEffect affects some i in is.
                val i = inEffect.x.asInstanceOf[IntegerVariable]
                val b = inEffect.a.asInstanceOf[IntegerValue]
                x2Effects(xs(safeIndex(before.value(i)))).subtractAll(outEffects)
                x2Effects(xs(safeIndex(b))).addAll(outEffects)
            }
        }
        result
    }

}
