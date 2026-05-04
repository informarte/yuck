package yuck.constraints

import scala.collection.*

import yuck.core.*

final class BinPackingItem
    [Weight <: NumericalValue[Weight]]
    (val bin: IntegerVariable, val weight: Weight)
{
    override def toString = "(%s, %s)".format(bin, weight)
}

/**
 * Basis for implementing MiniZinc's ''bin_packing_load'' constraint.
 *
 * Maintains the loads for a given set of bins.
 *
 * Ignores tasks assigned to bins other than the given bins.
 */
final class BinPacking
    [Load <: NumericalValue[Load],
     LoadDomain <: NumericalDomain[Load, LoadDomain],
     LoadVariable <: NumericalVariable[Load, LoadDomain, LoadVariable]]
    (id: Id[Constraint],
     items: immutable.Seq[BinPackingItem[Load]],
     // In generic code, scalac translates == to BoxesRunTime.equals, which incurs overhead to
     // properly compare numbers of different types.
     // We avoid this overhead by using IntegerValue instead of int.
     loads: immutable.Map[IntegerValue, LoadVariable]) // bin -> load
    (using typeTraits: NumericalTypeTraits[Load, LoadDomain, LoadVariable])
    extends Constraint(id)
{

    require(items.forall(_.weight >= typeTraits.zero))
    require(loads.valuesIterator.toSet.size == loads.size)
    require(items.iterator.map(_.bin).toSet.size == items.size)

    override def toString =
        "bin_packing([%s], [%s])".format(
            items.mkString(", "),
            loads.iterator.map(item => "(%s, %s)".format(item._1, item._2)).mkString(", "))

    override def inVariables = items.view.filter(_.weight > typeTraits.zero).map(_.bin)
    override def outVariables = loads.view.values

    private val x2Item = items.view.map(item => (item.bin: AnyVariable) -> item).to(immutable.HashMap)
    private val currentLoads = new mutable.HashMap[IntegerValue, Load] // bin -> load
    private val loadDeltas = new mutable.HashMap[IntegerValue, Load] // bin -> load delta
    private val effects = loads.view
        .map((i, load) => i -> new ReusableMoveEffectWithFixedVariable(load))
        .to(immutable.HashMap) // bin -> effect

    override def initialize(now: SearchState) = {
        currentLoads.clear()
        for i <- loads.keysIterator do {
            currentLoads(i) = typeTraits.zero
        }
        for item <- items do {
            val i = now.value(item.bin)
            if currentLoads.contains(i) then {
                currentLoads(i) += item.weight
            }
        }
        for i <- loads.keysIterator do {
            val effect = effects(i)
            effect.a = currentLoads(i)
        }
        effects.view.values
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
       loadDeltas.clear()
       for x <- move do {
           val item = x2Item(x)
           val j = before.value(item.bin)
           val k = after.value(item.bin)
           if effects.contains(j) then {
               loadDeltas += j -> (loadDeltas.getOrElse(j, typeTraits.zero) - item.weight)
           }
           if effects.contains(k) then {
               loadDeltas += k -> (loadDeltas.getOrElse(k, typeTraits.zero) + item.weight)
           }
       }
       for (j, loadDelta) <- loadDeltas do {
           effects(j).a = currentLoads(j) + loadDelta
       }
       loadDeltas.view.keys.map(effects(_))
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
       for (j, loadDelta) <- loadDeltas do {
           currentLoads(j) += loadDelta
       }
       loadDeltas.view.keys.map(effects(_))
    }

}
