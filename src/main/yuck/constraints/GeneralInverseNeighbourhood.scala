package yuck.constraints

import yuck.core.*

/**
 * This neighbourhood can be used to maintain an ''inverse'' constraint where
 * f and g do not share variables.
 *
 * @author Michael Marte
 */
final class GeneralInverseNeighbourhood
    (space: Space, f: InverseFunction, g: InverseFunction, randomGenerator: RandomGenerator)
    extends InverseNeighbourhood(space, f, g)
{

    require(f.xs.toSet.intersect(g.xs.toSet).isEmpty)
    require(f.xs.forall(x => x.domain.isSubsetOf(g.indexDomain)))
    require(g.xs.forall(x => x.domain.isSubsetOf(f.indexDomain)))

    private val effects = Vector.fill(4)(new ReusableMoveEffect[IntegerValue])
    private val candidates1 =
        f.xs.indices.iterator
        .filter(i => f.xs(i).domain.size > 1 && g.xs(rawValue(f.xs(i)) - g.offset).domain.size > 1)
        .toVector

    override def nextMove() = {
        if (candidates1.isEmpty) {
            new ChangeValues[IntegerValue](space.nextMoveId(), Nil)
        } else {
            val i1 = candidates1(randomGenerator.nextInt(candidates1.size))
            val x1 = f.xs(i1)
            val a1 = value(x1)
            val j1 = a1.toInt - g.offset
            val y1 = g.xs(j1)
            val b1 = value(y1)
            assert(b1.toInt - f.offset == i1)
            val candidates2 =
                y1
                .domain
                .valuesIterator
                .map(_.toInt - f.offset)
                .filter(i2 => i2 != i1)
                .filter(
                    i2 => {
                        val x2 = f.xs(i2)
                        val a2 = value(x2)
                        val j2 = a2.toInt - g.offset
                        val y2 = g.xs(j2)
                        val b2 = value(y2)
                        assert(b2.toInt - f.offset == i2)
                        x1.domain.contains(a2) && x2.domain.contains(a1) &&
                        y1.domain.contains(b2) && y2.domain.contains(b1)
                    })
                .toVector
            if (candidates2.isEmpty) {
                new ChangeValues[IntegerValue](space.nextMoveId(), Nil)
            } else {
                val i2 = candidates2(randomGenerator.nextInt(candidates2.size))
                val x2 = f.xs(i2)
                val a2 = value(x2)
                val j2 = a2.toInt - g.offset
                val y2 = g.xs(j2)
                val b2 = value(y2)
                assert(b2.toInt - f.offset == i2)
                // {(x1, y1), (x2, y2)} -> {(x1, y2), (x2, y1)}
                effects(0).set(x1, a2)
                effects(1).set(x2, a1)
                effects(2).set(y1, b2)
                effects(3).set(y2, b1)
                new ChangeValues(space.nextMoveId(), effects)
            }
        }
    }

}
