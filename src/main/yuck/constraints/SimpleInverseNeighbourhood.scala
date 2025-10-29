package yuck.constraints

import yuck.core.*

/**
 * This neighbourhood can be used to maintain an ''inverse'' constraint where
 *
 *  - f and g do not share variables
 *  - every x in f can be paired with every y in g and vice versa.
 *
 * @author Michael Marte
 */
final class SimpleInverseNeighbourhood
    (override protected val space: Space,
     f: InverseFunction,
     g: InverseFunction,
     randomGenerator: RandomGenerator)
    extends InverseNeighbourhood(f, g)
{

    require(f.xs.toSet.intersect(g.xs.toSet).isEmpty)
    require(f.xs.forall(x => x.domain == g.indexDomain))
    require(g.xs.forall(x => x.domain == f.indexDomain))

    private val effects = Vector.fill(4)(new ReusableMoveEffect[IntegerValue])

    override def nextMove() = {
        val i1 = randomGenerator.nextInt(n)
        val x1 = f.xs(i1)
        val a1 = value(x1)
        val j1 = a1.toInt - g.offset
        val y1 = g.xs(j1)
        val b1 = value(y1)
        assert(b1.toInt - f.offset == i1)
        val i2 = {
            val i = randomGenerator.nextInt(n - 1)
            if (i < i1) i else i + 1
        }
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
