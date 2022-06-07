package yuck.constraints

import yuck.core.*

/**
 * This neighbourhood can be used to maintain an ''inverse'' constraint with
 * f = g.
 *
 * @author Michael Marte
 */
final class SelfInverseNeighbourhood
    (space: Space, f: InverseFunction, randomGenerator: RandomGenerator)
    extends InverseNeighbourhood(space, f, f)
{

    private val n = f.xs.size

    require(n > 2)
    require(n % 2 == 0)
    require(f.xs.forall(x => x.domain == f.indexDomain))

    private val effects = Vector.fill(4){new ReusableMoveEffect[IntegerValue]}

    override def nextMove = {
        val i1 = randomGenerator.nextInt(n)
        val x1 = f.xs(i1)
        val a1 = value(x1)
        val i2 = a1.toInt - f.offset
        val x2 = f.xs(i2)
        val a2 = value(x2)
        assert(a2.toInt - f.offset == i1)
        val i3 = {
            val i = randomGenerator.nextInt(n - 2)
            if (i < min(i1, i2)) i else if (i > max(i1, i2) - 2) i + 2 else i + 1
        }
        val x3 = f.xs(i3)
        val a3 = value(x3)
        val i4 = a3.toInt - f.offset
        val x4 = f.xs(i4)
        val a4 = value(x4)
        assert(a4.toInt - f.offset == i3)
        // {(x1, x2), (x3, x4)} -> {(x1, x3), (x2, x4)}
        effects(0).set(x1, a3)
        effects(1).set(x3, a1)
        effects(2).set(x2, a4)
        effects(3).set(x4, a2)
        new ChangeValues(space.nextMoveId(), effects)
    }

}
