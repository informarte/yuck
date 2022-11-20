package yuck.constraints

import scala.annotation.tailrec
import scala.collection.*

import yuck.core.*

/**
 * This neighbourhood can be used to maintain any ''all_different_int'' constraint.
 *
 * @author Michael Marte
 */
final class AlldistinctNeighbourhood
    [V <: Value[V]]
    (space: Space,
     xs: immutable.IndexedSeq[Variable[V]],
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution)
    (using valueTraits: ValueTraits[V])
    extends Neighbourhood
{

    private val n = xs.size
    private def value(x: Variable[V]) = space.searchState.value(x)

    require(n > 1)
    require(xs.toSet.size == n)
    require(xs.forall(space.isSearchVariable))
    require(xs.forall(_.domain.isFinite))
    require(xs.forall(_.hasValidValue(space)))
    require(xs.map(value).toSet.size == n)

    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    private val probabilityOfSwappingInValues = moveSizeDistribution.probability(1)
    private val variablesHaveTheSameDomain = xs.forall(x => x.domain == xs.head.domain)
    private val swappingInValuesIsPossible = xs.iterator.flatMap(_.domain.valuesIterator).toSet.size > n
    private val effects = Vector.fill(3){new ReusableMoveEffect[V]}
    private val swaps = for (n <- 1 to 3) yield effects.take(n)
    private def succeed(n: Int): Move = new ChangeValues[V](space.nextMoveId(), swaps(n - 1))
    private def fail: Move = new ChangeValues[V](space.nextMoveId(), Nil)

    @tailrec
    private def nextMove(m: Int): Move = {
        require(m <= n)
        val swapInAValue =
            swappingInValuesIsPossible &&
            (m == 1 || randomGenerator.nextDecision(probabilityOfSwappingInValues))
        if (swapInAValue) {
            val usedValues = valueTraits.createDomain(xs.iterator.map(value).toSet)
            if (m == 1) {
                val candidates =
                    randomGenerator
                    .lazyShuffle(xs)
                    .map(x => (x, (x.domain.diff(usedValues))))
                    .filter(! _._2.isEmpty)
                val (x, unusedValues) = candidates.next()
                val u = unusedValues.randomValue(randomGenerator)
                // {(x, a)} -> {(x, u)}
                effects(0).set(x, u)
                succeed(1)
            } else if (variablesHaveTheSameDomain) {
                val candidates =
                    randomGenerator
                    .lazyShuffle(xs.indices)
                    .map(i => (i, (xs(i).domain.diff(usedValues))))
                    .filter(! _._2.isEmpty)
                val (i, unusedValues) = candidates.next()
                val x = xs(i)
                val a = value(x)
                val u = unusedValues.randomValue(randomGenerator)
                val j = {
                    val k = randomGenerator.nextInt(n - 1)
                    if (k < i) k else k + 1
                }
                val y = xs(j)
                if (m == 2) {
                    // {(x, a), (y, b)} -> {(x, u), (y, a)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    succeed(2)
                } else {
                    val b = value(y)
                    val k = {
                        val l = randomGenerator.nextInt(n - 2)
                        if (l < min(i, j)) l else if (l > max(i, j) - 2) l + 2 else l + 1
                    }
                    val z = xs(k)
                    // {(x, a), (y, b), (z, c)} -> {(x, u), (y, a), (z, b)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    effects(2).set(z, b)
                    succeed(3)
                }
            } else if (m == 2) {
                val candidates =
                    for {
                        x <- randomGenerator.lazyShuffle(xs)
                        unusedValues = x.domain.diff(usedValues)
                        if ! unusedValues.isEmpty
                        a = value(x)
                        ys = xs.filter(y => y != x && y.domain.contains(a))
                        y <- randomGenerator.lazyShuffle(ys)
                    } yield {
                        (x, unusedValues, y)
                    }
                if (candidates.hasNext){
                    val (x, unusedValues, y) = candidates.next()
                    val a = value(x)
                    val u = unusedValues.randomValue(randomGenerator)
                    // {(x, a), (y, b)} -> {(x, u), (y, a)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    succeed(2)
                } else {
                    nextMove(1)
                }
            } else {
                val candidates =
                    for {
                        x <- randomGenerator.lazyShuffle(xs)
                        unusedValues = x.domain.diff(usedValues)
                        if ! unusedValues.isEmpty
                        a = value(x)
                        ys = xs.filter(y => y != x && y.domain.contains(a))
                        y <- randomGenerator.lazyShuffle(ys)
                        b = value(y)
                        zs = xs.filter(z => x != z && y != z && z.domain.contains(b))
                        z <- randomGenerator.lazyShuffle(zs)
                    } yield {
                        (x, unusedValues, y, z)
                    }
                if (candidates.hasNext) {
                    val (x, unusedValues, y, z) = candidates.next()
                    val (a, b) = (value(x), value(y))
                    val u = unusedValues.randomValue(randomGenerator)
                    // {(x, a), (y, b), (z, c)} -> {(x, u), (y, a), (z, b)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    effects(2).set(z, b)
                    succeed(3)
                } else {
                    nextMove(2)
                }
            }
        } else if (variablesHaveTheSameDomain) {
            val i = randomGenerator.nextInt(n)
            val x = xs(i)
            val a = value(x)
            val j = {
                val k = randomGenerator.nextInt(n - 1)
                if (k < i) k else k + 1
            }
            val y = xs(j)
            val b = value(y)
            if (m == 2) {
                // {(x, a), (y, b)} -> {(x, b), (y, a)}
                effects(0).set(x, b)
                effects(1).set(y, a)
                succeed(2)
            } else {
                val k = {
                    val l = randomGenerator.nextInt(n - 2)
                    if (l < min(i, j)) l else if (l > max(i, j) - 2) l + 2 else l + 1
                }
                val z = xs(k)
                val c = value(z)
                // {(x, a), (y, b), (z, c)} -> {(x, c), (y, a), (z, b)}
                effects(0).set(x, c)
                effects(1).set(y, a)
                effects(2).set(z, b)
                succeed(3)
            }
        } else if (m == 2) {
            val candidates =
                for {
                    x <- randomGenerator.lazyShuffle(xs)
                    a = value(x)
                    ys = xs.filter(y => y != x && x.domain.contains(value(y)) && y.domain.contains(a))
                    y <- randomGenerator.lazyShuffle(ys)
                } yield {
                    (x, y)
                }
            if (candidates.hasNext) {
                val (x, y) = candidates.next()
                val (a, b) = (value(x), value(y))
                // {(x, a), (y, b)} -> {(x, b), (y, a)}
                effects(0).set(x, b)
                effects(1).set(y, a)
                succeed(2)
            } else if (swappingInValuesIsPossible) {
                nextMove(1)
            } else {
                fail
            }
        } else {
            val candidates =
                for {
                    x <- randomGenerator.lazyShuffle(xs)
                    a = value(x)
                    ys = xs.filter(y => y != x && y.domain.contains(a))
                    y <- randomGenerator.lazyShuffle(ys)
                    b = value(y)
                    zs = xs.filter(z => x != z && y != z && x.domain.contains(value(z)) && z.domain.contains(b))
                    z <- randomGenerator.lazyShuffle(zs)
                } yield {
                    (x, y, z)
                }
            if (candidates.hasNext) {
                val (x, y, z) = candidates.next()
                val (a, b, c) = (value(x), value(y), value(z))
                // {(x, a), (y, b), (z, c)} -> {(x, c), (y, a), (z, b)}
                effects(0).set(x, c)
                effects(1).set(y, a)
                effects(2).set(z, b)
                succeed(3)
            } else {
                nextMove(2)
            }
        }
    }

    override def searchVariables = xs.toSet

    override def children = Nil

    override def nextMove = {
        val m1 = min(n, moveSizeDistribution.nextIndex(randomGenerator))
        val m2 = if (swappingInValuesIsPossible) m1 else max(m1, 2)
        nextMove(m2)
    }

}
