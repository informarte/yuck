package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Given variables x[1], ..., x[n] and an m-by-n value matrix, the constraint
 * computes the distance of (s(x[1]), ..., s(x[n])) to each row of the matrix
 * and provides the minimum distance as measure of constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 */
final class Table
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint],
     xs: immutable.IndexedSeq[X],
     private var rows: immutable.IndexedSeq[immutable.IndexedSeq[A]],
     costs: BooleanVariable,
     forceImplicitSolving: Boolean = false)
    (using typeTraits: OrderedTypeTraits[A, D, X])
    extends Constraint(id)
{

    private val n = xs.size // number of variables/ columns
    rows.foreach(row => require(row.size == n))

    private val hasDuplicateVariables = xs.toSet.size < n

    override def toString =
        "table([%s], [%s], %s)".format(
            xs.mkString(", "),
            rows.iterator.map(row => "[%s]".format(row.mkString(", "))).mkString(", "),
            costs)

    override def inVariables = xs
    override def outVariables = List(costs)

    private var cols: Vector[Vector[A]] = null // columns improve data locality

    private var currentDistances: Array[Long] = null // for each row
    protected var futureDistances: Array[Long] = null // for each row

    private val x2i: HashMap[AnyVariable, Int] =
        if hasDuplicateVariables then null else xs.view.zipWithIndex.to(HashMap)
    private val x2is: HashMap[AnyVariable, Vector[Int]] =
        if hasDuplicateVariables
        then xs.view.zipWithIndex.groupBy(_._1).view.mapValues(_.map(_._2).toVector).to(HashMap)
        else null

    private val effect = costs.reuseableEffect

    private val costModel = typeTraits.costModel

    inline private def computeDistance(a: A, b: A): Long =
        costModel.eqViolation(a, b)

    override def propagate() = {
        if costs.domain == TrueDomain && typeTraits.domainCapabilities.createDomain then {
            rows = rows.filter(row => (0 until n).forall(i => xs(i).domain.contains(row(i))))
            val effects =
                NoPropagationOccurred.pruneDomains(
                    (0 until n).iterator.map(i =>
                        val feasibleValues = rows.iterator.map(row => row(i)).toSet
                        val x = xs(i)
                        (x, x.domain.intersect(typeTraits.createDomain(feasibleValues)))))
            if ! effects.affectedVariables.isEmpty then {
                cols = null
            }
            effects
        } else {
            NoPropagationOccurred
        }
    }

    override def initialize(now: SearchState) = {
        val m = rows.size
        cols = rows.toVector.map(_.toVector).transpose
        currentDistances = new Array[Long](m)
        futureDistances = new Array[Long](m)
        for j <- 0 until m do {
            val row = rows(j)
            for i <- 0 until n do {
                currentDistances(j) =
                    safeAdd(currentDistances(j), computeDistance(now.value(xs(i)), row(i)))
            }
        }
        effect.a = BooleanValue(computeMinDistance(currentDistances))
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        if cols.eq(null) then {
            initialize(before)
        }
        Array.copy(currentDistances, 0, futureDistances, 0, rows.size)
        if hasDuplicateVariables then {
            if move.size == 1 then {
                computeFutureDistances(move, before, after, x2is(move.effects.head.x).iterator)
            } else {
                computeFutureDistances(move, before, after, move.involvedVariablesIterator.flatMap(x2is))
            }
        } else {
            if move.size == 1 then {
                computeFutureDistances(move, before, after, x2i(move.effects.head.x))
            } else {
                computeFutureDistances(move, before, after, move.involvedVariablesIterator.map(x2i))
            }
        }
        effect.a = BooleanValue(computeMinDistance(futureDistances))
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        val tmp = currentDistances
        currentDistances = futureDistances
        futureDistances = tmp
        effect
    }

    inline private def computeFutureDistances(move: Move, before: SearchState, after: SearchState, it: Iterator[Int]): Unit = {
        while it.hasNext do {
            computeFutureDistances(move, before, after, it.next())
        }
    }

    inline private def computeFutureDistances(move: Move, before: SearchState, after: SearchState, i: Int): Unit = {
        val x = xs(i)
        computeFutureDistances(cols(i), before.value(x), after.value(x))
    }

    private def computeFutureDistances(col: Vector[A], a: A, b: A): Unit = {
        var j = 0
        val m = col.size
        while j < m do {
            val c = col(j)
            futureDistances(j) =
                safeAdd(futureDistances(j), safeSub(computeDistance(b, c), computeDistance(a, c)))
            j += 1
        }
    }

    private def computeMinDistance(distances: Array[Long]): Long = {
        var result = distances(0)
        var j = 1
        val m = rows.size
        while j < m do {
            result = min(result, distances(j))
            j += 1
        }
        result
    }

    final override def isCandidateForImplicitSolving(space: Space) =
        rows.size > 1 &&
        xs.toSet.size == xs.size &&
        xs.forall(! space.isChannelVariable(_)) &&
        xs.forall(_.domain.isFinite) &&
        (forceImplicitSolving || xs.size <= 3)

    final override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: IndexedSeq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]):
        Option[Neighbourhood] =
    {
        if isCandidateForImplicitSolving(space) then {
            val xs1 = xs
            val rows1 = rows.filter(row => (0 until n).forall(i => xs(i).domain.contains(row(i))))
            if rows1.size > 1 then {
                val xs2 = xs.filterNot(_.domain.isSingleton)
                val rows2 =
                    if xs2.size == xs1.size
                    then rows1
                    else rows1.map(row => row.view.zipWithIndex.filterNot((a, i) => xs(i).domain.isSingleton).map(_._1).toVector)
                if rows2(0).size > 1 then {
                    val row = rows1(randomGenerator.nextInt(rows1.size))
                    for (x, a) <- xs1.view.zip(row.iterator) do {
                        space.setValue(x, a)
                    }
                    space.setValue(costs, True)
                    Some(new TableNeighbourhood(space, xs2, rows2, randomGenerator))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

}
