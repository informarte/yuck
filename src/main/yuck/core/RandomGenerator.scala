package yuck.core

import scala.collection.*

import it.unimi.dsi.fastutil.ints.Int2IntOpenHashMap

/**
 * Provides an interface for random generation of decisions, integers, and probabilities.
 */
abstract class RandomGenerator {

    /** Generates a random integer. */
    def nextInt(): Int

    /** Generates a random integer in the interval [0, limit). */
    def nextInt(limit: Int): Int

    /** Generates a random long integer. */
    def nextLong(): Long

    /** Generates a random long integer in the interval [0, limit). */
    def nextLong(limit: Long): Long

    /** Generates a random double in the interval [0, 1). */
    def nextProbability(): Double

    /** Generates a random decision. */
    inline final def nextDecision(): Boolean = nextInt(2) != 0

    /** Generates a random decision under consideration of the given probability for "yes". */
    final def nextDecision(p: Probability): Boolean =
         p.value == 1 || (p.value > 0 && nextInt(100) < p.value * 100)

    /** Creates a random generator of the same type seeded with nextInt. */
    def nextGen(): RandomGenerator

    /**
     * Retrieves all elements from the given collection, shuffles them,
     * and returns the resulting sequence.
     *
     * Implements the Fisher-Yates algorithm, see: http://en.wikipedia.org/wiki/Fisher–Yates_shuffle.
     */
    final def shuffle
        [T, C[T] <: Iterable[T]]
        (source: C[T])
        (using bf: BuildFrom[C[T], T, C[T]]): C[T] =
    {
        val buf = new mutable.ArrayBuffer[T]
        buf.addAll(source)
        for i <- buf.size - 1 to 1 by -1 do {
            val j = nextInt(i + 1)
            val tmp = buf(i)
            buf(i) = buf(j)
            buf(j) = tmp
        }
        bf.newBuilder(source).addAll(buf).result()
    }

    // Inspired by https://drmaciver.com/2018/01/lazy-fisher-yates-shuffling-for-precise-rejection-sampling/
    private final class FisherYatesRangeIterator(var n: Int) extends Iterator[Int] {

        require(n >= 0)

        // Sparse representation of the Fisher-Yates array:
        // If a key k is not present, then it is mapped to itself.
        private val map = new Int2IntOpenHashMap

        override def hasNext: Boolean = n > 0

        override def next(): Int = {
            if ! hasNext then {
                throw new NoSuchElementException
            }
            val i = nextInt(n)
            n -= 1
            val j = map.getOrDefault(i, i)
            if i < n then {
                val k = map.getOrDefault(n, n)
                map.put(i, k)
            }
            map.remove(n)
            j
        }

    }

    private final class FisherYatesInPlaceIterator[T](source: mutable.IndexedSeq[T]) extends Iterator[T] {

        private var n = source.size

        inline override def hasNext = n > 0

        override def next() = {
            if ! hasNext then {
                throw new NoSuchElementException
            }
            val i = nextInt(n)
            n -= 1
            val a = source(i)
            if i < n then {
                source(i) = source(n)
                source(n) = a
            }
            a
        }

    }

    /**
     * Shuffles [0, n[ lazily.
     *
     * In case not all elements are needed, lazyShuffle is more efficient than
     * shuffle because less random numbers are generated.
     */
    final def lazyShuffle(n: Int): Iterator[Int] = new FisherYatesRangeIterator(n)

    /**
     * Shuffles the given collection lazily.
     *
     * In case not all elements are needed, lazyShuffle is more efficient than
     * shuffle because less random numbers are generated.
     */
    final def lazyShuffle[T](source: IndexedSeq[T]): Iterator[T] =
        if source.isEmpty
        then Iterator.empty
        else if source.size == 1
        then source.iterator
        else lazyShuffle(source.size).map(source.apply)

    /**
     * Shuffles the given collection lazily in place.
     *
     * In case not all elements are needed, lazyShuffleInPlace is more efficient than
     * shuffle because less random numbers are generated.
     */
    final def lazyShuffleInPlace[T](source: mutable.IndexedSeq[T]): Iterator[T] =
        if source.isEmpty
        then Iterator.empty
        else if source.size == 1
        then source.iterator
        else new FisherYatesInPlaceIterator[T](source)

}
