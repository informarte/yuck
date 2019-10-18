package yuck.core

import scala.collection._

/**
 * Provides an interface for random generation of decisions, integers, and probabilities.
 *
 * @author Michael Marte
 */
abstract class RandomGenerator {

    /** Generates a random integer. */
    def nextInt: Int

    /** Generates a random integer in the interval [0, limit). */
    def nextInt(limit: Int): Int

    /** Generates a random long integer. */
    def nextLong: Long

    /** Generates a random long integer in the interval [0, limit). */
    def nextLong(limit: Long): Long

    /** Generates a random double in the interval [0, 1). */
    def nextProbability: Double

    /** Generates a random decision. */
    final def nextDecision: Boolean = nextInt(2) != 0

    /** Generates a random decision under consideration of the given probability for "yes". */
    final def nextDecision(p: Probability): Boolean =
         p.value == 1 || (p.value > 0 && nextInt(100) < p.value * 100)

    /** Creates a random generator of the same type seeded with nextInt. */
    def nextGen: RandomGenerator

    /**
     * Retrieves all elements from the given collection, shuffles them,
     * and returns the resulting sequence.
     *
     * Implements the Fisher-Yates algorithm, see: http://en.wikipedia.org/wiki/Fisher–Yates_shuffle.
     */
    final def shuffle
        [T, C[T] <: Iterable[T]]
        (source: C[T])
        (implicit bf: BuildFrom[C[T], T, C[T]]): C[T] =
    {
        val buf = new mutable.ArrayBuffer[T]
        buf.addAll(source)
        for (i <- buf.size - 1 to 1 by -1) {
            val j = nextInt(i + 1)
            val tmp = buf(i)
            buf(i) = buf(j)
            buf(j) = tmp
        }
        bf.newBuilder(source).addAll(buf).result
    }

    private final class LazyShuffleIterator[T](source: IndexedSeq[T]) extends Iterator[T] {
        private var n = source.size
        private val buf = Array.tabulate(n)(identity)
        @inline override def hasNext = n > 0
        override def next = {
            require(hasNext)
            val i = nextInt(n)
            n -= 1
            val a = source(buf(i))
            if (i < n) buf.update(i, buf(n))
            a
        }
    }


    /**
     * Shuffles the given collection lazily.
     *
     * Time and space complexity for creating the iterator are O(n).
     * In case not all elements are needed, lazyShuffle is more efficient than
     * shuffle because less random numbers are generated.
     */
    @inline final def lazyShuffle[T](source: IndexedSeq[T]): Iterator[T] =
        if (source.isEmpty) Iterator.empty
        else if (source.size == 1) source.iterator
        else new LazyShuffleIterator[T](source)

}
