package yuck.core

import scala.language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

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

    /** Generates a random double in the interval [0, 1). */
    def nextProbability: Double

    /** Generates a random decision. */
    def nextDecision: Boolean = nextInt(2) != 0

    /** Creates a random generator of the same type seeded with nextInt. */
    def nextGen: RandomGenerator

    /**
     * Retrieves all elements from the given collection, shuffles them,
     * and returns the resulting sequence.
     *
     * Implements the Fisher-Yates algorithm, see: http://en.wikipedia.org/wiki/Fisher–Yates_shuffle.
     */
    final def shuffle
        [T, From[T] <: TraversableOnce[T], To[T] <: Seq[T]]
        (source: From[T])
        (implicit cbf: CanBuildFrom[_, T, To[T]]): To[T] =
    {
        val buf = new ArrayBuffer[T] ++= source
        for (i <- buf.size - 1 to 1 by -1) {
            val j = nextInt(i + 1)
            val tmp = buf(i)
            buf(i) = buf(j)
            buf(j) = tmp
        }
        (cbf() ++= buf).result
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
        else if (source.size == 1) source.toIterator
        else new LazyShuffleIterator[T](source)

}
