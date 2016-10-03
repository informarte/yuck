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
     * Retrieves all elements from the given source collection, shuffles them,
     * and returns the resulting sequence.
     *
     * Implements the Fisher-Yates algorithm, see: http://en.wikipedia.org/wiki/Fisher–Yates_shuffle.
     */
    def shuffle
        [T, From[X] <: TraversableOnce[X], To[X] <: Seq[X]]
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

}
