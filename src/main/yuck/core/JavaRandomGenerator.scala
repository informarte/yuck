package yuck.core

/**
 * Creates a [[https://docs.oracle.com/javase/8/docs/api/java/util/SplittableRandom.html Java random generator]]
 * and makes it available for optimization.
 *
 * @author Michael Marte
 */
final class JavaRandomGenerator(seed: Long = DefaultSeed) extends RandomGenerator {
    private val wrappee = new java.util.SplittableRandom(seed)
    // warm-up
    for (i <- 1 until 8192) nextInt()
    override def nextInt() = wrappee.nextInt()
    override def nextInt(limit: Int) = wrappee.nextInt(limit)
    override def nextLong() = wrappee.nextLong()
    override def nextLong(limit: Long) = wrappee.nextLong(limit)
    override def nextProbability() = wrappee.nextDouble()
    override def nextGen() = new JavaRandomGenerator(nextLong())
}
