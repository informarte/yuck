package yuck.core

/**
 * Creates a [[http://docs.oracle.com/javase/7/docs/api/java/util/Random.html Java random generator]]
 * and makes it available for optimization.
 *
 * @author Michael Marte
 */
final class JavaRandomGenerator(seed: Int = 5489) extends RandomGenerator {
    private val wrappee = new java.util.Random(seed)
    // warm-up
    for (i <- 1 until 8192) nextInt
    override def nextInt = wrappee.nextInt
    override def nextInt(limit: Int) = wrappee.nextInt(limit)
    override def nextProbability = wrappee.nextDouble
    override def nextGen = new JavaRandomGenerator(nextInt)
}
