package yuck.core

/**
 * @author Michael Marte
 *
 * Randomly assigns values to variables without assignment.
 */
final class RandomInitializer(space: Space, randomGenerator: RandomGenerator) extends Runnable {

    override def run {
        // To protect us from the implementation details of hash set operations, we need to sort
        // the variables before initializing them.
        val xs = (space.searchVariables -- space.searchState.mappedVariables).toList.sorted
        for (x <- xs) {
            x.assignRandomValue(space, randomGenerator)
        }
    }

}
