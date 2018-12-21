package yuck.core

/**
 * @author Michael Marte
 *
 * Randomly assigns values to variables without assignment.
 */
final class RandomInitializer(space: Space, randomGenerator: RandomGenerator) extends Runnable {

    override def run {

        // Problem parameters are not search variables, so they need special attention.
        for (x <- space.problemParameters) {
            x.assignRandomValue(space, randomGenerator)
        }

        // To protect us from the implementation details of hash set operations, we need to sort
        // the search variables before initializing them.
        val xs = (space.searchVariables -- space.searchState.mappedVariables).toList.sorted
        for (x <- xs) {
            x.assignRandomValue(space, randomGenerator)
        }

    }

}
