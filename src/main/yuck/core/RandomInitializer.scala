package yuck.core

/**
 * @author Michael Marte
 *
 * Randomly assigns values to variables without assignment.
 */
final class RandomInitializer(space: Space, randomGenerator: RandomGenerator) extends Runnable {

    override def run = {

        // Problem parameters are not search variables, so they need special attention.
        for (x <- space.problemParameters) {
            x.randomMoveEffect(randomGenerator).affect(space)
        }

        // To protect us from the implementation details of hash set operations, we need to sort
        // the search variables before initializing them.
        val xs = (space.searchVariables.diff(space.searchState.mappedVariables)).toBuffer.sorted
        for (x <- xs) {
            x.randomMoveEffect(randomGenerator).affect(space)
        }

    }

}
