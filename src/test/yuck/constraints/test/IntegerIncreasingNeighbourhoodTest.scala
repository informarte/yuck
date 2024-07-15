package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.{IntegerIncreasing, IntegerIncreasingNeighbourhood}
import yuck.core.{*, given}

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
final class IntegerIncreasingNeighbourhoodTest
    (numberOfVariables: Int,
     domainGenerator: (Int, Int) => IntegerDomain,
     strict: Boolean,
     override protected val propagate: Boolean)
    extends SpecialNeighbourhoodTest
{

    private val xs =
        for (i <- 0 until numberOfVariables) yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), domainGenerator(numberOfVariables, i))

    override protected def createConstraint() =
        new IntegerIncreasing(space.nextConstraintId(), null, xs, strict, costs)

    override protected def checkSearchState(searchState: SearchState) = {
        assert(xs.forall(_.hasValidValue(searchState)))
        for (i <- 0 until xs.size - 1) {
            val a = searchState.value(xs(i))
            val b = searchState.value(xs(i + 1))
            if (strict) {
                assertLt(a, b)
            } else {
                assertLe(a, b)
            }
        }
        assertEq(searchState.value(costs), True)
    }

    override protected val expectedNeighbourhoodClass = classOf[IntegerIncreasingNeighbourhood]

}

/**
 * @author Michael Marte
 *
 */
object IntegerIncreasingNeighbourhoodTest {

    private val holeGenerators = List(
        new Function2[Int, Int, IntegerDomain] {
            override def toString = "NoHoles"
            override def apply(numberOfVariables: Int, i: Int) =
                IntegerRange(1, 2 * numberOfVariables)
        },
        new Function2[Int, Int, IntegerDomain] {
            override def toString = "HoleAtI"
            override def apply(numberOfVariables: Int, i: Int) =
                IntegerRange(1, 2 * numberOfVariables).diff(IntegerDomain(i))
        }
    )

    private def configurations =
        for (numberOfVariables <- List(100);
             holeGenerator <- holeGenerators;
             strict <- List(false, true);
             propagate <- List(false, true))
        yield Vector(numberOfVariables, holeGenerator, strict, propagate)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}, {2}, {3}")
    def parameters = configurations.map(_.toArray).asJava

}
