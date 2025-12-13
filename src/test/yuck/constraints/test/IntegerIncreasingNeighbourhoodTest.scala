package yuck.constraints.test

import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.constraints.{IntegerIncreasing, IntegerIncreasingNeighbourhood}
import yuck.core.*

@ParameterizedClass
@MethodSource(Array("parameters"))
final class IntegerIncreasingNeighbourhoodTest
    (numberOfVariables: Int,
     domainGenerator: (Int, Int) => IntegerDomain,
     strict: Boolean,
     override protected val propagate: Boolean)
    extends SpecialNeighbourhoodTest
{

    private val xs =
        for i <- 0 until numberOfVariables yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), domainGenerator(numberOfVariables, i))

    override protected def createConstraint() =
        new IntegerIncreasing(space.nextConstraintId(), null, xs, strict, costs)

    override protected def checkSearchState(searchState: SearchState) = {
        assert(xs.forall(_.hasValidValue(searchState)))
        for i <- 0 until xs.size - 1 do {
            val a = searchState.value(xs(i))
            val b = searchState.value(xs(i + 1))
            if strict then {
                assertLt(a, b)
            } else {
                assertLe(a, b)
            }
        }
        assertEq(searchState.value(costs), True)
    }

    override protected val expectedNeighbourhoodClass = classOf[IntegerIncreasingNeighbourhood]

}

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
        for numberOfVariables <- List(100)
            holeGenerator <- holeGenerators
            strict <- List(false, true)
            propagate <- List(false, true)
        yield
            Array(numberOfVariables, holeGenerator, strict, propagate)

    def parameters = configurations.toArray

}
