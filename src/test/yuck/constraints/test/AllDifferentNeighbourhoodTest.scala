package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.{AllDifferent, AllDifferentNeighbourhood}
import yuck.core.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
final class AllDifferentNeighbourhoodTest
    (numberOfVariables: Int,
     withException: Boolean,
     domainGenerator: Int => IntegerDomain)
    extends SpecialNeighbourhoodTest
{

    private val xs =
        for (i <- 0 until numberOfVariables) yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), domainGenerator(i))

    private val exceptedValues = if withException then Set(Zero) else Set()

    override protected def createConstraint() =
        new AllDifferent(space.nextConstraintId(), null, xs, exceptedValues, costs, logger)

    override protected val expectedNeighbourhoodClass = classOf[AllDifferentNeighbourhood[?]]

    override protected def checkSearchState(searchState: SearchState) = {
        assert(xs.forall(_.hasValidValue(searchState)))
        if (withException) {
            assertEq(
                xs.view.map(searchState.value).filter(_ != Zero).toSet.size +
                    xs.view.map(searchState.value).count(_ == Zero),
                numberOfVariables)
        } else {
            assertEq(xs.view.map(searchState.value).toSet.size, xs.size)
        }
        assertEq(searchState.value(costs), True)
    }

    override protected val moveSizeDistribution = Distribution(1, List(70, 20, 10))

}

/**
 * @author Michael Marte
 *
 */
object AllDifferentNeighbourhoodTest {

    private def domainGenerators(numberOfVariables: Int, withException: Boolean) =
        if withException
        then List(
            new Function1[Int, IntegerDomain] {
                override def toString = "SmallDomainsWithoutHoles"
                override def apply(i: Int) = IntegerRange(0, numberOfVariables / 2)
            },
            new Function1[Int, IntegerDomain] {
                override def toString = "SmallDomainsWithHoles"
                override def apply(i: Int) = IntegerRange(0, numberOfVariables / 2).diff(IntegerDomain(i / 2))
            }
        )
        else List (
            new Function1[Int, IntegerDomain] {
                override def toString = "TightDomainsWithoutHoles"
                override def apply(i: Int) = IntegerRange(0, numberOfVariables - 1)
            },
            new Function1[Int, IntegerDomain] {
                override def toString = "TightDomainsWithHoles"
                override def apply(i: Int) = IntegerRange(0, numberOfVariables - 1).diff(IntegerDomain(i))
            },
            new Function1[Int, IntegerDomain] {
                override def toString = "BigDomainsWithoutHoles"
                override def apply(i: Int) = IntegerRange(0, numberOfVariables * 2)
            },
            new Function1[Int, IntegerDomain] {
                override def toString = "BigDomainsWithHoles"
                override def apply(i: Int) = IntegerRange(0, numberOfVariables * 2).diff(IntegerDomain(i * 2))
            }
        )

    private def configurations =
        for (numberOfVariables <- List(100);
             withException <- List(false, true);
             domainGenerator <- domainGenerators(numberOfVariables, withException))
        yield Vector(numberOfVariables, withException, domainGenerator)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}, {2}")
    def parameters = configurations.map(_.toArray).asJava

}
