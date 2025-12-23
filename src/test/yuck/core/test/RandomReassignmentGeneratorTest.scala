package yuck.core.test

import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.core.*

@ParameterizedClass
@MethodSource(Array("parameters"))
@Execution(ExecutionMode.CONCURRENT)
final class RandomReassignmentGeneratorTest
    (override protected val moveSizeDistribution: Distribution,
     override protected val maybeHotSpotDistribution: Option[Distribution],
     override protected val maybeFairChoiceRate: Option[Probability],
     numberOfVariables: Int)
    extends GeneralNeighbourhoodTest[IntegerValue, IntegerDomain, IntegerVariable]
{

    override protected val typeTraits = IntegerTypeTraits

    override protected val xs =
        for i <- 0 until numberOfVariables yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(0, numberOfVariables - 1))

    override protected lazy val neighbourhood =
        new RandomReassignmentGenerator(
            space, xs, randomGenerator, moveSizeDistribution, maybeHotSpotDistribution, maybeFairChoiceRate)

    override protected val acceptableMoveSizeFrequencyDeviation = new AcceptableDeviation(0.1, 0)
    override protected val acceptableVariableFrequencyDeviation = new AcceptableDeviation(0.1, 1)

}

object RandomReassignmentGeneratorTest extends GeneralNeighbourhoodTestParameterFactory {

    override protected val moveSizeDistributions =
        List(List(100), List(90, 10), List(50, 35, 15), List(50, 25, 15, 10)).map(Distribution(1, _))

}
