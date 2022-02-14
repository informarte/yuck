package yuck.flatzinc.test.vrp

import org.junit.*
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

/**
 * Runs the CVRP benchmarks
 *
 * @author Michael Marte
 */
@Test
@RunWith(classOf[runners.Suite])
@SuiteClasses(
    Array(
        classOf[AugeratBenchmark],
        classOf[UchoaBenchmark]))
class CvrpBenchmarks
