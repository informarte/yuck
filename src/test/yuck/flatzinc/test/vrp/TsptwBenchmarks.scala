package yuck.flatzinc.test.vrp

import org.junit._
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

/**
 * Runs the TSPTW benchmarks
 *
 * @author Michael Marte
 */
@Test
@RunWith(classOf[runners.Suite])
@SuiteClasses(
    Array(
        classOf[AscheuerBenchmark],
        classOf[DumasBenchmark],
        classOf[GendreauBenchmark]))
class TsptwBenchmarks
