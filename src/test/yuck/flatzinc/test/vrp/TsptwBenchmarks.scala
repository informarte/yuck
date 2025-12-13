package yuck.flatzinc.test.vrp

import org.junit.platform.suite.api.{SelectClasses, Suite}

/**
 * Runs the TSPTW benchmarks
 */
@Suite
@SelectClasses(
    Array(
        classOf[AscheuerBenchmark],
        classOf[DumasBenchmark],
        classOf[GendreauBenchmark]))
class TsptwBenchmarks
