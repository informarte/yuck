package yuck.flatzinc.test.vrp

import org.junit.platform.suite.api.{SelectClasses, Suite}

/**
 * Runs the CVRPTW benchmarks
 */
@Suite
@SelectClasses(
    Array(
        classOf[HombergerBenchmark],
        classOf[SolomonBenchmark]))
class CvrptwBenchmarks
