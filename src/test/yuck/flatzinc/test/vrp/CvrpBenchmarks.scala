package yuck.flatzinc.test.vrp

import org.junit.platform.suite.api.{SelectClasses, Suite}

/**
 * Runs the CVRP benchmarks
 */
@Suite
@SelectClasses(
    Array(
        classOf[AugeratBenchmark],
        classOf[UchoaBenchmark]))
class CvrpBenchmarks
