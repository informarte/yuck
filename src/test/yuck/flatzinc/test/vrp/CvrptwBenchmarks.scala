package yuck.flatzinc.test.vrp

import org.junit.*
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

/**
 * Runs the CVRPTW benchmarks
 */
@RunWith(classOf[runners.Suite])
@SuiteClasses(
    Array(
        classOf[HombergerBenchmark],
        classOf[SolomonBenchmark]))
class CvrptwBenchmarks
