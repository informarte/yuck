package yuck.flatzinc.test.vrp

import org.junit.platform.suite.api.{SelectClasses, Suite}

/**
 * Runs the VRP benchmarks
 */
@Suite
@SelectClasses(
    Array(
        classOf[CvrpBenchmarks],
        classOf[CvrptwBenchmarks],
        classOf[TsptwBenchmarks]))
class VrpBenchmarks
