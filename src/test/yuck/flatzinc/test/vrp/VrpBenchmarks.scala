package yuck.flatzinc.test.vrp

import org.junit._
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

/**
 * Runs the VRP benchmarks
 *
 * @author Michael Marte
 */
@Test
@RunWith(classOf[runners.Suite])
@SuiteClasses(
    Array(
        classOf[CvrpBenchmarks],
        classOf[CvrptwBenchmarks],
        classOf[TsptwBenchmarks]))
class VrpBenchmarks
