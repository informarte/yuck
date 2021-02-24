package yuck.util.testing

import org.junit.runners.Suite
import org.junit.runners.model.RunnerBuilder

/**
 * @author Michael Marte
 *
 */
class ParallelTestSuiteRunner(val klass: Class[_], val builder: RunnerBuilder) extends Suite(klass, builder) {
    setScheduler(new ParallelTestScheduler)
}
