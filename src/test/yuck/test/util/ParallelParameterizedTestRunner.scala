package yuck.test.util

import org.junit.runners.Parameterized

/**
 * @author Michael Marte
 *
 */
class ParallelParameterizedTestRunner(val klass: Class[_]) extends Parameterized(klass) {
    setScheduler(new ParallelTestScheduler)
}
