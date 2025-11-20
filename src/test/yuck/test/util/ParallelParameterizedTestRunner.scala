package yuck.test.util

import org.junit.runners.Parameterized

class ParallelParameterizedTestRunner(val klass: Class[?]) extends Parameterized(klass) {
    setScheduler(new ParallelTestScheduler)
}
