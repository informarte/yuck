package yuck.test.util

import org.junit.runners.BlockJUnit4ClassRunner

class ParallelTestRunner(val klass: Class[?]) extends BlockJUnit4ClassRunner(klass) {
    setScheduler(new ParallelTestScheduler)
}
