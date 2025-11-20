package yuck.test

import org.junit.*

/**
 * A suite of rather simple integration test
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[Queens],
        classOf[SendMoreMoney],
        classOf[SendMostMoney]))
class HelloWorldTestSuite
