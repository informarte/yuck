package yuck.test

import org.junit.platform.suite.api.{SelectClasses, Suite}

/**
 * A suite of rather simple integration test
 */
@Suite
@SelectClasses(
    Array(
        classOf[Queens],
        classOf[SendMoreMoney],
        classOf[SendMostMoney]))
class HelloWorldTestSuite
