package yuck.util.testing

import org.junit.After
import org.scalamock.MockFactoryBase

/**
 * @author Michael Marte
 *
 */
final class MockingException(message: String) extends RuntimeException(message)

/**
 * Integrates JUnit with ScalaMock.
 *
 * Inspired by https://stackoverflow.com/questions/48060849/using-scalamock-outside-of-mockfactory.
 *
 * @author Michael Marte
 */
trait Mocking extends MockFactoryBase {

    override type ExpectationException = MockingException

    override protected def newExpectationException(message: String, methodName: Option[Symbol]) =
        new MockingException("%s, %s".format(message, methodName))

    @After
    protected def verifyAllScalaMockExpectations: Unit = {
        withExpectations(() => ())
    }

}
