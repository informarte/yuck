package yuck.test.util

import org.junit.Assert

import scala.annotation.tailrec

/**
 * @author Michael Marte
 *
 */
trait YuckAssert {

    protected def assert(b: Boolean): Unit = {
        if (! b) {
            Assert.fail()
        }
    }

    protected def assert(message: => String, b: Boolean): Unit = {
        if (! b) {
            Assert.fail(message)
        }
    }

    protected def assertEq[T](a: T, b: T): Unit = {
        assert("%s (testee) != %s".format(a, b), a == b)
        if (a != null && b != null) {
            assert("%s.hashcode (testee) != %s.hashcode".format(a, b), a.hashCode == b.hashCode)
        }
    }

    protected def assertNe[T](a: T, b: T): Unit = {
        assert("%s (testee) == %s".format(a, b), a != b)
    }

    protected def assertLe[T](a: T, b: T)(implicit ord: Ordering[T]): Unit = {
        assert("%s (testee) > %s".format(a, b), ord.compare(a, b) <= 0)
    }

    protected def assertLt[T](a: T, b: T)(implicit ord: Ordering[T]): Unit = {
        assert("%s (testee) >= %s".format(a, b), ord.compare(a, b) < 0)
    }

    protected def assertGe[T](a: T, b: T)(implicit ord: Ordering[T]): Unit = {
        assert("%s (testee) < %s".format(a, b), ord.compare(a, b) >= 0)
    }

    protected def assertGt[T](a: T, b: T)(implicit ord: Ordering[T]): Unit = {
        assert("%s (testee) <= %s".format(a, b), ord.compare(a, b) > 0)
    }

    /** Expects an IllegalArgumentException. */
    protected def assertEx(operation: => Unit): Unit = {
        assertEx(operation, classOf[IllegalArgumentException])
    }

    /** Expects a NotImplementedError. */
    protected def assertNie(operation: => Unit): Unit = {
        assertEx(operation, classOf[NotImplementedError])
    }

    protected def assertEx(operation: => Unit, expectedExceptionType: Class[_ <: Throwable]): Unit = {
        var failed = true
        try {
            operation
            failed = false
        }
        catch {
            case throwable: Throwable =>
                assert(
                    "Expected %s but got %s".format(expectedExceptionType, throwable.getClass),
                    findExceptionType(throwable, expectedExceptionType))
        }
        if (! failed) {
            Assert.fail("Expected %s".format(expectedExceptionType))
        }
    }

    @tailrec
    private def findExceptionType(throwable: Throwable, expectedExceptionType: Class[_ <: Throwable]): Boolean =
        findExceptionType(throwable.getClass, expectedExceptionType) ||
            (throwable.getCause != null && findExceptionType(throwable.getCause, expectedExceptionType))

    @tailrec
    private def findExceptionType(throwableType: Class[_], expectedExceptionType: Class[_ <: Throwable]): Boolean =
        throwableType == expectedExceptionType ||
            (throwableType.getSuperclass != null &&
                findExceptionType(throwableType.getSuperclass, expectedExceptionType))

}
