package yuck.util.testing

import org.junit.Assert

/**
 * @author Michael Marte
 *
 */
trait YuckAssert {

    protected def assert(b: Boolean) {
        Assert.assertTrue(b)
    }

    protected def assertEq[T](a: T, b: T) {
        Assert.assertTrue("%s (testee) != %s".format(a, b), a == b)
        if (a != null && b != null) {
            Assert.assertTrue("%s.hashcode (testee) != %s.hashcode".format(a, b), a.hashCode == b.hashCode)
        }
    }

    protected def assertNe[T](a: T, b: T) {
        Assert.assertTrue("%s (testee) == %s".format(a, b), a != b)
    }

    protected def assertLe[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) > %s".format(a, b), ord.compare(a, b) <= 0)
    }

    protected def assertLt[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) >= %s".format(a, b), ord.compare(a, b) < 0)
    }

    protected def assertGe[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) < %s".format(a, b), ord.compare(a, b) >= 0)
    }

    protected def assertGt[T](a: T, b: T)(implicit ord: Ordering[T]) {
        Assert.assertTrue("%s (testee) <= %s".format(a, b), ord.compare(a, b) > 0)
    }

    /** Expects an IllegalArgumentException. */
    protected def assertEx(operation: => Unit) {
        assertEx(operation, classOf[IllegalArgumentException])
    }

    protected def assertEx(operation: => Unit, expectedExceptionType: Class[_ <: Throwable]) {
        var failed = true
        try {
            operation
            failed = false
        }
        catch {
            case throwable: Throwable =>
                Assert.assertTrue(
                    "Expected %s but got %s".format(expectedExceptionType, throwable.getClass),
                    findExceptionType(throwable, expectedExceptionType))
        }
        if (! failed) {
            Assert.fail("Expected %s".format(expectedExceptionType))
        }
    }

    private def findExceptionType(throwable: Throwable, expectedExceptionType: Class[_ <: Throwable]): Boolean =
        findExceptionType(throwable.getClass, expectedExceptionType) ||
            (throwable.getCause != null && findExceptionType(throwable.getCause, expectedExceptionType))

    private def findExceptionType(throwableType: Class[_], expectedExceptionType: Class[_ <: Throwable]): Boolean =
        throwableType == expectedExceptionType ||
            (throwableType.getSuperclass != null &&
                findExceptionType(throwableType.getSuperclass, expectedExceptionType))

}
