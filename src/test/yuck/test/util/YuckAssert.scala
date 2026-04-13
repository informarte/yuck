package yuck.test.util

import scala.annotation.tailrec

import org.junit.jupiter.api.Assertions

trait YuckAssert {

    protected inline final def assert(b: Boolean): Unit = {
        if ! b then {
            Assertions.fail()
        }
    }

    protected inline final def assert(message: => String, b: Boolean): Unit = {
        if ! b then {
            Assertions.fail(message)
        }
    }

    protected final def assertEq[T](a: T, b: T): Unit = {
        assert("%s (testee) != %s".format(a, b), a == b)
        if a != null && b != null then {
            assert("%s.hashcode (testee) != %s.hashcode".format(a, b), a.hashCode == b.hashCode)
        }
    }

    protected inline final def assertNe[T](a: T, b: T): Unit = {
        assert("%s (testee) == %s".format(a, b), a != b)
    }

    protected inline final def assertLe[T](a: T, b: T)(using ord: Ordering[T]): Unit = {
        assert("%s (testee) > %s".format(a, b), ord.compare(a, b) <= 0)
    }

    protected inline final def assertLt[T](a: T, b: T)(using ord: Ordering[T]): Unit = {
        assert("%s (testee) >= %s".format(a, b), ord.compare(a, b) < 0)
    }

    protected inline final def assertGe[T](a: T, b: T)(using ord: Ordering[T]): Unit = {
        assert("%s (testee) < %s".format(a, b), ord.compare(a, b) >= 0)
    }

    protected inline final def assertGt[T](a: T, b: T)(using ord: Ordering[T]): Unit = {
        assert("%s (testee) <= %s".format(a, b), ord.compare(a, b) > 0)
    }

    /** Expects an IllegalArgumentException. */
    protected inline final def assertThrows(operation: => Unit): Unit = {
        assertThrows(operation, classOf[IllegalArgumentException])
    }

    protected final def assertThrows(operation: => Unit, expectedExceptionType: Class[? <: Throwable]): Unit = {
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
        if ! failed then {
            Assertions.fail("Expected %s".format(expectedExceptionType))
        }
    }

    @tailrec
    private def findExceptionType(throwable: Throwable, expectedExceptionType: Class[? <: Throwable]): Boolean =
        findExceptionType(throwable.getClass, expectedExceptionType) ||
            (throwable.getCause != null && findExceptionType(throwable.getCause, expectedExceptionType))

    @tailrec
    private def findExceptionType(throwableType: Class[?], expectedExceptionType: Class[? <: Throwable]): Boolean =
        throwableType == expectedExceptionType ||
            (throwableType.getSuperclass != null &&
                findExceptionType(throwableType.getSuperclass, expectedExceptionType))

}
