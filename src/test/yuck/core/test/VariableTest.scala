package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class VariableTest extends UnitTest {

    @Test
    def testVariableEquality {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        assertEq(s, s)
        assertEq(t, t)
        assertNe(s, t)
    }

    @Test
    def testVariableCasting {
        val space = new Space(logger)
        val b = space.createVariable("b", UnboundedBooleanDomain)
        val i = space.createVariable("i", UnboundedIntegerDomain)
        BooleanValueTraits.staticDowncast(b)
        BooleanValueTraits.staticDowncast(i)
        BooleanValueTraits.dynamicDowncast(b)
        BooleanValueTraits.staticDowncast[List](List(b, i))
        assertEx(BooleanValueTraits.dynamicDowncast(i))
        assertEx(IntegerValueTraits.dynamicDowncast(b))
        BooleanValueTraits.dynamicDowncast[List](List(b))
        assertEx(BooleanValueTraits.dynamicDowncast[List](List(b, i)))
        IntegerValueTraits.dynamicDowncast(i)
        val foo = space.createVariable("foo", new IntegerPowersetDomain(UnboundedIntegerDomain))
        val bar = space.createVariable("bar", new SingletonIntegerSetDomain(UnboundedIntegerDomain))
        IntegerSetValueTraits.dynamicDowncast(foo)
        IntegerSetValueTraits.dynamicDowncast(bar)
        IntegerSetValueTraits.dynamicDowncast[List](List(foo, bar))
        assertEx(IntegerSetValueTraits.dynamicDowncast[List](List(b, i)))
    }

}
