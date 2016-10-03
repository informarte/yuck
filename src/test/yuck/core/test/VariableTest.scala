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
class VariableTest extends UnitTest {

    @Test
    def testVariableEquality {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        assertEq(s, s)
        assertEq(t, t)
        assertNe(s, t)
    }

    @Test
    def testVariableCasting {
        val space = new Space
        val b = space.createVariable("b", UnboundedBooleanDomain)
        val i = space.createVariable("i", UnboundedIntegerDomain)
        BooleanValue.Traits.staticCast(b)
        BooleanValue.Traits.staticCast(i)
        BooleanValue.Traits.dynamicCast(b)
        BooleanValue.Traits.staticCast[List](List(b, i))
        assertEx(BooleanValue.Traits.dynamicCast(i))
        assertEx(IntegerValue.Traits.dynamicCast(b))
        BooleanValue.Traits.dynamicCast[List](List(b))
        assertEx(BooleanValue.Traits.dynamicCast[List](List(b, i)))
        IntegerValue.Traits.dynamicCast(i)
        val foo = space.createVariable("foo", new IntegerPowersetDomain(UnboundedIntegerDomain))
        val bar = space.createVariable("bar", new SingletonIntegerSetDomain(UnboundedIntegerDomain))
        IntegerSetValue.Traits.dynamicCast(foo)
        IntegerSetValue.Traits.dynamicCast(bar)
        IntegerSetValue.Traits.dynamicCast[List](List(foo, bar))
        assertEx(IntegerSetValue.Traits.dynamicCast[List](List(b, i)))
    }

}
