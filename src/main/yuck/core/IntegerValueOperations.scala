package yuck.core

/**
 * Integrates IntegerValue with the Scala library.
 *
 * @author Michael Marte
 */
implicit object IntegerValueOperations extends Integral[IntegerValue] {
    override def compare(a: IntegerValue, b: IntegerValue) = a.compare(b)
    override def plus(a: IntegerValue, b: IntegerValue) = a + b
    override def minus(a: IntegerValue, b: IntegerValue) = a - b
    override def times(a: IntegerValue, b: IntegerValue) = a * b
    override def quot(a: IntegerValue, b: IntegerValue) = a / b
    override def rem(a: IntegerValue, b: IntegerValue) = a % b
    override def negate(a: IntegerValue) = a.negated
    override def fromInt(a: Int) = IntegerValue(a)
    override def parseString(str: String) = ???
    override def toInt(a: IntegerValue) = a.toInt
    override def toLong(a: IntegerValue) = a.toLong
    override def toFloat(a: IntegerValue) = a.toFloat
    override def toDouble(a: IntegerValue) = a.toDouble
    override def zero = Zero
    override def one = One
    override def abs(a: IntegerValue) = a.abs
}
