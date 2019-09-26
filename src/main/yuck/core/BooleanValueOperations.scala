package yuck.core

/**
 * Integrates BooleanValue with the Scala library.
 *
 * @author Michael Marte
 */
final object BooleanValueOperations extends Numeric[BooleanValue] {
    override def compare(a: BooleanValue, b: BooleanValue) = a.compare(b)
    override def plus(a: BooleanValue, b: BooleanValue) = a + b
    override def minus(a: BooleanValue, b: BooleanValue) = a - b
    override def times(a: BooleanValue, b: BooleanValue) = a * b
    override def negate(a: BooleanValue) = a.negate
    override def fromInt(a: Int) = BooleanValue.get(a)
    override def parseString(str: String) = ???
    override def toInt(a: BooleanValue) = a.toInt
    override def toLong(a: BooleanValue) = a.toLong
    override def toFloat(a: BooleanValue) = a.toFloat
    override def toDouble(a: BooleanValue) = a.toDouble
    override def zero = True
    override def one = False
    override def abs(a: BooleanValue) = a.abs
}
