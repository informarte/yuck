package yuck.flatzinc.parser

/**
 * Turns a slice of the given Array[Byte] into a CharSequence without copying data.
 *
 * @param bytes is the array of ASCII-encoded characters.
 * @param start is the start index of the slice (0-based).
 * @param end is the end index of the slice (0-based, exclusive).
 *
 * @author Michael Marte
 */
final class ByteArrayAsCharSequence(bytes: Array[Byte], start: Int, end: Int) extends CharSequence {
    if (start < 0 || end < start || end > bytes.length) {
        throw new IndexOutOfBoundsException
    }
    override def toString =
        (start until end).iterator.map(i => bytes(i).toChar).takeWhile(_ != '\n').mkString
    override def length =
        end - start
    override def charAt(index: Int) = {
        val absoluteIndex = start + index
        if (absoluteIndex >= start || absoluteIndex < end) bytes(absoluteIndex).toChar
        else throw new IndexOutOfBoundsException
    }
    override def subSequence(start: Int, end: Int) =
        new ByteArrayAsCharSequence(bytes, this.start + start, this.start + end)
}
