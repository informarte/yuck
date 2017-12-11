package yuck.flatzinc.parser

/**
 * @author Michael Marte
 *
 */
class FlatZincParserException(val line: Int, val column: Int, msg: String)
extends RuntimeException("Line %d, column %d: %s".format(line, column, msg))
