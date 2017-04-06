package yuck.flatzinc.compiler

/**
 * @author Michael Marte
 *
 */
class UnsupportedFlatZincTypeException(flatZincType: yuck.flatzinc.ast.Type)
extends RuntimeException("Type %s is not supported".format(flatZincType))
