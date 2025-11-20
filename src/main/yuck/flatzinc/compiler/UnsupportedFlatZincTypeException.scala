package yuck.flatzinc.compiler

class UnsupportedFlatZincTypeException(flatZincType: yuck.flatzinc.ast.Type)
extends RuntimeException("Type %s is not supported".format(flatZincType))
