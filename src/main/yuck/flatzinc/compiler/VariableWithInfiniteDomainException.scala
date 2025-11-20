package yuck.flatzinc.compiler

import yuck.core.AnyVariable

final class VariableWithInfiniteDomainException(x: AnyVariable)
extends RuntimeException("Domain of variable %s is infinite".format(x))
