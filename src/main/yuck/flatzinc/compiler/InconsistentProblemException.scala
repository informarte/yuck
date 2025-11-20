package yuck.flatzinc.compiler

import yuck.core.InconsistentProblemException

final class InconsistentConstraintException(val constraint: yuck.flatzinc.ast.Constraint)
extends InconsistentProblemException("%s is inconsistent".format(constraint))

final class DomainWipeOutException(val decl: yuck.flatzinc.ast.Expr)
extends InconsistentProblemException("Domain of %s was wiped out".format(decl))
