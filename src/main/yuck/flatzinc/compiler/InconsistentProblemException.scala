package yuck.flatzinc.compiler

/**
 * @author Michael Marte
 *
 */
abstract class InconsistentProblemException(msg: String) extends RuntimeException(msg)

/**
 * @author Michael Marte
 *
 */
final class InconsistentConstraintException(constraint: yuck.flatzinc.ast.Constraint)
extends InconsistentProblemException("%s is inconsistent".format(constraint))

/**
 * @author Michael Marte
 *
 */
final class DomainWipeOutException(decl: yuck.flatzinc.ast.Expr)
extends InconsistentProblemException("Domain of %s was wiped out".format(decl))
