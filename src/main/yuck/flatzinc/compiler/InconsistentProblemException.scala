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
final class InconsistentConstraintException(val constraint: yuck.flatzinc.ast.Constraint)
extends InconsistentProblemException("%s is inconsistent".format(constraint))

/**
 * @author Michael Marte
 *
 */
final class DomainWipeOutException1(val decl: yuck.flatzinc.ast.Expr)
extends InconsistentProblemException("Domain of %s was wiped out".format(decl))

/**
 * @author Michael Marte
 *
 */
final class DomainWipeOutException2(val x: yuck.core.AnyVariable)
extends InconsistentProblemException("Domain of %s was wiped out".format(x))
