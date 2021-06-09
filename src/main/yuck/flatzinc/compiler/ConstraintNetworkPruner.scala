package yuck.flatzinc.compiler

import yuck.core.RandomGenerator
import yuck.flatzinc.ast.{Annotation, Term}

/**
 * Removes useless constraints from the constraint network.
 *
 * The implementation assumes that objective variables have already been registered.
 *
 * @author Michael Marte
 */
final class ConstraintNetworkPruner
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    private def registerOutputVariables(): Unit = {
        for (decl <- cc.ast.varDecls) {
            for (annotation <- decl.annotations) {
                annotation match {
                    case Annotation(Term("output_var", _)) =>
                        cc.space.registerOutputVariable(compileAnyExpr(Term(decl.id, Nil)))
                    case Annotation(Term("output_array", _)) =>
                        for (x <- compileAnyArray(Term(decl.id, Nil))) {
                            cc.space.registerOutputVariable(x)
                        }
                    case _ =>
                }
            }
        }
    }

    override def run() = {
        registerOutputVariables()
        cc.space.removeUselessConstraints()
    }

}
