package yuck.flatzinc.compiler

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.ast.*

/**
 * Identifies search variables and variables that occur in defines_var annotations.
 *
 * @author Michael Marte
 */
final class VariableClassifier
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    override def run() = {
        classifyVars()
    }

    private def classifyVars(): Unit = {
        for (constraint <- cc.ast.constraints) {
            for (annotation <- constraint.annotations) {
                annotation match {
                    case Annotation(Term("defines_var", Seq(a))) => cc.definedVars += compileAnyExpr(a)
                    case Annotation(Term("defines_vars", Seq(a))) => cc.definedVars ++= compileAnyArray(a)
                    case _ =>
                }
            }
        }
        for (Annotation(expr) <- cc.ast.solveGoal.annotations) {
            findSearchVars(expr)
        }
        // Sometimes the objective variable is declared as a search variable.
        // We ignore this definition to facilitate the computation of the objective value
        // by a function.
        cc.ast.solveGoal match {
            case Satisfy(_) =>
            case Minimize(a, _) => cc.searchVars -= compileAnyExpr(a)
            case Maximize(a, _) => cc.searchVars -= compileAnyExpr(a)
        }
        // Sometimes variables, which occur in a "defines_var" annotation, are also marked as search variables.
        cc.searchVars --= cc.definedVars
        // Sometimes a variable is tagged with "is_defined_var" but there is no corresponding "defines_var"
        // annotation.
        for (varDecl <- cc.ast.varDecls) {
            for (annotation <- varDecl.annotations) {
                annotation match {
                    case Annotation(Term("is_defined_var", Nil)) =>
                        if (varDecl.valueType.isArrayType) {
                            cc.searchVars --= compileAnyArray(Term(varDecl.id, Nil))
                        } else {
                            cc.searchVars -= compileAnyExpr(Term(varDecl.id, Nil))
                        }
                    case _ =>
                }
            }
        }
    }

    private def findSearchVars(annotation: Expr): Unit = {
        annotation match {
            case Term(search, ArrayConst(elems) :: _)
            if List("bool_search", "int_search", "set_search").contains(search) =>
                for (elem <- elems if cc.declaredVars.contains(elem)) {
                    cc.searchVars += compileAnyExpr(elem)
                }
            case Term(search, Term(id, Nil) :: _)
            if List("bool_search", "int_search", "set_search").contains(search) =>
                if (cc.ast.varDeclsByName.contains(id)) {
                    cc.ast.varDeclsByName(id).valueType match {
                        case ArrayType(Some(IntRange(1, n)), _) =>
                            for (idx <- 1 to n.toInt) {
                                cc.searchVars += compileAnyExpr(ArrayAccess(id, IntConst(idx)))
                            }
                    }
                } else {
                    assert(cc.ast.paramDeclsByName.contains(id))
                }
            case Term("seq_search", ArrayConst(searches) :: _) =>
                searches.foreach(findSearchVars)
            case _ =>
        }
    }

}
