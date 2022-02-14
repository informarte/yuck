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

    private val declaredVars = cc.declaredVars
    private val searchVars = cc.searchVars
    private val definedVars = cc.definedVars
    private val domains = cc.domains

    override def run() = {
        classifyVars()
    }

    private def classifyVars(): Unit = {
        for (constraint <- cc.ast.constraints) {
            for (annotation <- constraint.annotations) {
                annotation match {
                    case Annotation(Term("defines_var", List(a))) => definedVars += compileAnyExpr(a)
                    case Annotation(Term("defines_vars", List(a))) => definedVars ++= compileAnyArray(a)
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
            case Minimize(a, _) => searchVars -= compileAnyExpr(a)
            case Maximize(a, _) => searchVars -= compileAnyExpr(a)
        }
        if (searchVars.isEmpty) {
            // In case there is no search annotation, we initially consider all variables with
            // finite domain as subject to search except for variables that occur in a "defines_var"
            // annotation.
            searchVars ++= declaredVars.iterator.filter(domains(_).isFinite).map(compileAnyExpr)
            searchVars --= definedVars
        } else {
          // Sometimes variables, which occur in a "defines_var" annotation, are also marked as search variables.
          searchVars --= definedVars
        }
        // Sometimes a variable is tagged with "is_defined_var" but there is no corresponding "defines_var"
        // annotation.
        for (varDecl <- cc.ast.varDecls) {
            for (annotation <- varDecl.annotations) {
                annotation match {
                    case Annotation(Term("is_defined_var", Nil)) =>
                        if (varDecl.varType.isArrayType) {
                            searchVars --= compileAnyArray(Term(varDecl.id, Nil))
                        } else {
                            searchVars -= compileAnyExpr(Term(varDecl.id, Nil))
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
                for (elem <- elems if declaredVars.contains(elem)) {
                    searchVars += compileAnyExpr(elem)
                }
            case Term(search, Term(id, Nil) :: _)
            if List("bool_search", "int_search", "set_search").contains(search) =>
                if (cc.ast.varDeclsByName.contains(id)) {
                    cc.ast.varDeclsByName(id).varType match {
                        case ArrayType(Some(IntRange(1, n)), _) =>
                            for (idx <- 1 to n) {
                                searchVars += compileAnyExpr(ArrayAccess(id, IntConst(idx)))
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
