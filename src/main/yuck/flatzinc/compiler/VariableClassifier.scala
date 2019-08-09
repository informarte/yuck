package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.flatzinc.ast._

/**
 * Identifies search variables and variables that occur in defines_var annotations.
 *
 * @author Michael Marte
 */
class VariableClassifier
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    private val declaredVars = cc.declaredVars
    private val searchVars = cc.searchVars
    private val definedVars = cc.definedVars
    private val domains = cc.domains

    override def run: Unit = {
        classifyVars
    }

    private def classifyVars: Unit = {
        for (constraint <- cc.ast.constraints) {
            for (Annotation(Term("defines_var", List(a))) <- constraint.annotations) {
                definedVars += compileAnyExpr(a)
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
            // finite domain as subject to search except for variables that occur in a defines_var
            // annotation.
            searchVars ++= declaredVars.iterator.filter(domains(_).isFinite).map(compileAnyExpr)
            searchVars --= definedVars
        } else {
          // Sometimes variables with a defines_var annotation are also marked as search variables.
          searchVars --= definedVars
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
                cc.ast.varDeclsByName(id).varType match {
                    case ArrayType(Some(IntRange(1, n)), _) =>
                        for (idx <- 1 to n) {
                            searchVars += compileAnyExpr(ArrayAccess(id, IntConst(idx)))
                        }
                }
            case Term("seq_search", ArrayConst(searches) :: _) =>
                searches.foreach(findSearchVars)
            case _ =>
        }
    }

}
