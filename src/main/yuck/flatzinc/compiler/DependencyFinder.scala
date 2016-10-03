package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.flatzinc.ast._

/**
 * mzn2fzn adds defines_var annotations to constraints, but not in all places
 * where a functional dependency could be exploited, so this phase identifies these
 * places and adds appropriate annotations.
 *
 * @author Michael Marte
 */
class DependencyFinder
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    private val logger = cc.logger
    private val domains = cc.domains
    private val equalVars = cc.equalVars
    private val declaredVars = cc.declaredVars
    private val searchVars = new mutable.HashSet[Expr]
    private val channelVars = new mutable.HashSet[Expr]
    private val unclassifiedVars = new mutable.HashSet[Expr]

    override def run {
        classifyVars
        cc.ast = cc.ast.copy(constraints = cc.ast.constraints.map(addDefinesVarAnnotation))
    }

    private def classifyVars {
        for (constraint <- cc.ast.constraints) {
            for (Annotation(Term("defines_var", List(a))) <- constraint.annotations) {
                val e = equalVars(a)
                channelVars ++= e
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
            case Minimize(a, _) => searchVars -= a
            case Maximize(a, _) => searchVars -= a
        }
        if (searchVars.isEmpty) {
            // In case there is no search annotation, we initially consider all variables with
            // finite domain as subject to search except for variables that occur in a defines_var
            // annotation.
            searchVars ++= declaredVars.toIterator.filter(domains(_).isFinite)
            searchVars --= channelVars
        }
        searchVars --= channelVars
        unclassifiedVars ++= declaredVars
        unclassifiedVars --= searchVars
        unclassifiedVars --= channelVars
    }

    private def findSearchVars(annotation: Expr) {
        annotation match {
            case Term(search, ArrayConst(elems) :: _)
            if List("bool_search", "int_search", "set_search").contains(search) =>
                for (elem <- elems if declaredVars.contains(elem)) {
                    searchVars ++= equalVars(elem)
                }
            case Term(search, Term(id, Nil) :: _)
            if List("bool_search", "int_search", "set_search").contains(search) =>
                cc.ast.varDeclsByName(id).varType match {
                    case ArrayType(Some(IntRange(1, n)), _) =>
                        for (idx <- 1 to n) {
                            searchVars ++= equalVars(ArrayAccess(id, IntConst(idx)))
                        }
                }
            case Term("seq_search", ArrayConst(searches) :: _) =>
                searches.foreach(findSearchVars)
        }
    }

    private val blacklist =
        List("bool_eq", "bool_ne", "bool_le", "bool_lt", "array_bool_xor",
             "int_eq", "int_ne", "int_le", "int_lt",
             "int_lin_ne", "int_lin_le",
             "set_in", "set_subset",
             "cumulative")

    // According to section 5.6.3 of the FlatZinc spec, defines_var annotations are only
    // an "early warning" to the solver that there is a functional dependency that could
    // be exploited.
    // So we add as many annotations as possible and hope that the rest of the compiler can
    // cope with them.
    private def addDefinesVarAnnotation(constraint: yuck.flatzinc.ast.Constraint): yuck.flatzinc.ast.Constraint = {
        constraint match {
            case Constraint(_, _, annotations) if annotations.exists(_.term.id == "defines_var") =>
                constraint
            case Constraint(name, _, _) if blacklist.contains(name) =>
                constraint
            case Constraint("array_int_minimum" | "array_int_maximum", args, annotations)
            if isPotentialChannel(args.head) =>
                addDefinesVarAnnotation(constraint, args.head)
            case Constraint("int_lin_eq", List(as0, bs0, c), annotations) =>
                val as = getArrayElems(as0).toList
                val bs = getArrayElems(bs0).toList
                val candidates =
                    as.toIterator.zip(bs.toIterator).filter{
                        case (IntConst(1) | IntConst(-1), b) => isPotentialChannel(b)
                        case _ => false
                    }
                if (candidates.hasNext) {
                    val (_, b) = candidates.next
                    addDefinesVarAnnotation(constraint, b)
                } else {
                    constraint
                }
            case Constraint("nvalue", n :: _, _) if isPotentialChannel(n) =>
                addDefinesVarAnnotation(constraint, n)
            case Constraint("yuck_bin_packing", List(loads, _, _), _) =>
                getArrayElems(loads).filter(isPotentialChannel).foldLeft(constraint)(addDefinesVarAnnotation)
            case Constraint(name, args, annotations) if args.size > 1 && isPotentialChannel(args.last) =>
               addDefinesVarAnnotation(constraint, args.last)
            case _ =>
                constraint
        }
    }

    private def isPotentialChannel(a: Expr): Boolean =
        ! a.isConst && unclassifiedVars.contains(a) && ! domains(a).isSingleton

    private def addDefinesVarAnnotation(
        constraint: yuck.flatzinc.ast.Constraint, a: Expr): yuck.flatzinc.ast.Constraint =
    {
        logger.logg("Annotating %s with defines_var(%s)".format(constraint, a))
        val result = constraint.copy(annotations = Annotation(Term("defines_var", List(a))) :: constraint.annotations)
        val e = equalVars(a)
        channelVars ++= e
        unclassifiedVars --= e
        assert(definesVar(result, a))
        result
    }

}
