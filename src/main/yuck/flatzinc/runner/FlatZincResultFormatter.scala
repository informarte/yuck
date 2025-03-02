package yuck.flatzinc.runner

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.ast.*

/**
 * @author Michael Marte
 *
 */
final class FlatZincResultFormatter(ast: FlatZincAst) extends (FlatZincResult => Seq[String]) {

    private val outputVarDecls =
        ast.varDecls.iterator
            .filter(_.annotations.iterator.map(_.term.id).exists(id => id == "output_var" || id == "output_array"))
            .toVector

    override def apply(result: FlatZincResult) = {
        val compilerResult = result.compilerResult
        require(compilerResult.ast.eq(ast))
        val searchState = result.searchState
        val sortedMap = new mutable.TreeMap[String, String]() // id -> value
        for (decl <- outputVarDecls) {
            for (annotation <- decl.annotations) {
                annotation match {
                    case Annotation(Term("output_var", Nil)) =>
                        val x = compilerResult.vars(decl.id)
                        sortedMap += (decl.id -> value(searchState, x).toString)
                    case Annotation(Term("output_array", List(ArrayConst(dimensions)))) =>
                        val ArrayType(Some(IntRange(1, n)), _) = decl.valueType: @unchecked
                        val a =
                            "array%dd(%s, [%s])".format(
                                dimensions.size,
                                (for (case IntSetConst(IntRange(lb, ub)) <- dimensions.iterator) yield
                                    "%d..%d".format(lb, ub)).mkString(", "),
                                (for (idx <- (1 to n.toInt).iterator) yield
                                    value(searchState, compilerResult.arrays(decl.id)(idx - 1)).toString)
                                    .mkString(", "))
                        sortedMap += (decl.id -> a)
                    case _ =>
                }
            }
        }
        val lines = mutable.ArrayBuffer[String]()
        for ((id, value) <- sortedMap) lines += "%s = %s;".format(id, value)
        val objective = compilerResult.objective
        val costs = objective.costs(result.searchState)
        if (objective.isSolution(costs)) {
            lines += FlatZincSolutionSeparator
            if (objective.isInstanceOf[HierarchicalObjective] && objective.isOptimal(costs)) {
                lines += FlatZincBestSolutionFoundIndicator
            }
        } else {
            lines += FlatZincNoSolutionFoundIndicator
        }
        lines.toSeq
    }

    private def value(searchState: SearchState, x: AnyVariable): AnyValue = {
        val a = searchState.value(x)
        a match {
            case b: BooleanValue => if (b.truthValue) True else False
            case _ => a
        }
    }

}
