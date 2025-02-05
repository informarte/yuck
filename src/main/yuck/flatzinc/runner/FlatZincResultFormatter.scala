package yuck.flatzinc.runner

import java.util.concurrent.Callable

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.ast.*
import yuck.flatzinc.compiler.FlatZincCompilerResult

/**
 * @author Michael Marte
 *
 */
final class FlatZincResultFormatter(ast: FlatZincAst) extends (Result => Seq[String]) {

    private val outputVarDecls =
        ast.varDecls.iterator
            .filter(_.annotations.iterator.map(_.term.id).exists(id => id == "output_var" || id == "output_array"))
            .toVector

    override def apply(result: Result) = {
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        val sortedMap = new mutable.TreeMap[String, String]() // id -> value
        for (decl <- outputVarDecls) {
            for (annotation <- decl.annotations) {
                annotation match {
                    case Annotation(Term("output_var", Nil)) =>
                        val x = compilerResult.vars(decl.id)
                        sortedMap += (decl.id -> value(result, x).toString)
                    case Annotation(Term("output_array", List(ArrayConst(dimensions)))) =>
                        val ArrayType(Some(IntRange(1, n)), _) = decl.valueType: @unchecked
                        val a =
                            "array%dd(%s, [%s])".format(
                                dimensions.size,
                                (for (case IntSetConst(IntRange(lb, ub)) <- dimensions.iterator) yield
                                    "%d..%d".format(lb, ub)).mkString(", "),
                                (for (idx <- (1 to n.toInt).iterator) yield
                                    value(result, compilerResult.arrays(decl.id)(idx - 1)).toString).mkString(", "))
                        sortedMap += (decl.id -> a)
                    case _ =>
                }
            }
        }
        val lines = mutable.ArrayBuffer[String]()
        for ((id, value) <- sortedMap) lines += "%s = %s;".format(id, value)
        if (result.isSolution) {
            lines += FlatZincSolutionSeparator
            if (result.objective.isInstanceOf[HierarchicalObjective] && result.isOptimal) {
                lines += FlatZincBestSolutionFoundIndicator
            }
        } else {
            lines += FlatZincNoSolutionFoundIndicator
        }
        lines.toSeq
    }

    private def value(result: Result, x: AnyVariable): AnyValue = {
        val a = result.bestProposal.value(x)
        a match {
            case b: BooleanValue => if (b.truthValue) True else False
            case _ => a
        }
    }

}
