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
final class FlatZincResultFormatter(result: Result) extends Callable[Seq[String]] {

    private val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]

    override def call() = {
        var sortedMap = new immutable.TreeMap[String, String]() // id -> value
        for (decl <- compilerResult.ast.varDecls) {
            for (annotation <- decl.annotations) {
                annotation match {
                    case Annotation(Term("output_var", Nil)) =>
                        val x = compilerResult.vars(decl.id)
                        sortedMap = sortedMap + (decl.id -> value(x).toString)
                    case Annotation(Term("output_array", List(ArrayConst(dimensions)))) =>
                        val ArrayType(Some(IntRange(1, n)), _) = decl.valueType: @unchecked
                        val a =
                            "array%dd(%s, [%s])".format(
                                dimensions.size,
                                (for (case IntSetConst(IntRange(lb, ub)) <- dimensions) yield
                                    "%d..%d".format(lb, ub)).mkString(", "),
                                (for (idx <- 1 to n.toInt) yield
                                    value(compilerResult.arrays(decl.id)(idx - 1)).toString).mkString(", "))
                        sortedMap = sortedMap + (decl.id -> a)
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

    private def value(x: AnyVariable): AnyValue = {
        val a = result.bestProposal.value(x)
        a match {
            case b: BooleanValue => if (b.truthValue) True else False
            case _ => a
        }
    }

}
