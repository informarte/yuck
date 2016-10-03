package yuck.flatzinc.runner

import java.util.concurrent.Callable

import scala.collection._

import yuck.core._
import yuck.flatzinc.ast._
import yuck.flatzinc.compiler.FlatZincCompilerResult

/**
 * @author Michael Marte
 *
 */
final class FlatZincResultFormatter(result: Result) extends Callable[Seq[String]] {

    private val compilerResult = result.userData.asInstanceOf[FlatZincCompilerResult]

    override def call = {
        var sortedMap = new immutable.TreeMap[String, String]() // id -> value
        for (decl <- compilerResult.ast.varDecls) {
            for (annotation <- decl.annotations) {
                annotation match {
                    case Annotation(Term("output_var", Nil)) =>
                        val x = compilerResult.vars(decl.id)
                        sortedMap = sortedMap + (decl.id -> value(x, decl).toString)
                    case Annotation(Term("output_array", List(ArrayConst(dimensions)))) =>
                        val ArrayType(Some(IntRange(1, n)), _) = decl.varType
                        val a =
                            "array%dd(%s, [%s])".format(
                                dimensions.size,
                                (for (IntSetConst(IntRange(lb, ub)) <- dimensions) yield
                                    "%d..%d".format(lb, ub)).mkString(", "),
                                (for (idx <- 1 to n) yield
                                    value(compilerResult.arrays(decl.id)(idx - 1), decl).toString).mkString(", "))
                        sortedMap = sortedMap + (decl.id -> a)
                    case _ =>
                }
            }
        }
        val lines = mutable.ArrayBuffer[String]()
        for ((id, value) <- sortedMap) lines += "%s = %s;".format(id, value)
        if (result.isSolution) {
            lines += FLATZINC_SOLUTION_SEPARATOR
            if (result.objective.isInstanceOf[HierarchicalObjective] && result.isGoodEnough) {
                lines += FLATZINC_BEST_SOLUTION_FOUND_INDICATOR
            }
        } else {
            lines += FLATZINC_NO_SOLUTION_FOUND_INDICATOR
        }
        lines.toSeq
    }

    private def value(x: AnyVariable, decl: VarDecl): AnyValue = decl.varType match {
        case BoolType | ArrayType(_, BoolType) =>
            if (result.bestProposal.value(IntegerValue.Traits.staticCast(x)) == Zero) True else False
        case _ => result.bestProposal.anyValue(x)
    }

}
