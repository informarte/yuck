package yuck.flatzinc.runner

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.ast.*

object FlatZincResultFormatter extends (FlatZincResult => Seq[String]) {

    override def apply(result: FlatZincResult) = {
        val compilerResult = result.compilerResult
        val searchState = result.searchState
        val outputMap = new mutable.TreeMap[String, String]() // id -> value
        for case (decl, x) <- compilerResult.outputVars do {
            outputMap += decl.id -> value(searchState, x).toString
        }
        for (decl, xs) <- compilerResult.outputArrays do {
            val Annotation(Term("output_array", List(ArrayConst(dimensions)))) = decl.annotations.head.runtimeChecked
            val ArrayType(Some(IntRange(1, n)), _) = decl.valueType.runtimeChecked
            val a =
                "array%dd(%s, [%s])".format(
                    dimensions.size,
                    (for case IntSetConst(IntRange(lb, ub)) <- dimensions.iterator yield
                        "%d..%d".format(lb, ub)).mkString(", "),
                    (1 to n.toInt).iterator
                        .map(idx => value(searchState, xs(idx - 1)).toString)
                        .mkString(", ")
                )
            outputMap += decl.id -> a
        }
        val lines = mutable.ArrayBuffer[String]()
        for (id, value) <- outputMap do lines += "%s = %s;".format(id, value)
        val objective = compilerResult.objective
        val costs = objective.costs(result.searchState)
        if objective.isSolution(costs) then {
            lines += FlatZincSolutionSeparator
            if objective.isInstanceOf[HierarchicalObjective] && objective.isOptimal(costs) then {
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
            case b: BooleanValue => if b.truthValue then True else False
            case _ => a
        }
    }

}
