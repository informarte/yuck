package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.flatzinc.ast.FlatZincAst

/**
 * @author Michael Marte
 *
 */
final class FlatZincCompilerResult(
    val ast: FlatZincAst,
    val space: Space,
    val vars: immutable.Map[String, AnyVariable], // also holds named parameters
    val arrays: immutable.Map[String, immutable.IndexedSeq[AnyVariable]],
    val costVar: Variable[IntegerValue],
    val maybeObjectiveVar: Option[Variable[IntegerValue]],
    val objective: AnyObjective,
    val maybeNeighbourhood: Option[Neighbourhood]
)
