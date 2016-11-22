package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast._
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class CompilationContext(
    var ast: FlatZincAST, // allow for AST transformation
    val cfg: FlatZincSolverConfiguration,
    val logger: LazyLogger)
{
    val declaredVars = new mutable.HashSet[Expr]
    val equalVars = new mutable.HashMap[Expr, mutable.TreeSet[Expr] /* head = representative */]
    val impliedConstraints = new mutable.HashSet[yuck.flatzinc.ast.Constraint]
    val space = new Space(logger)
    val consts = new mutable.HashMap[Expr, AnyVariable] // holds unnamed inline constants
    val arrayConsts = new mutable.HashMap[Expr, immutable.IndexedSeq[AnyVariable]] // holds unnamed inline arrays
    val vars = new mutable.HashMap[Expr, AnyVariable] // also holds named parameters
    val arrays = new mutable.HashMap[Expr, immutable.IndexedSeq[AnyVariable]]
    val domains = new mutable.HashMap[Expr, AnyDomain]
    val searchVars = new mutable.HashSet[AnyVariable]
    val channelVars = new mutable.HashSet[AnyVariable]
    val costVars = new mutable.ArrayBuffer[Variable[IntegerValue]]
    var costVar: Variable[IntegerValue] = null
    var objective: AnyObjective = null
    var strategy: MoveGenerator = null
}
