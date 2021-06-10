package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast._
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class CompilationContext(
    val ast: FlatZincAst,
    val cfg: FlatZincSolverConfiguration,
    val logger: LazyLogger,
    val sigint: Sigint)
{
    val declaredVars = new mutable.HashSet[Expr]
    val equalVars = new mutable.AnyRefMap[Expr, mutable.TreeSet[Expr] /* head = representative */]
    val impliedConstraints = new mutable.HashSet[yuck.flatzinc.ast.Constraint]
    val space = new Space(logger, sigint, cfg.checkIncrementalCostUpdate, cfg.checkAssignmentsToNonChannelVariables)
    val consts = new mutable.AnyRefMap[Expr, AnyVariable] // holds unnamed inline constants
    val arrayConsts = new mutable.AnyRefMap[Expr, immutable.IndexedSeq[AnyVariable]] // holds unnamed inline arrays
    val vars = new mutable.AnyRefMap[Expr, AnyVariable] // also holds named parameters
    val arrays = new mutable.AnyRefMap[Expr, immutable.IndexedSeq[AnyVariable]]
    val domains = new mutable.AnyRefMap[Expr, AnyDomain]
    val searchVars = new mutable.HashSet[AnyVariable]
    val definedVars = new mutable.HashSet[AnyVariable] // variables that occur in a defines_var annotation
    val implicitlyConstrainedVars = new mutable.HashSet[AnyVariable]
    val costVars = new mutable.ArrayBuffer[BooleanVariable]
    var objective: AnyObjective = null
    var maybeNeighbourhood: Option[Neighbourhood] = null
}
