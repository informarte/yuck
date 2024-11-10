package yuck.flatzinc.compiler

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.*
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
    val space = new Space(logger, sigint, cfg.checkIncrementalCostUpdate, cfg.checkAssignmentsToNonChannelVariables, cfg.delayCycleCheckingUntilInitialization)
    val consts = new mutable.AnyRefMap[Expr, AnyVariable] // holds unnamed inline constants
    val vars = new mutable.AnyRefMap[Expr, AnyVariable] // holds named variables and parameters
    val arrayConsts = new mutable.AnyRefMap[Expr, immutable.IndexedSeq[AnyVariable]] // holds unnamed inline arrays
    val arrays = new mutable.AnyRefMap[Expr, immutable.IndexedSeq[AnyVariable]] // holds named arrays of variables and parameters
    val domains = new mutable.AnyRefMap[Expr, AnyDomain]
    val searchVars = new mutable.HashSet[AnyVariable] // variables that occur in a search annotation
    val definedVars = new mutable.HashSet[AnyVariable] // variables that occur in a defines_var annotation
    val implicitlyConstrainedVars = new mutable.HashSet[AnyVariable]
    val costVars = new mutable.HashSet[BooleanVariable]
    val costVarsFromRedundantConstraints = new mutable.HashSet[BooleanVariable]
    val warmStartAssignment = new mutable.HashMap[AnyVariable, AnyVariable]
    var objective: AnyObjective = null
    var maybeNeighbourhood: Option[Neighbourhood] = null
}
