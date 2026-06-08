package yuck.flatzinc.compiler

import java.time.Duration

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.*
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

final class CompilationContext(
    val ast: FlatZincAst,
    val cfg: FlatZincSolverConfiguration,
    val sharedBound: SharedBound,
    val logger: LazyLogger,
    val sigint: Sigint,
    val space: Space,
    val equalVars: mutable.HashMap[Expr, mutable.TreeSet[Expr] /* head = representative */] = mutable.HashMap.empty,
    val impliedConstraints: mutable.HashSet[yuck.flatzinc.ast.Constraint] = mutable.HashSet.empty,
    val domains: mutable.HashMap[Expr, AnyDomain] = mutable.HashMap.empty,
    val consts: mutable.HashMap[Expr, AnyVariable] = mutable.HashMap.empty, // holds unnamed inline constants
    val vars: mutable.HashMap[Expr, AnyVariable] = mutable.HashMap.empty, // holds named variables and parameters
    val arrayConsts: mutable.HashMap[Expr, immutable.IndexedSeq[AnyVariable]] = mutable.HashMap.empty, // holds unnamed inline arrays
    val arrays: mutable.HashMap[Expr, immutable.IndexedSeq[AnyVariable]] = mutable.HashMap.empty, // holds named arrays of variables and parameters
    val searchVars: mutable.HashSet[AnyVariable] = mutable.HashSet.empty, // variables that occur in a search annotation
    val definedVars: mutable.HashSet[AnyVariable] = mutable.HashSet.empty, // variables that have a defines_var annotation
    val outputVars: mutable.HashMap[VarDecl, AnyVariable] = mutable.HashMap.empty, // variables that have an output_var annotation
    val outputArrays: mutable.HashMap[VarDecl, immutable.IndexedSeq[AnyVariable]] = mutable.HashMap.empty, // variables that have an output_array annotation
    val danglingVars: mutable.HashSet[AnyVariable] = mutable.HashSet.empty,
    val implicitlyConstrainedVars: mutable.HashSet[AnyVariable] = mutable.HashSet.empty,
    val costVars: mutable.HashSet[BooleanVariable] = mutable.HashSet.empty,
    val costVarsFromRedundantConstraints: mutable.HashSet[BooleanVariable] = mutable.HashSet.empty,
    val warmStartAssignment: mutable.HashMap[AnyVariable, AnyVariable] = mutable.HashMap.empty,
    var objective: AnyObjective = null,
    var maybeNeighbourhood: Option[Neighbourhood] = null,
    var compilerRuntime: Duration = Duration.ZERO,
    var compilerStageRuntimes: FlatZincCompilerStageRuntimes = null)
{

    /**
     * Returns a shallow copy of this context with the original configuration replaced by the given one.
     */
    def clone(cfg: FlatZincSolverConfiguration): CompilationContext = new CompilationContext(
        ast, cfg, sharedBound, logger, sigint, space,
        equalVars, impliedConstraints, domains, consts, vars, arrayConsts, arrays,
        searchVars, definedVars, outputVars, outputArrays, danglingVars, implicitlyConstrainedVars, costVars,
        costVarsFromRedundantConstraints, warmStartAssignment, objective, maybeNeighbourhood, compilerRuntime,
        compilerStageRuntimes
    )

    /**
     * Returns a deep copy of this context with the original configuration replaced by the given one.
     */
    def copy(cfg: FlatZincSolverConfiguration): CompilationContext = {
        val copy = new CompilationContext(ast, cfg, sharedBound, logger, sigint, space.copy())
        copy.equalVars.addAll(equalVars.view.mapValues(value => mutable.TreeSet.from(value)(using value.ordering)))
        copy.impliedConstraints.addAll(impliedConstraints)
        copy.domains.addAll(domains)
        copy.consts.addAll(consts)
        copy.vars.addAll(vars)
        copy.arrayConsts.addAll(arrayConsts)
        copy.arrays.addAll(arrays)
        copy.searchVars.addAll(searchVars)
        copy.outputVars.addAll(outputVars)
        copy.outputArrays.addAll(outputArrays)
        copy.danglingVars.addAll(danglingVars)
        copy.definedVars.addAll(definedVars)
        copy.implicitlyConstrainedVars.addAll(implicitlyConstrainedVars)
        copy.costVars.addAll(costVars)
        copy.costVarsFromRedundantConstraints.addAll(costVarsFromRedundantConstraints)
        copy.warmStartAssignment.addAll(warmStartAssignment)
        copy.objective = if objective == null then null else objective.copy
        copy.maybeNeighbourhood = maybeNeighbourhood
        copy.compilerRuntime = compilerRuntime
        copy.compilerStageRuntimes = compilerStageRuntimes
        copy
    }

    def post(goals: immutable.Set[Goal], constraint: yuck.core.Constraint): CompilationContext = {
        if cfg.checkIncrementalCostUpdate(constraint) then {
            space.post(new CheckedConstraint(constraint))
        } else {
            space.post(constraint)
        }
        space.registerGoals(constraint, goals)
        this
    }

    def post(goals: immutable.Seq[Goal], constraint: yuck.core.Constraint): CompilationContext =
        post(goals.toSet, constraint)


}
