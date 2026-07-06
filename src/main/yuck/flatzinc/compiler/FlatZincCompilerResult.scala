package yuck.flatzinc.compiler

import java.time.Duration

import scala.collection.*

import yuck.core.*
import yuck.flatzinc.ast.VarDecl

final class FlatZincCompilerStageRuntimes(
    val domainInitializerRuntime: Duration,
    val variableFactoryRuntime: Duration,
    val variableClassifierRuntime: Duration,
    val constraintFactoryRuntime: Duration,
    val cycleBreakerRuntime: Duration,
    val objectiveFactoryRuntime: Duration,
    val presolverRuntime: Duration,
    val neighbourhoodFactoryRuntime: Duration,
    val constraintNetworkPrunerRuntime: Duration,
    val arrayAccessOptimizerRuntime: Duration,
    val warmStartAnnotationParserRuntime: Duration
)

final class FlatZincCompilerResult(
    val space: Space,
    val outputVars: immutable.Map[VarDecl, AnyVariable], // also holds named parameters
    val outputArrays: immutable.Map[VarDecl, immutable.IndexedSeq[AnyVariable]],
    val objective: AnyObjective,
    val maybeNeighbourhood: Option[Neighbourhood],
    val performWarmStart: Boolean,
    val runtime: Duration,
    val stageRuntimes: FlatZincCompilerStageRuntimes
)
