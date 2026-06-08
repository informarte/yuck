package yuck.flatzinc.compiler

import java.time.Duration
import java.util.concurrent.Callable

import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.ast.FlatZincAst
import yuck.util.arm.Sigint
import yuck.util.logging.{LazyLogger, LogLevel}

/**
 * This class orchestrates the various compiler stages.
 *
 * There are a lot of constraints on the execution order of the stages,
 * see src/doc/design/compiler/compiler-stage-ordering.mzn.
 * (The problem has 5 solutions, one of which is implemented here.)
 */
final class FlatZincCompiler1
(
    ast: FlatZincAst,
    cfg: FlatZincSolverConfiguration,
    sharedBound: SharedBound,
    logger: LazyLogger,
    sigint: Sigint)
    extends Callable[CompilationContext]
{

    override def call() = {
        val (cc, runtime) = logger.withTimedLogScope("Compiling problem") {
            compile()
        }
        cc.compilerRuntime = cc.compilerRuntime.plus(runtime)
        cc
    }

    private def compile(): CompilationContext = {

        val cc = new CompilationContext(
            ast, cfg, sharedBound, logger, sigint,
            new Space(logger, sigint, cfg.checkAssignmentsToNonChannelVariables, cfg.maybeSpaceProfilingMode))

        val domainInitializerRuntime = run(new DomainInitializer(cc))
        val variableFactoryRuntime = run(new VariableFactory(cc))
        cc.equalVars.clear()
        val variableClassifierRuntime = run(new VariableClassifier(cc))
        val constraintFactoryRuntime = run(new ConstraintFactory(cc))
        cc.impliedConstraints.clear()
        val cycleBreakerRuntime = run(new CycleBreaker(cc))
        val presolverRuntime = if cfg.runPresolver then run(new Presolver(cc)) else Duration.ZERO
        val objectiveFactoryRuntime = run(new ObjectiveFactory(cc))
        val warmStartAnnotationParserRuntime = run(new WarmStartAnnotationParser(cc))

        cc.danglingVars.addAll(cc.vars.values.filter(cc.space.isDanglingVariable))

        cc.domains.clear()
        cc.consts.clear()
        cc.vars.clear()
        cc.arrayConsts.clear()
        cc.arrays.clear()

        cc.compilerStageRuntimes = FlatZincCompilerStageRuntimes(
            domainInitializerRuntime, variableFactoryRuntime, variableClassifierRuntime, constraintFactoryRuntime,
            cycleBreakerRuntime, Duration.ZERO, presolverRuntime, objectiveFactoryRuntime, Duration.ZERO, Duration.ZERO,
            warmStartAnnotationParserRuntime)

        cc
    }

    // Use the optional root log level to focus on a particular compilation phase.
    private def run(phase: CompilationPhase, rootLogLevel: LogLevel = LogLevel.FineLogLevel): Duration = {
        if sigint.isSet then {
            throw new FlatZincCompilerInterruptedException
        }
        val (_, duration) =
            logger.withRootLogLevel(rootLogLevel) {
                logger.withTimedLogScope("Running %s".format(phase.getClass.getSimpleName)) {
                    phase.run()
                }
            }
        duration
    }

}
