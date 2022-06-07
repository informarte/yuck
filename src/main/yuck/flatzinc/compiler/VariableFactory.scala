package yuck.flatzinc.compiler

import yuck.core.*
import yuck.flatzinc.ast.*

/**
 * Generates Yuck variables from FlatZinc variable definitions.
 *
 * For each class of FlatZinc variables that, in earlier phases, were identified to
 * be equivalent, a representative is chosen and only for this representative a Yuck
 * variable is introduced.
 *
 * Notice that other phases may introduce additional variables on-the-fly as needed.
 *
 * @author Michael Marte
 */
final class VariableFactory
    (override protected val cc: CompilationContext)
    extends CompilationPhase
{

    private val domains = cc.domains
    private val vars = cc.vars
    private val equalVars = cc.equalVars
    private val arrays = cc.arrays
    private val space = cc.space

    override def run() = {
        cc.ast.paramDecls.foreach(createParameter)
        cc.ast.varDecls.foreach(createVariable)
    }

    import HighPriorityImplicits.*

    private def createParameter(decl: ParamDecl): Unit = {
        decl.paramType match {
            case BoolType =>
                val x = compileExpr[BooleanValue](decl.value)
                vars += Term(decl.id, Nil) -> x
            case IntType(_) =>
                val x = compileExpr[IntegerValue](decl.value)
                vars += Term(decl.id, Nil) -> x
            case IntSetType(_) =>
                val x = compileExpr[IntegerSetValue](decl.value)
                vars += Term(decl.id, Nil) -> x
            case ArrayType(Some(IntRange(1, n)), BoolType) =>
                val array = compileArray[BooleanValue](decl.value)
                assert(array.size == n)
                arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), IntType(_)) =>
                val array = compileArray[IntegerValue](decl.value)
                assert(array.size == n)
                arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), IntSetType(_)) =>
                val array = compileArray[IntegerSetValue](decl.value)
                assert(array.size == n)
                arrays += Term(decl.id, Nil) -> array
            case other =>
                throw new UnsupportedFlatZincTypeException(other)
        }
    }

    private def createVariable(decl: VarDecl): Unit = {
        decl.varType match {
            case BoolType =>
                createVariable[BooleanValue](Term(decl.id, Nil))
            case IntType(_) =>
                createVariable[IntegerValue](Term(decl.id, Nil))
            case IntSetType(_) =>
                createVariable[IntegerSetValue](Term(decl.id, Nil))
            case ArrayType(Some(IntRange(1, n)), BoolType) =>
                val array =
                    for (idx <- 1 to n.toInt) yield {
                        val name = "%s[%d]".format(decl.id, idx)
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable[BooleanValue](key)
                    }
                arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), IntType(_)) =>
                val array =
                    for (idx <- 1 to n.toInt) yield {
                        val name = "%s[%d]".format(decl.id, idx)
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable[IntegerValue](key)
                    }
                arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), IntSetType(_)) =>
                val array =
                    for (idx <- 1 to n.toInt) yield {
                        val name = "%s[%d]".format(decl.id, idx)
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable[IntegerSetValue](key)
                    }
                arrays += Term(decl.id, Nil) -> array
            case other =>
                throw new UnsupportedFlatZincTypeException(other)
        }
    }

    private def createVariable
        [V <: OrderedValue[V]]
        (key: Expr)
        (implicit valueTraits: OrderedValueTraits[V]):
        OrderedVariable[V] =
    {
        def factory(key: Expr) =
            valueTraits.createVariable(space, key.toString, valueTraits.safeDowncast(domains(key)))
        val maybeEqualVars = equalVars.get(key)
        if (maybeEqualVars.isDefined) {
            val representative = maybeEqualVars.get.head
            if (! vars.contains(representative)) {
                vars += representative -> factory(representative)
            }
            val x = valueTraits.safeDowncast(vars(representative))
            if (key != representative) {
                vars += key -> x
            }
            x
        }
        else {
            val x = factory(key)
            vars += key -> x
            x
        }
    }

}
