package yuck.flatzinc.compiler

import yuck.core.{given, *}
import yuck.flatzinc.ast.*

/**
 * Generates Yuck variables from FlatZinc parameter and variable declarations.
 *
 * For each class of FlatZinc parameters and variables that, in earlier phases, were identified to
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

    override def run() = {
        cc.ast.paramDecls.iterator.filterNot(_.valueType.isArrayType).foreach(createVariable)
        cc.ast.paramDecls.iterator.filter(_.valueType.isArrayType).foreach(createVariable)
        cc.ast.varDecls.iterator.filterNot(_.valueType.isArrayType).foreach(createVariable)
        cc.ast.varDecls.iterator.filter(_.valueType.isArrayType).foreach(createVariable)
    }

    import HighPriorityImplicits.*

    private def createVariable(decl: PlaceholderDecl): Unit = {
        decl.valueType match {
            case BoolType =>
                createVariable[BooleanValue](Term(decl.id, Nil))
            case IntType(_) =>
                createVariable[IntegerValue](Term(decl.id, Nil))
            case IntSetType(_) =>
                createVariable[IntegerSetValue](Term(decl.id, Nil))
            case ArrayType(Some(IntRange(1, n)), BoolType) =>
                val array =
                    for (idx <- 1 to n.toInt) yield {
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable[BooleanValue](key)
                    }
                cc.arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), IntType(_)) =>
                val array =
                    for (idx <- 1 to n.toInt) yield {
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable[IntegerValue](key)
                    }
                cc.arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), IntSetType(_)) =>
                val array =
                    for (idx <- 1 to n.toInt) yield {
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable[IntegerSetValue](key)
                    }
                cc.arrays += Term(decl.id, Nil) -> array
            case other =>
                throw new UnsupportedFlatZincTypeException(other)
        }
    }

    private def createVariable
        [V <: Value[V]]
        (key: Expr)
        (using valueTraits: ValueTraits[V]):
        Variable[V] =
    {
        def factory(key: Expr) =
            valueTraits.createVariable(cc.space, key.toString, valueTraits.safeDowncast(cc.domains(key)))
        val maybeEqualVars = cc.equalVars.get(key)
        if (maybeEqualVars.isDefined) {
            val representative = maybeEqualVars.get.head
            if (! cc.vars.contains(representative)) {
                cc.vars += representative -> factory(representative)
            }
            val x = valueTraits.safeDowncast(cc.vars(representative))
            if (key != representative) {
                cc.vars += key -> x
            }
            x
        }
        else {
            val x = factory(key)
            cc.vars += key -> x
            x
        }
    }

}
