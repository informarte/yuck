package yuck.flatzinc.compiler

import yuck.core._
import yuck.flatzinc.ast._

/**
 * Generates Yuck variables from FlatZinc variable definitions.
 *
 * Boolean FlatZinc variables get translated to integer Yuck variables with infinite,
 * non-negative domains where 0 means true and positive values mean false.
 * (Positive values in the domains of Boolean channel variables will be used to encode
 * the degree of violation of reified constraints.)
 *
 * We generate Yuck variables with unbounded domains and enforce their actual domains
 * (as specified by the FlatZinc model) in a later phase, either by pruning the domain
 * or by means of suitable constraints. In particular, the domains of Boolean search
 * variables will be reduced to {0, 1}.
 *
 * For each class of FlatZinc variables that, in earlier phases, were identified to
 * be equivalent, a representative is chosen and only for this representative a Yuck variable
 * is introduced.
 *
 * Notice that other phases may introduce additional variables on-the-fly as needed.
 *
 * @author Michael Marte
 */
class VariableFactory
    (cc: CompilationContext, randomGenerator: RandomGenerator)
    extends CompilationPhase(cc, randomGenerator)
{

    private val domains = cc.domains
    private val vars = cc.vars
    private val equalVars = cc.equalVars
    private val arrays = cc.arrays
    private val space = cc.space

    override def run {
        cc.ast.paramDecls.foreach(createParameter)
        cc.ast.varDecls.foreach(createVariable)
    }

    private def createParameter(decl: ParamDecl) {
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
        }
    }

    private def createVariable(decl: VarDecl) {
        decl.varType match {
            case BoolType =>
                createVariable(Term(decl.id, Nil), BoolType, createBooleanVariable)
            case IntType(_) =>
                createVariable(Term(decl.id, Nil), decl.varType, createIntegerVariable)
            case IntSetType(_) =>
                createVariable(Term(decl.id, Nil), decl.varType, createIntegerSetVariable)
            case ArrayType(Some(IntRange(1, n)), BoolType) =>
                val array =
                    for (idx <- 1 to n) yield {
                        val name = "%s[%d]".format(decl.id, idx)
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable(key, BoolType, createBooleanVariable)
                    }
                arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), baseType@IntType(_)) =>
                val array =
                    for (idx <- 1 to n) yield {
                        val name = "%s[%d]".format(decl.id, idx)
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable(key, baseType, createIntegerVariable)
                    }
                arrays += Term(decl.id, Nil) -> array
            case ArrayType(Some(IntRange(1, n)), baseType@IntSetType(_)) =>
                val array =
                    for (idx <- 1 to n) yield {
                        val name = "%s[%d]".format(decl.id, idx)
                        val key = ArrayAccess(decl.id, IntConst(idx))
                        createVariable(key, baseType, createIntegerSetVariable)
                    }
                arrays += Term(decl.id, Nil) -> array
        }
    }

    private def createVariable
        [Value <: OrderedValue[Value]]
        (key: Expr, varType: Type, factory: (Expr, Type) => Variable[Value]):
        Variable[Value] =
    {
        val maybeEqualVars = equalVars.get(key)
        if (maybeEqualVars.isDefined) {
            val representative = maybeEqualVars.get.head
            if (! vars.contains(representative)) {
                vars += representative -> factory(representative, varType)
            }
            val x = vars(representative).asInstanceOf[Variable[Value]]
            if (key != representative) {
                vars += key -> x
            }
            x
        }
        else {
            factory(key, varType)
        }
    }

    private def createBooleanVariable(key: Expr, varType: Type): Variable[IntegerValue] = {
        val name = key.toString
        val x = space.createVariable(name, ZeroOneIntegerDomain)
        val dx = BooleanValueTraits.dynamicDowncast(domains(key))
        if (dx.isSingleton) {
            x.pruneDomain(if (dx.singleValue == True) ZeroIntegerDomain else OneIntegerDomain)
        }
        vars += key -> x
        x
    }

    private def createIntegerVariable(key: Expr, varType: Type): Variable[IntegerValue] = {
        val name = key.toString
        val x = space.createVariable(name, IntegerValueTraits.dynamicDowncast(domains(key)))
        vars += key -> x
        x
    }

    private def createIntegerSetVariable(key: Expr, varType: Type): Variable[IntegerSetValue] = {
        val name = key.toString
        val x = space.createVariable(name, IntegerSetValueTraits.dynamicDowncast(domains(key)))
        vars += key -> x
        x
    }

}
