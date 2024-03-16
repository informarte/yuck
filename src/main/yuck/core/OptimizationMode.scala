package yuck.core

/**
 * @author Michael Marte
 *
 */
enum OptimizationMode(name: String) {
    case Min extends OptimizationMode("Minimization")
    case Max extends OptimizationMode("Maximization")
    override def toString = name
}
