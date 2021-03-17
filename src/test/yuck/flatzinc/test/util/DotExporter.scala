package yuck.flatzinc.test.util

import scala.collection.mutable

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.nio.{Attribute, AttributeType, DefaultAttribute}

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class DotExporter(space: Space, dotWriter: java.io.FileWriter) extends Runnable {

    private trait Vertex
    private case class VariableVertex(x: AnyVariable) extends Vertex
    private case class ConstraintVertex(constraint: Constraint) extends Vertex

    private val red = new DefaultAttribute("red", AttributeType.STRING)
    private val blue = new DefaultAttribute("blue", AttributeType.STRING)
    private val green = new DefaultAttribute("green", AttributeType.STRING)
    private val MaxLabelLength = 32
    private val MaxTooltipLength = 1024

    private def vertexAttributes(v: Vertex): java.util.Map[String, Attribute] = {
        val attrMap = new java.util.HashMap[String, Attribute]
        v match {
            case VariableVertex(x) =>
                attrMap.put("label", new DefaultAttribute(x.toString.take(MaxLabelLength), AttributeType.STRING))
                val typeInfo = x match {
                    case _: BooleanVariable => "bool"
                    case _: IntegerVariable => "int"
                    case _: IntegerSetVariable => "set of int"
                }
                val tooltip = "%s: %s = %s".format(x, typeInfo, space.searchState.value(x))
                attrMap.put("tooltip", new DefaultAttribute(tooltip.take(MaxTooltipLength), AttributeType.STRING))
                val maybeColor =
                    if (space.isSearchVariable(x)) Some(red)
                    else if (space.isProblemParameter(x)) Some(blue)
                    else None
                if (maybeColor.isDefined) {
                    attrMap.put("fontcolor", maybeColor.get)
                }
            case ConstraintVertex(constraint) =>
                attrMap.put("label", new DefaultAttribute(constraint.getClass.getSimpleName, AttributeType.STRING))
                attrMap.put("tooltip", new DefaultAttribute(constraint.toString.take(MaxTooltipLength), AttributeType.STRING))
                val maybeColor =
                    if (space.isImplicitConstraint(constraint)) Some(green)
                    else None
                if (maybeColor.isDefined) {
                    attrMap.put("fontcolor", maybeColor.get)
                }
        }
        attrMap
    }

    override def run() = {
        val network = new DefaultDirectedGraph[Vertex, DefaultEdge](classOf[DefaultEdge])
        val variableVertices = new mutable.HashMap[AnyVariable, VariableVertex]
        val constraints = new mutable.HashSet[Constraint]
        for (x <- space.searchVariables) {
            val v = VariableVertex(x)
            variableVertices += x -> v
            network.addVertex(v)
            constraints ++= space.directlyAffectedConstraints(x)
        }
        for (x <- space.problemParameters) {
            val v = VariableVertex(x)
            variableVertices += x -> v
            network.addVertex(v)
            constraints ++= space.directlyAffectedConstraints(x)
        }
        for (x <- space.channelVariables) {
            val v = VariableVertex(x)
            variableVertices += x -> v
            network.addVertex(v)
            constraints ++= space.directlyAffectedConstraints(x)
        }
        for (constraint <- constraints) {
            val v = ConstraintVertex(constraint)
            network.addVertex(v)
            for (x <- constraint.inVariables) {
                network.addEdge(variableVertices(x), v)
            }
            for (x <- constraint.outVariables) {
                network.addEdge(v, variableVertices(x))
            }
        }
        val exporter = new DOTExporter[Vertex, DefaultEdge]
        exporter.setVertexAttributeProvider(vertexAttributes)
        exporter.exportGraph(network, dotWriter)
    }

}
