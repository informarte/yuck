package yuck.flatzinc.test.util

import scala.collection.mutable

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.nio.{Attribute, AttributeType, DefaultAttribute}

import yuck.core.*

final class DotExporter(space: Space, dotWriter: java.io.FileWriter) extends Runnable {

    import DotExporter.*

    private def vertexAttributes(v: Vertex): java.util.Map[String, Attribute] = {
        val attrMap = new java.util.HashMap[String, Attribute]
        v match {
            case VariableVertex(x) =>
                attrMap.put("label", new DefaultAttribute(x.toString.take(MaxLabelLength), AttributeType.STRING))
                val tooltip = "%s: %s = %s".format(x, x.domain, space.searchState.value(x))
                attrMap.put("tooltip", new DefaultAttribute(tooltip.take(MaxTooltipLength), AttributeType.STRING))
                val maybeColor =
                    if space.isSearchVariable(x)
                    then Some(Red)
                    else if space.isProblemParameter(x)
                    then Some(Blue)
                    else None
                if maybeColor.isDefined then {
                    attrMap.put("fontcolor", maybeColor.get)
                }
            case ConstraintVertex(constraint) =>
                attrMap.put("label", new DefaultAttribute(constraint.getClass.getSimpleName, AttributeType.STRING))
                val goals = space.goals(constraint)
                val tooltip =
                    if goals.isEmpty
                    then constraint.toString
                    else "%s\n[%s]".format(constraint, goals.mkString(", "))
                attrMap.put("tooltip", new DefaultAttribute(tooltip.take(MaxTooltipLength), AttributeType.STRING))
                val maybeColor =
                    if space.isImplicitConstraint(constraint)
                    then Some(Green)
                    else None
                if maybeColor.isDefined then {
                    attrMap.put("fontcolor", maybeColor.get)
                }
        }
        attrMap
    }

    override def run() = {
        val network = new DefaultDirectedGraph[Vertex, DefaultEdge](classOf[DefaultEdge])
        val variableVertices = new mutable.HashMap[AnyVariable, VariableVertex]
        val constraints = new mutable.HashSet[Constraint]
        for x <- space.searchVariables do {
            val v = VariableVertex(x)
            variableVertices += x -> v
            network.addVertex(v)
            constraints ++= space.directlyAffectedConstraints(x)
        }
        for x <- space.problemParameters do {
            val v = VariableVertex(x)
            variableVertices += x -> v
            network.addVertex(v)
            constraints ++= space.directlyAffectedConstraints(x)
        }
        for x <- space.channelVariables do {
            val v = VariableVertex(x)
            variableVertices += x -> v
            network.addVertex(v)
            constraints += space.definingConstraint(x) // include constraints with no inputs, e.g. and([])
            constraints ++= space.directlyAffectedConstraints(x)
        }
        for constraint <- constraints do {
            val v = ConstraintVertex(constraint)
            network.addVertex(v)
            for x <- constraint.inVariables do {
                network.addEdge(variableVertices(x), v)
            }
            for x <- constraint.outVariables do {
                network.addEdge(v, variableVertices(x))
            }
        }
        val exporter = new DOTExporter[Vertex, DefaultEdge]
        exporter.setVertexAttributeProvider(vertexAttributes)
        exporter.exportGraph(network, dotWriter)
    }

}

object DotExporter {

    private trait Vertex
    private case class VariableVertex(x: AnyVariable) extends Vertex
    private case class ConstraintVertex(constraint: Constraint) extends Vertex

    private val Red = new DefaultAttribute("red", AttributeType.STRING)
    private val Blue = new DefaultAttribute("blue", AttributeType.STRING)
    private val Green = new DefaultAttribute("green", AttributeType.STRING)

    private val MaxLabelLength = 32
    private val MaxTooltipLength = 1024

}
