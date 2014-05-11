package com.glowingavenger.plan.util

import org.jgrapht.DirectedGraph
import com.glowingavenger.plan.ActionEdge
import org.jgrapht.graph.{DirectedMultigraph, DefaultDirectedGraph}
import scala.collection.JavaConversions._
import ReachGraph._
import com.glowingavenger.plan.model.state.BeliefState

object ReachGraph {
  @inline implicit def graphToReachGraph(g: DirectedGraph[BeliefState, ActionEdge]): ReachGraph = new ReachGraph(g)
}

class ReachGraph(g: DirectedGraph[BeliefState, ActionEdge]) {
  def +++(other: DirectedGraph[BeliefState, ActionEdge]): DirectedGraph[BeliefState, ActionEdge] = {
    val newGraph = new DirectedMultigraph[BeliefState, ActionEdge](classOf[ActionEdge])
    for (v <- g.vertexSet()) newGraph.addVertex(v)
    for (v <- other.vertexSet()) newGraph.addVertex(v)
    for (e <- g.edgeSet()) newGraph.addEdge(e.from, e.to, e)
    for (e <- other.edgeSet()) newGraph.addEdge(e.from, e.to, e)
    newGraph
  }

  def ++(edges: Iterable[ActionEdge]): DirectedGraph[BeliefState, ActionEdge] = {
    val newGraph = g +++ new DirectedMultigraph[BeliefState, ActionEdge](classOf[ActionEdge])
    for (e <- edges) {
      newGraph.addVertex(e.from)
      newGraph.addVertex(e.to)
      newGraph.addEdge(e.from, e.to, e)
    }
    newGraph
  }

  def +(edge: ActionEdge): DirectedGraph[BeliefState, ActionEdge] = {
    val newGraph = g +++ new DirectedMultigraph[BeliefState, ActionEdge](classOf[ActionEdge])
    newGraph.addVertex(edge.from)
    newGraph.addVertex(edge.to)
    newGraph.addEdge(edge.from, edge.to, edge)
    newGraph
  }

  def <<>>(v: BeliefState): Set[ActionEdge] = asScalaSet[ActionEdge](g.outgoingEdgesOf(v)).toSet
}
