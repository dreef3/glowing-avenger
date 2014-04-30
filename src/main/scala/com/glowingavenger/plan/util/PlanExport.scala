package com.glowingavenger.plan.util

import scalax.collection.Graph
import scalax.collection.edge.WLDiEdge
import com.glowingavenger.plan.problem.{Action, BeliefState}
import scalax.collection.io.dot._
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.{DefaultEdge, DefaultDirectedWeightedGraph}
import java.util
import com.glowingavenger.plan.PlanDescription
import scala.collection.JavaConversions._

object PlanExport {
  private implicit def beliefState2JavaMap(state: BeliefState): java.util.Map[String, java.lang.Boolean] = {
    if (state == null)
      null
    val map = new util.HashMap[String, java.lang.Boolean]()
    for ((s, v) <- state.attrs) {
      map.put(s.name, if (v.isDefined) v.get else null)
    }
    map
  }

  def exportDescription(planDescr: PlanDescription): PlanDescr = {
    val plan = planDescr.plan
    val g = new DefaultDirectedWeightedGraph[java.util.Map[String, java.lang.Boolean], ActionEdge](classOf[ActionEdge])
    val edges =  asScalaSet(plan.edgeSet())
    for {
      e: com.glowingavenger.plan.ActionEdge <- edges
      from = e.from
      to = e.to
    } {
      g.addVertex(from)
      g.addVertex(to)
      g.addEdge(from, to, new ActionEdge(from, to, e.action))
    }

    val goal: BeliefState = plan.vertexSet() find (planDescr.problem.goal includes) match {
      case Some(s) => s
      case None => null
    }
    PlanDescr(planDescr.init, goal, g)
  }
}

case class PlanDescr(init: java.util.Map[String, java.lang.Boolean], goal: java.util.Map[String, java.lang.Boolean], plan: DirectedGraph[java.util.Map[String, java.lang.Boolean], ActionEdge])

class ActionEdge(val from: java.util.Map[String, java.lang.Boolean], val to: java.util.Map[String, java.lang.Boolean], val action: Action) extends DefaultEdge
