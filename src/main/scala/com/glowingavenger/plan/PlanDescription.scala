package com.glowingavenger.plan

import com.glowingavenger.plan.model.{Problem, BeliefState}
import org.jgrapht.DirectedGraph
import com.glowingavenger.plan.model.action.Action
import org.jgrapht.graph.DefaultEdge

case class PlanDescription(init: BeliefState, plan: DirectedGraph[BeliefState, ActionEdge], problem: Problem)

case class ActionEdge(from: BeliefState, to: BeliefState, action: Action) extends DefaultEdge {
  override def toString: String = s"$from -> ($action) -> $to"
}
