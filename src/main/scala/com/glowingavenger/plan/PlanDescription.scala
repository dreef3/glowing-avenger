package com.glowingavenger.plan

import org.jgrapht.DirectedGraph
import com.glowingavenger.plan.model.action.{Question, Action}
import org.jgrapht.graph.DefaultEdge
import com.glowingavenger.plan.model.state.BeliefState
import com.glowingavenger.plan.model.Problem

case class PlanDescription(init: BeliefState, plan: DirectedGraph[BeliefState, ActionEdge], problem: Problem)

case class ActionEdge(from: BeliefState, to: BeliefState, action: Action) extends DefaultEdge {
  override def toString: String = action match {
    case q: Question if to.predicates(q.attr) == Some(true) => q.toString
    case q: Question => "~" + q.toString
    case other => other.toString
  }
}
