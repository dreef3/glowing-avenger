package com.glowingavenger.plan.impl

import com.glowingavenger.plan.model.Problem
import scala.collection.immutable.Queue
import org.jgrapht.DirectedGraph
import com.glowingavenger.plan.{ActionEdge, PlanDescription}
import com.glowingavenger.plan.util.ReachGraph._
import com.glowingavenger.plan.model.action.Question
import com.glowingavenger.plan.model.state.BeliefState
import org.jgrapht.graph.DirectedMultigraph

trait ProblemAware {
  def problem: Problem
}

abstract class AbstractPlanner extends Successors with Axioms with ProblemAware with PlanInitializer {

  def build(): PlanDescription = {
    val init = initState()
    val front = Queue(init)
    val empty = new DirectedMultigraph[BeliefState, ActionEdge](classOf[ActionEdge])
    val graph = buildRec(front, empty)
    PlanDescription(init, graph, problem)
  }

  private def buildRec(front: Queue[BeliefState], plan: DirectedGraph[BeliefState, ActionEdge]): DirectedGraph[BeliefState, ActionEdge] = {
    if (front.isEmpty) plan
    else {
      val (next, nextQueue) = front.dequeue
      val (edges, queued) = successors(next)
      buildRec(nextQueue ++ queued.toSet, plan ++ edges.toSet)
    }
  }
}
