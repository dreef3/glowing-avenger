package com.glowingavenger.plan.impl

import org.sat4j.scala.Logic.BoolExp
import com.glowingavenger.plan.model.{Problem, BeliefState}
import scala.collection.immutable.Queue
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.DirectedGraph
import com.glowingavenger.plan.{ActionEdge, PlanDescription}
import com.glowingavenger.plan.util.ReachGraph._
import com.glowingavenger.plan.model.action.Question

trait ProblemAware {
  def problem: Problem
}

abstract class AbstractPlanner extends Successors with Axioms with ProblemAware with PlanInitializer {

  def build(): PlanDescription = {
    val init = initState()
    val front = Queue(init)
    val empty = new DefaultDirectedGraph[BeliefState, ActionEdge](classOf[ActionEdge])
    val graph = buildRec(front, empty)
    PlanDescription(init, graph, problem)
  }

  private def buildRec(front: Queue[BeliefState], plan: DirectedGraph[BeliefState, ActionEdge]): DirectedGraph[BeliefState, ActionEdge] = {
    if (front.isEmpty) plan
    else {
      val (next, nextQueue) = front.dequeue
      val edges = successors(next)
      val answers = edges collect {
        case ActionEdge(from, to, q: Question) if from == next => to
      }
      buildRec(nextQueue ++ answers, plan ++ edges)
    }
  }
}
