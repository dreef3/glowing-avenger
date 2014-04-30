package com.glowingavenger.agent

import scala.actors.Actor
import com.glowingavenger.plan.problem.{BeliefState, PDDLProblem}
import com.glowingavenger.plan.{ActionEdge, ContingencyPlan}
import scala.collection.JavaConversions._
import scala.collection.mutable

trait PlanTraversalListener {
  def onStateChange(before: Option[BeliefState], state: BeliefState,
                    producer: Option[ActionEdge], successors: Iterable[ActionEdge]): ActionEdge

  def onFinish(state: BeliefState, producer: Option[ActionEdge])
  def onFailure(state: BeliefState, producer: Option[ActionEdge])
}

class PlanExecutor(problem: PDDLProblem, listener: PlanTraversalListener) extends Actor {
  override def act() {
    val plan = ContingencyPlan.build(problem)

    def doAct(before: Option[BeliefState], state: BeliefState, producer: Option[ActionEdge]) {
      if (problem.goal includes state) {
        listener.onFinish(state, producer)
      } else {
        val successors: mutable.Set[ActionEdge] = plan.plan.outgoingEdgesOf(state)
        if (successors.isEmpty) {
          listener.onFailure(state, producer)
        } else {
          val newProducer = listener.onStateChange(before, state, producer, successors)
          val newState = newProducer.to
          doAct(Some(state), newState, Some(newProducer))
        }
      }
    }

    doAct(None, plan.init, None)
  }
}
