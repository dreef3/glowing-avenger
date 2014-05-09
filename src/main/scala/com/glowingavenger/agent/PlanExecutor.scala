package com.glowingavenger.agent

import com.glowingavenger.plan.model.{Problem, BeliefState}
import com.glowingavenger.plan.{ActionEdge, ContingencyPlan}
import com.glowingavenger.plan.model.BeliefStateImplicits._
import com.glowingavenger.plan.util.ReachGraph._

trait PlanTraversalListener {
  def onStateChange(before: Option[BeliefState], state: BeliefState,
                    producer: Option[ActionEdge], successors: Iterable[ActionEdge]): ActionEdge

  def onFinish(state: BeliefState, producer: Option[ActionEdge])
  def onFailure(state: BeliefState, producer: Option[ActionEdge])
}

class PlanExecutor(problem: Problem, listener: PlanTraversalListener) {
  def exec() {
    val plan = ContingencyPlan(problem)

    def doAct(before: Option[BeliefState], state: BeliefState, producer: Option[ActionEdge]) {
      if (problem.goal includes state) {
        listener.onFinish(state, producer)
      } else {
        val successors = plan.plan <<>> state
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
