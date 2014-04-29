package com.glowingavenger.dialog

import com.glowingavenger.agent.PlanTraversalListener
import com.glowingavenger.plan.problem.{Question, Action, BeliefState}
import com.glowingavenger.plan.ActionEdge

abstract class AbstractDialogAgent extends PlanTraversalListener {
  override def onStateChange(before: Option[BeliefState], state: BeliefState,
                             producer: Option[ActionEdge], successors: Iterable[ActionEdge]): ActionEdge = {
    
  }

  protected abstract def handleAction(action: Action): Boolean
  
  protected abstract def handleQuestion(question: Question): Boolean
}

class Actions(edges: Iterable[ActionEdge]) {
  val
}
