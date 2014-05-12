package com.glowingavenger.plan.impl

import com.glowingavenger.plan.ActionEdge
import com.glowingavenger.plan.model.action.Question
import com.glowingavenger.plan.model.state.{Answer, BeliefState}

trait Successors {
  protected def successors(state: BeliefState): List[ActionEdge] = Nil
}

trait QuestionSuccessors extends Successors with Axioms {
  override protected def successors(state: BeliefState): List[ActionEdge] = {
    super.successors(state) ::: state.unknown.map(Question).map {
      question =>
        question.result(state) match {
          case a: Answer =>
            ActionEdge(state, applyAxiom(a.yes), question) ::
              ActionEdge(state, applyAxiom(a.no), question) :: Nil
          case _ => Nil
        }
    }.flatten
  }
}

