package com.glowingavenger.plan.impl

import com.glowingavenger.plan.ActionEdge
import com.glowingavenger.plan.model.action.Question
import com.glowingavenger.plan.model.state.{Answer, BeliefState}

trait Successors {
  def successors(state: BeliefState): (List[ActionEdge], List[BeliefState]) = (Nil, Nil)
}

trait QuestionSuccessors extends Successors with Axioms {
  override def successors(state: BeliefState): (List[ActionEdge], List[BeliefState]) = {
    val (edges, states) = super.successors(state)
    val questions = state.unknown.map(Question).map {
      question =>
        question.result(state) match {
          case a: Answer =>
            ActionEdge(state, applyAxiom(a.yes), question) ::
              ActionEdge(state, applyAxiom(a.no), question) :: Nil
          case _ => Nil
        }
    }.flatten
    (edges ::: questions, states ::: questions.map(_.to))
  }
}
