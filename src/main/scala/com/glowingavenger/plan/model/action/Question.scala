package com.glowingavenger.plan.model.action

import com.glowingavenger.plan.model.state.{BeliefState, Answer}

case class Question(attr: Symbol) extends Action {
  override def result(state: BeliefState): BeliefState = {
    if (applicableIn(state))
      Answer(state.predicates, attr)
    else state
  }

  override def applicableIn(state: BeliefState): Boolean = {
    state.predicates.contains(attr) && state.predicates(attr) == None
  }

  override def attrs: List[Symbol] = attr :: Nil

  override val name: String = attr.name

  override val cost: Int = 0
}
