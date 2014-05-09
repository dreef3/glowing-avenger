package com.glowingavenger.plan.model.action

import com.glowingavenger.plan.model.{Answer, BeliefState}

/**
 * Created by andrey on 09.05.14.
 */
case class Question(attr: Symbol) extends Action {
  override def result(state: BeliefState): BeliefState = {
    if (applicableIn(state))
      Answer(state.attrs, attr)
    else state
  }

  override def applicableIn(state: BeliefState): Boolean = {
    state.attrs.contains(attr) && state.attrs(attr) == None
  }

  override def attrs: List[Symbol] = attr :: Nil

  override val name: String = attr.name

  override val cost: Int = 0
}
