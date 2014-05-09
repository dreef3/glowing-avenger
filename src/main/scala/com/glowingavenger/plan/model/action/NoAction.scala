package com.glowingavenger.plan.model.action

import com.glowingavenger.plan.model.BeliefState

/**
 * Created by andrey on 09.05.14.
 */
case class NoAction() extends Action {
  override def result(state: BeliefState): BeliefState = state

  override def applicableIn(state: BeliefState): Boolean = false

  override def attrs: List[Symbol] = Nil

  override val name: String = NoAction.getClass.getSimpleName

  override val cost: Int = 0
}
