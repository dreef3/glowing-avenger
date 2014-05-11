package com.glowingavenger.plan.model.action

import com.glowingavenger.plan.model.state.BeliefState

trait Action {
  val name: String

  def cost: Int

  def attrs: List[Symbol]

  def applicableIn(state: BeliefState): Boolean

  def result(state: BeliefState): BeliefState

  override def toString: String = name
}
