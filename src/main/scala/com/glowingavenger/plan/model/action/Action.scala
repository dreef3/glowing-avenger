package com.glowingavenger.plan.model.action

import com.glowingavenger.plan.model.BeliefState

/**
 * Created by andrey on 09.05.14.
 */
trait Action {
  val name: String

  def cost: Int

  def attrs: List[Symbol]

  def applicableIn(state: BeliefState): Boolean

  def result(state: BeliefState): BeliefState

  override def toString: String = name
}
