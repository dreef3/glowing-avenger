package com.glowingavenger.plan.problem

import org.sat4j.scala.Logic._
import com.glowingavenger.plan.util.Model

class LogicAction(val precond: BoolExp, val effect: BoolExp, actionName: String) extends Action {
  require(isSat(precond)._1)

  private lazy val precondModels = Model.retrieveModelsOrThrow(precond)
  private lazy val precondState = new BeliefState(precondModels)
  private lazy val effectModels = Model.retrieveModelsOrThrow(effect)

  override def result(state: BeliefState): BeliefState = if (applicableIn(state)) new BeliefState(state.attrs ++ effectModels) else state
  override def applicableIn(state: BeliefState): Boolean = precondState includes state

  override val attrs: List[Symbol] = (effectModels ++ precondModels).keySet.toList
  override val name: String = actionName

  override val cost: Int = 1
}

object LogicAction {
  def apply(precond: BoolExp, effect: BoolExp, name: String = "") = new LogicAction(precond, effect, name)
}
