package com.glowingavenger.agent.problem

import org.sat4j.scala.Logic._
import com.glowingavenger.agent.util.Model
import com.glowingavenger.agent.problem.BeliefState

class LogicAction(val precond: BoolExp, val effect: BoolExp, actionName: String) extends Action {
  require(isSat(precond)._1)

  private val precondModels = Model.retrieveModelsOrThrow(precond)
  private val effectModels = Model.retrieveModelsOrThrow(effect)

  override def result(state: BeliefState): BeliefState = if (applicableIn(state)) new BeliefState(state.attrs ++ effectModels) else state
  override def applicableIn(state: BeliefState): Boolean = isSat(state.asBoolExp & precond)._1

  override val attrs: List[Symbol] = (effectModels ++ precondModels).keySet.toList
  override val name: String = actionName

  override val cost: Int = 1
}

object LogicAction {
  def apply(precond: BoolExp, effect: BoolExp, name: String = "") = new LogicAction(precond, effect, name)
}