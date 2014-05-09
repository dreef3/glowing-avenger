package com.glowingavenger.plan.model.action

import org.sat4j.scala.Logic._
import com.glowingavenger.plan.util.Model
import com.glowingavenger.plan.model.BeliefState

class LogicAction(val precond: BoolExp, val effect: BoolExp, actionName: String) extends Action {
  require(isSat(precond)._1)

  private lazy val precondModels = Model.retrieveModelsOrThrow(precond)
  private lazy val precondState = new BeliefState(precondModels)
  private lazy val effectModels = Model.retrieveModelsOrThrow(effect)

  override def result(state: BeliefState): BeliefState = if (applicableIn(state)) new BeliefState(state.attrs ++ effectModels) else state
  override def applicableIn(state: BeliefState): Boolean = precondState includes state

  override val attrs: List[Symbol] = (effectModels ++ precondModels).keySet.toList
  override val name: String = actionName


  override def toString: String = s"LogicAction(${PrettyPrint(precond)}, ${PrettyPrint(effect)}, $name)"

  override val cost: Int = 1


  def canEqual(other: Any): Boolean = other.isInstanceOf[LogicAction]

  override def equals(other: Any): Boolean = other match {
    case that: LogicAction =>
      (that canEqual this) &&
        attrs == that.attrs &&
        name == that.name &&
        cost == that.cost &&
        precond == that.precond &&
        effect == that.effect
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(attrs, name, cost, precond, effect)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object LogicAction {
  def apply(precond: BoolExp, effect: BoolExp, name: String = "") = new LogicAction(precond, effect, name)
}
