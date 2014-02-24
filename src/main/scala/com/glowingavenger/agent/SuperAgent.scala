package com.glowingavenger.agent

import org.sat4j.scala.Logic._

object Prop {
  def unknown(label: String) = new Unknown(label)
  def known(label: String, value: Boolean) = new Known(label, value, Symbol(label))
  def known(unknown: Unknown, value: Boolean) = new Known(unknown.label, value, Symbol(unknown.label))
}

abstract class Prop(val label: String) {
  override def toString = label
}
case class Known(override val label: String, value: Boolean, symbol: Symbol) extends Prop(label)
case class Unknown(override val label: String) extends Prop(label)

object Action {
  def apply(label: String, precond: BoolExp, effect: BoolExp) = new Action(label, precond, effect)
}
class Action(val label: String, val precond: BoolExp, val effect: BoolExp) {
  override def toString = label
}

class Environment(val actions: List[Action], val props: List[Prop], val axioms: BoolExp)

class Agent(val env: Environment, val init: BoolExp, val goal: BoolExp) {
  def findGrantedSequence(seq: List[Action] = Nil, goal: BoolExp = goal): List[Action] = {
    for (
      action <- env.actions
      if isSat(env.axioms & action.effect & goal)._1
    ) if (isSat(action.precond & env.axioms)._1) action :: seq
    else findGrantedSequence(action :: seq, action.precond)
    Nil
  }
}