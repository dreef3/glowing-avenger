package com.glowingavenger.agent.problem

import org.sat4j.scala.Logic.BoolExp
import org.sat4j.scala.Logic._

class Problem (val attrs: List[Symbol], val actions: List[Action], val kb: BoolExp, val init: BeliefState, val goal: BeliefState, val heruistic: (BeliefState, BeliefState) => Int) {
  require(init.attrs.keys.find(!attrs.contains(_)) == None)
  require(goal.attrs.keys.find(!attrs.contains(_)) == None)
  for (action <- actions) {
    require(action.attrs.find(!attrs.contains(_)) == None)
  }
}

object Problem {
  def lampProblem() = {
    val attrs = List('S, 'B, 'L, 'C, 'W)
    val kb = 'L iff 'B & 'S
    val actions = List(LogicAction(~'S, 'S & ('L | ~'L), "on"), LogicAction('S, ~'S & ('L | ~'L), "off"), LogicAction(~'B & ~'S, 'B, "ch"),
      LogicAction('B, ~'B, "br"), LogicAction(~'C, 'C, "onc"), LogicAction('C, ~'C, "offc"), LogicAction(~'W, 'W, "op"), LogicAction('W, ~'W, "cl"))
    val init = BeliefState.fromBoolExp(~'L)
    val goal = BeliefState.fromBoolExp('L)
    val heruistic = (i: BeliefState, g: BeliefState) => i.countEqual(g)
    new Problem(attrs, actions, kb, init, goal, heruistic)
  }
}