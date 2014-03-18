package com.glowingavenger.agent.problem

import org.sat4j.scala.Logic.BoolExp
import com.glowingavenger.agent.problem.Action

class Problem (val attrs: List[Symbol], val actions: List[Action], val kb: BoolExp, val init: BeliefState, val goal: BeliefState, val heruistic: (BeliefState, BeliefState) => Int) {
  require(init.attrs.keys.find(!attrs.contains(_)) == None)
  require(goal.attrs.keys.find(!attrs.contains(_)) == None)
  for (action <- actions) {
    require(action.attrs.find(!attrs.contains(_)) == None)
  }
}
