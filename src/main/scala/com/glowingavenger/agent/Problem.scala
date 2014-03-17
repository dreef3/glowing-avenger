package com.glowingavenger.agent

import org.sat4j.scala.Logic.BoolExp

class Problem (val attrs: List[Symbol], val actions: List[Action], val kb: BoolExp, val init: BeliefState, val goal: BeliefState) {
  require(init.attrs.keys.find(!attrs.contains(_)) == None)
  require(goal.attrs.keys.find(!attrs.contains(_)) == None)
  for (action <- actions) {
    require(action.attrs.find(!attrs.contains(_)) == None)
  }
}
