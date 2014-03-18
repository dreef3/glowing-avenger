package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._
import com.glowingavenger.agent.problem.{BeliefState, Problem, LogicAction}
import com.glowingavenger.agent.plan.ContingentPlan

class ContingentPlanSpec extends FlatSpec with Matchers {
  behavior of "Contingent Plan"

  trait Env {
    val attrs = List('S, 'B, 'L)
    val kb = 'L iff 'B & 'S
    val actions = List(LogicAction(~'S, 'S, "on"), LogicAction('S, ~'S, "off"), LogicAction(~'B & ~'S, 'B, "ch"), LogicAction('B, ~'B, "br"))
    val init = BeliefState.fromBoolExp(~'L, List('S, 'B))
    val goal = BeliefState.fromBoolExp('L, List('S, 'B))
    val heruistic = (i: BeliefState, g: BeliefState) => i.countEqual(g)
    val problem = new Problem(attrs, actions, kb, init, goal, heruistic)
    val plan = new ContingentPlan(problem)
  }

  it should "produce a valid plan for the problem" in new Env {
    val created = plan.create()
    created should not be None
  }
}
