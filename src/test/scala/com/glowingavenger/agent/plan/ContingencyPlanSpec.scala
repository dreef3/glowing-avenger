package com.glowingavenger.agent.plan

import org.sat4j.scala.Logic._
import com.glowingavenger.agent.problem._
import com.glowingavenger.agent.PlanSpec

class ContingencyPlanSpec extends PlanSpec {
  behavior of "Simple Plan Builder"

  it should "produce a contingent plan for the problem" in new ProblemEnv {
    val planner = ContingencyPlan(problem)
    val planDescr = planner.build()
    planDescr.plan should not be empty
    planDescr.plan.nodes should contain(BeliefState.fromBoolExp('L & 'S  & 'B))
  }
}
