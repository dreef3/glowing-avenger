package com.glowingavenger.agent.plan

import org.sat4j.scala.Logic._
import com.glowingavenger.agent.problem._
import com.glowingavenger.agent.PlanSpec

class ContingencyPlanSpec extends PlanSpec {
  behavior of "Simple Plan Builder"

  it should "produce a contingent plan for the problem" in new ProblemEnv {
    val planner = ContingencyPlan(problem)
    val planGraph = planner.build()
    planGraph should not be empty
    planGraph.nodes should contain(BeliefState.fromBoolExp('L & 'S  & 'B))
  }
}
