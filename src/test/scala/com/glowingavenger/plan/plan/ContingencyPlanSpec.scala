package com.glowingavenger.plan.plan

import org.sat4j.scala.Logic._
import com.glowingavenger.plan.model._
import com.glowingavenger.plan.{ContingencyPlan, PlanSpec}

class ContingencyPlanSpec extends PlanSpec {
  behavior of "Simple Plan Builder"

  it should "produce a contingent plan for the problem" in new ProblemEnv {
    val planner = ContingencyPlan(problem)
    val planDescr = planner.build()
    planDescr.plan.vertexSet() should not be empty
    planDescr.plan.edgeSet() should not be empty
    planDescr.plan.vertexSet() should contain(BeliefState.fromBoolExp('L & 'S  & 'B))
  }
}
