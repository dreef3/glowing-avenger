package com.glowingavenger.plan

import com.glowingavenger.plan.model.{BeliefState, Domain, Problem}
import com.glowingavenger.plan.model.action.Question
import org.sat4j.scala.Logic._
import com.glowingavenger.plan.util.ReachGraph._
import com.glowingavenger.plan.model.BeliefStateImplicits._

class ContingencyPlanSpec extends PlanSpec {
  behavior of "Contingency Planner"

  it should "produce two edges for the question" in {
    val problem = new Problem(new Domain(List('A), Nil, Nil), 'A.?, 'A)
    val plan = ContingencyPlan(problem)
    plan.init shouldBe new BeliefState(Map('A -> None))
    plan.plan <<>> plan.init shouldBe Set(ActionEdge('A?, toProp('A), Question('A)), ActionEdge('A?, ~'A, Question('A)))
  }

  it should "produce a contingent plan for the problem" in new ProblemEnv {
    val planDescr = ContingencyPlan(problem)
    planDescr.plan.vertexSet() should not be empty
    planDescr.plan.edgeSet() should not be empty
    planDescr.plan.vertexSet() should contain(BeliefState.apply('L & 'S  & 'B))
  }
}
