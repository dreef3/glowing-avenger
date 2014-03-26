package com.glowingavenger.agent.util

import com.glowingavenger.agent.PlanSpec
import com.glowingavenger.agent.plan.ContingencyPlan

class PlanExportSpec extends PlanSpec {
  behavior of "Exporter to DOT"

  it should "export a plan to DOT language format" in new ProblemEnv {
    val planner = ContingencyPlan(problem)
    val descr = planner.build()
    val dot = PlanExport.export(descr.plan)
    dot should not be empty
  }
}
