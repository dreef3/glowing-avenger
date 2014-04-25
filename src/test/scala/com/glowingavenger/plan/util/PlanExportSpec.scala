package com.glowingavenger.plan.util

import com.glowingavenger.plan.{ContingencyPlan, PlanSpec}

class PlanExportSpec extends PlanSpec {
  behavior of "Exporter to DOT"

  it should "export a plan to DOT language format" in new ProblemEnv {
    val planner = ContingencyPlan(problem)
    val descr = planner.build()
    val dot = PlanExport.export(descr.plan)
    dot should not be empty
  }
}
