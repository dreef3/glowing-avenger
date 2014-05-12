package com.glowingavenger.plan

import com.glowingavenger.plan.impl._
import com.glowingavenger.plan.model.Problem

class ContingencyPlan(override val problem: Problem) extends AbstractPlanner
with BackwardSearchInit with DefaultAxioms with CompoundPathSuccessors with QuestionSuccessors {

}

object ContingencyPlan {
  def apply(problem: Problem) = new ContingencyPlan(problem).build()
}
