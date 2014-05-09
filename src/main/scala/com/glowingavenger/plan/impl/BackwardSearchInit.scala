package com.glowingavenger.plan.impl

import com.glowingavenger.plan.model.BeliefState
import com.glowingavenger.plan.model.action.LogicAction
import com.glowingavenger.plan.model.BeliefStateImplicits._

trait PlanInitializer {
  protected def initState(): BeliefState
}

trait BackwardSearchInit extends PlanInitializer with Axioms with ProblemAware {
  protected def initState(): BeliefState = {
    val actions = problem.domain.actions collect {
      case a: LogicAction => a
    }
    val backward = new BackwardSearch(problem.domain.predicates, actions,
      Some(applyAxiom(problem.init, problem.goal).asBoolExp))
    backward.search(problem.init, problem.goal) match {
      case Some(model) => BeliefState(model)
      case _ => BeliefState.apply(problem.init)
    }
  }
}
