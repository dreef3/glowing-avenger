package com.glowingavenger.plan.impl

import com.glowingavenger.plan.model.action.LogicAction
import com.glowingavenger.plan.model.state.{BeliefState, BeliefStateImplicits}
import BeliefStateImplicits._

trait PlanInitializer {
  protected def initState(): BeliefState
}

trait BackwardSearchInit extends PlanInitializer with Axioms with ProblemAware {
  protected def initState(): BeliefState = {
    val actions = problem.domain.actions collect {
      case a: LogicAction => a
    }
    val backward = new BackwardSearch(problem.domain.predicates, actions,
      Some(chooseAxiom(problem.domain.axioms)(problem.init, problem.goal)))
    backward.search(problem.init, problem.goal) match {
      case Some(model) => BeliefState(model)
      case _ => BeliefState(problem.init)
    }
  }
}
