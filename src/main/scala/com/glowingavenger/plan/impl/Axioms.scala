package com.glowingavenger.plan.impl

import org.sat4j.scala.Logic.{True, BoolExp}
import com.glowingavenger.plan.model.state.BeliefState

trait Axioms {
  /**
   * Produces a function which chooses the axiom from the list for given belief states
   * @param axioms
   * @return axiom boolean expression
   */
  protected def axiom(axioms: Iterable[BoolExp]): (Iterable[BeliefState] => BoolExp)

  protected def axiom(axioms: BoolExp*): (Iterable[BeliefState] => BoolExp) = axiom(axioms)

  protected def chooseAxiom(axioms: Iterable[BoolExp])(states: BeliefState*): BoolExp = axiom(axioms)(states)

  protected def chooseAxiom(axioms: BoolExp*)(states: BeliefState*): BoolExp = axiom(axioms)(states)

  /**
   * Applies the axiom to given states using conjunction
   */
  protected def applyAxiom(clauses: BeliefState*): BeliefState
}

/**
 * Always returns a conjunction of all axioms.
 * A naive approach that would actually work only with a single axiom
 */
trait AxiomConjunction extends Axioms {
  protected def axiom(axioms: Iterable[BoolExp]): (Iterable[BeliefState] => BoolExp) = {
    val axiom = if (axioms.isEmpty) True else (axioms.head /: axioms.tail)(_ & _)
    stateList => axiom
  }
}

/**
 * We have this in a separated trait to simplify unit testing
 */
trait DefaultAxioms extends Axioms with AxiomConjunction with ProblemAware {
  protected val problemAxiom = super.chooseAxiom(problem.domain.axioms)_

  protected def applyAxiom(states: BeliefState*): BeliefState = {
    if (states.isEmpty)
      BeliefState(problemAxiom(Nil))
    else
      BeliefState((problemAxiom(states) /: states)(_ & _.toExpr), states.map(_.predicates.keys).flatten)
  }
}
