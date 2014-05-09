package com.glowingavenger.plan.impl

import org.sat4j.scala.Logic.{True, BoolExp}
import com.glowingavenger.plan.model.BeliefState
import com.glowingavenger.plan.model.BeliefStateImplicits._

trait Axioms {
  protected def axiom(axioms: Iterable[BoolExp]): (Iterable[BeliefState] => BeliefState)

  protected def applyAxiom(clauses: BeliefState*): BeliefState
}

/**
 * Naive approach that would actually work only with a single axiom
 */
trait AxiomConjunction extends Axioms {
  protected def axiom(axioms: Iterable[BoolExp]): (Iterable[BeliefState] => BeliefState) = {
    val axiom = if (axioms.isEmpty) True else (axioms.head /: axioms.tail)(_ & _)
    stateList => BeliefState((axiom /: stateList)(_ & _.asBoolExp), stateList.map(_.attrs.keys).flatten)
  }
}

/**
 * We have this in a separated trait to simplify unit testing
 */
trait DefaultAxioms extends Axioms with AxiomConjunction with ProblemAware {
  protected def applyAxiom(clauses: BeliefState*): BeliefState = axiom(problem.domain.axioms)(clauses)
}
