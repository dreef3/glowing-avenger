package com.glowingavenger.plan.model

import org.sat4j.scala.Logic.BoolExp

case class Domain(name: String, predicates: List[Symbol], actions: List[LogicAction], axioms: List[BoolExp])

case class UnboundProblem(name: String, domain: String, init: BoolExp, goal: BoolExp)

case class Problem(name: String, domain: Domain, init: BoolExp, goal: BoolExp)

object Problem {
  def apply(problem: UnboundProblem, domain: Domain): Problem = Problem(problem.name, domain, problem.init, problem.goal)
}
