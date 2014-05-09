package com.glowingavenger.plan.model

import org.sat4j.scala.Logic.BoolExp
import com.glowingavenger.plan.model.action.LogicAction

case class Domain(predicates: List[Symbol], actions: List[LogicAction], axioms: List[BoolExp], name: String = "")

case class UnboundProblem(domain: String, init: BoolExp, goal: BoolExp, name: String = "")

case class Problem(domain: Domain, init: BoolExp, goal: BoolExp, name: String = "")

object Problem {
  def apply(problem: UnboundProblem, domain: Domain): Problem = Problem(domain, problem.init, problem.goal, problem.name)
}
