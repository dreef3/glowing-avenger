package com.glowingavenger.plan

import org.scalatest.{FlatSpec, Matchers}
import com.glowingavenger.plan.importexport.PDDL
import scala.io.Source

abstract class PlanSpec extends FlatSpec with Matchers {
  trait ProblemEnv {
    val problem = PDDL.importProblem(Source.fromFile("src/main/resources/Bulb.pddl").mkString)
    val actions = problem.domain.actions
    val init = problem.init
    val goal = problem.goal
  }
}
