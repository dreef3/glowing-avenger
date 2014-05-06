package com.glowingavenger.plan

import com.glowingavenger.plan.model._
import org.sat4j.scala.Logic._
import org.scalatest.{FlatSpec, Matchers}
import com.glowingavenger.plan.importexport.PDDL

abstract class PlanSpec extends FlatSpec with Matchers {
  trait ProblemEnv {
    val problem = PDDL.importProblem("src/main/resources/Bulb.pddl")
    val actions = problem.domain.actions
    val init = problem.init
    val goal = problem.goal
  }
}
