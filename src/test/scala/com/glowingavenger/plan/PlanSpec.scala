package com.glowingavenger.plan

import org.scalatest.{FlatSpec, Matchers}
import com.glowingavenger.plan.importexport.PDDL
import scala.io.Source
import com.glowingavenger.plan.model.state.BeliefState
import org.sat4j.scala.Logic._

abstract class PlanSpec extends FlatSpec with Matchers {
  trait ProblemEnv {
    val problem = PDDL.importProblem(Source.fromFile("src/main/resources/Bulb.pddl").mkString)
    val actions = problem.domain.actions
    val init = problem.init
    val goal = problem.goal
    val initState = BeliefState(~'L & 'B.? & 'S.?)
    val goalState = BeliefState('L & 'B & 'S)
  }
}
