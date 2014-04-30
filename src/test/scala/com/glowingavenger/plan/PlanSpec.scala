package com.glowingavenger.plan

import com.glowingavenger.plan.problem._
import org.sat4j.scala.Logic._
import org.scalatest.{FlatSpec, Matchers}

abstract class PlanSpec extends FlatSpec with Matchers {
  trait ProblemEnv {
    val attrs = List('S, 'B, 'L, 'C, 'W)
    val kb = 'L iff 'B & 'S
    val actions = List(LogicAction(~'S, 'S & 'L.?, "on"), LogicAction('S.?, ~'S & 'L.?, "off"), LogicAction('B.? & ~'S, 'B, "ch"),
      LogicAction('B, ~'B, "br"), LogicAction(~'C, 'C, "onc"), LogicAction('C, ~'C, "offc"), LogicAction(~'W, 'W, "op"), LogicAction('W, ~'W, "cl"))
    val init = BeliefState.fromBoolExp(~'L)
    val goal = BeliefState.fromBoolExp('L)
    val heruistic = (i: BeliefState, g: BeliefState) => i.countEqual(g)
    val problem = new PDDLProblem(attrs, actions, kb, init, goal, heruistic)
  }
}
