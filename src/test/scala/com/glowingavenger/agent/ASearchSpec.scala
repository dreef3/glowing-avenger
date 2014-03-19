package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import com.glowingavenger.agent.util.ASearch
import com.glowingavenger.agent.problem.{NoAction, Action, LogicAction, BeliefState}
import org.sat4j.scala.Logic._

class ASearchSpec extends FlatSpec with Matchers {
  behavior of "A* Search"

  trait Env {
    val actions = List(LogicAction(~'S & 'B, 'S & 'L, "on"), LogicAction('S, ~'S, "off"), LogicAction(~'B & ~'S, 'B, "ch"), LogicAction('B, ~'B, "br"))
    val search = new ASearch[(BeliefState, Action)]{
      override def isGoal(node: (BeliefState, Action), goal: (BeliefState, Action)): Boolean = goal._1 includes node._1

      override def hCost(init: (BeliefState, Action), goal: (BeliefState, Action)): Int = 0

      override def cost(node: (BeliefState, Action)): Int = 1

      override def successors(node: (BeliefState, Action)): List[(BeliefState, Action)] = for {
        action <- actions
        result = action.result(node._1)
        if result != node._1
      } yield (result, action)
    }
  }

  it should "find a path to goal" in new Env {
    val result = search.search((new BeliefState(Map('L -> Some(false), 'B -> None, 'S -> Some(true))), NoAction()), (new BeliefState(Map('L -> Some(true), 'B -> Some(true), 'S -> Some(true))), NoAction()))
    result should not be None
  }
}
