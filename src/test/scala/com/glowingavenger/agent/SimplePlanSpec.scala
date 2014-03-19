package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._
import com.glowingavenger.agent.problem._
import scalax.collection.Graph
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LBase._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.WLDiEdge
import scala.collection.immutable.Queue
import com.glowingavenger.agent.util.ASearch
import com.glowingavenger.agent.plan.SimplePlan

class SimplePlanSpec extends FlatSpec with Matchers {
  behavior of "Simple Plan Builder"

  trait ProblemEnv {
    val attrs = List('S, 'B, 'L)
    val kb = 'L iff 'B & 'S
    val actions = List(LogicAction(~'S, 'S, "on"), LogicAction('S, ~'S, "off"), LogicAction(~'B & ~'S, 'B, "ch"), LogicAction('B, ~'B, "br"))
    val init = BeliefState.fromBoolExp(~'L, List('S, 'B))
    val goal = BeliefState.fromBoolExp('L, List('S, 'B))
    val heruistic = (i: BeliefState, g: BeliefState) => i.countEqual(g)
    val problem = new Problem(attrs, actions, kb, init, goal, heruistic)
  }

  it should "produce a contingent plan for the problem" in new ProblemEnv {
    val planner = SimplePlan(problem)
    val planGraph = planner.build()
    planGraph should not be empty
    planGraph.nodes should contain(init)
  }
}
